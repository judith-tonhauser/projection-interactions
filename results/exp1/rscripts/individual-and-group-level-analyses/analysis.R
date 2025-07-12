# Exp1: interaction of prior beliefs, at-issueness and projection
# for 20 clause-embedding predicates
# predict individual certainty ratings from mean at-issueness and mean prior belief ratings
# full data analyses only (no by-block analyses)
# analysis.R

# load required packages
library(tidyverse)
library(optimx)
library(xtable)
library(texreg) #to output models in latex 
library(forcats)
library(brms)
library(tidybayes)
library(emmeans)
library(brmsmargins)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# prepare the data -----

# read the data 
d <- read_csv("../../data/d.csv")
nrow(d) #10100
summary(d)
names(d)

# import Exp 2 data for two reasons
# 1) it has the mean prior ratings 
# 2) to calculate mean at-issueness ratings (over content/fact combinations)
d2 <- read_csv("../../../exp2/data/d.csv")
nrow(d2) #10000
summary(d2)
names(d2)

# create subsets with the same columns to bind the two datasets and calculate mean at-issueness ratings
tmp_d <- d %>%
  select(prior_type,content,short_trigger,ai)
nrow(tmp_d) #10100

tmp_d2 <- d2 %>%
  select(prior_type,content,short_trigger,ai)
nrow(tmp_d2) #10000

tmp_d = rbind(tmp_d,tmp_d2)
nrow(tmp_d) #20100
tmp_d

# calculate by-content/fact at-issueness means for each predicate
aiMeans = tmp_d %>%
  group_by(short_trigger,content,prior_type) %>%
  mutate(aiMean=mean(ai)) %>%
  ungroup() %>%
  select(-c(ai)) %>%
  distinct()
nrow(aiMeans) #800 = 20 predicates x 40 content/fact combinations

# add the aiMeans to the Exp 1 data
d = left_join(d,aiMeans)
nrow(d) #10100

# check: how many aiMeans per predicate?
tmp = d %>% 
  distinct(short_trigger,prior_type,content,aiMean)
nrow(tmp) #800

# [1] mean at-issueness (and individual prior ratings) ----

# change predicate names so that loops can work
d = d %>%
  mutate(short_trigger = recode(short_trigger,"be annoyed" = "be.annoyed","be right" = "be.right"))
table(d$short_trigger)

predicates = unique(as.character(d$short_trigger))
predicates

for (p in predicates) {
  nRow = 505
  nRowMinusOne = 505-1
  write_csv(assign(paste("data.", p, sep=""), d %>% filter(short_trigger == p) %>% 
                     #group_by(content) %>% mutate(aiMean=mean(ai)) %>%
                     select(c(short_trigger,projective,aiMean,prior,content,workerid)) %>%
                     #mutate(caiMean = aiMean-mean(aiMean), cprior=prior-mean(prior)) %>%
                     mutate(betaProjective = (projective*nRowMinusOne + .5)/nRow))
            ,
            file=paste("models/mean-ai-ind-prior/RQI-IIIa/data.",p,".csv",sep=""))
}

# center the variables in separate loop (doesn't work above for some reason)
for (p in predicates) {
  data = read_csv(paste("models/mean-ai-ind-prior/RQI-IIIa/data.", p, ".csv",sep=""))
  write_csv(assign(paste("data.", p, sep=""), data %>% mutate(caiMean = aiMean-mean(aiMean), cprior=prior-mean(prior))),
            file=paste("models/mean-ai-ind-prior/RQI-IIIa/data.",p,".csv",sep=""))
}

# check the variables
tmp = read_csv("models/mean-ai-ind-prior/RQI-IIIa/data.acknowledge.csv")
summary(tmp$aiMean)
summary(tmp$caiMean) 
summary(tmp$cprior)
summary(tmp$betaProjective)
nrow(tmp) #505

### questions I, II, III.a ----

# proj_ind ~ caiMean*cprior + (1+caiMean*cprior|participant) + (1|content) 
betamodel = bf(betaProjective ~ caiMean*cprior + (1+caiMean*cprior|workerid) + (1|content),
               phi ~ caiMean*cprior + (1+caiMean*cprior|workerid) + (1|content), # beta distribution's precision 
               family = Beta())

# fit the model for each predicate
for (p in predicates) {
  data.here = read_csv(paste("models/mean-ai-ind-prior/RQI-IIIa/data.", p, ".csv",sep=""))
  saveRDS(assign(paste("model.", p, ".rds", sep=""), brm(formula = betamodel,
                                                         family=Beta(),
                                                         data=data.here, 
                                                         cores = 4, iter = 4500, warmup = 900,
                                                         control = list(adapt_delta = .99,max_treedepth=15))),
          file=paste("models/mean-ai-ind-prior/RQI-IIIa/model.",p, ".rds", sep=""))
}

# check a model
tmp <- readRDS("models/mean-ai-ind-prior/RQI-IIIa/model.know.rds")
summary(tmp)

# hypotheses
hypotheses.fullData = data.frame(block = character(), predicate = character(), hypothesis = character(), estimate = numeric(), post.prob = numeric(), bf = numeric())
hypotheses.fullData

hypotheses = c("cprior > 0", "caiMean > 0", "caiMean:cprior > 0")
hypotheses

for (p in predicates) {
  for (h in hypotheses) {
    model = readRDS(paste("models/mean-ai-ind-prior/RQI-IIIa/model.",p,".rds",sep=""))
    estimate = hypothesis(model,h)$hypothesis$Estimate
    post.prob = hypothesis(model,h)$hypothesis$Post.Prob
    bf = hypothesis(model,h)$hypothesis$Evid.Ratio
    hypotheses.fullData = hypotheses.fullData %>% 
      add_row(block = "fullData", predicate = p, hypothesis = h, estimate = estimate, post.prob = post.prob, bf = bf)
  }
}

hypotheses.fullData

# save
write_csv(hypotheses.fullData,file="models/mean-ai-ind-prior/RQI-IIIa/hypotheses.fullData.csv")

# now test the < 0 hypotheses (to identify when there is a negative effect)
hypotheses.fullDataNEG = data.frame(block = character(), predicate = character(), hypothesisNEG = character(), estimateNEG = numeric(), post.probNEG = numeric(), bfNEG = numeric())
hypotheses.fullDataNEG

hypotheses = c("cprior < 0", "caiMean < 0", "caiMean:cprior < 0")
hypotheses

for (p in predicates) {
  for (h in hypotheses) {
    model = readRDS(paste("models/mean-ai-ind-prior/RQI-IIIa/model.",p,".rds",sep=""))
    estimate = hypothesis(model,h)$hypothesis$Estimate
    post.prob = hypothesis(model,h)$hypothesis$Post.Prob
    bf = hypothesis(model,h)$hypothesis$Evid.Ratio
    hypotheses.fullDataNEG = hypotheses.fullDataNEG %>% 
      add_row(block = "fullData", predicate = p, hypothesisNEG = h, estimateNEG = estimate, post.probNEG = post.prob, bfNEG = bf)
  }
}

hypotheses.fullDataNEG

# save
write_csv(hypotheses.fullDataNEG,file="models/mean-ai-ind-prior/RQI-IIIa/hypotheses.fullDataNEG.csv")

### question III.b ----

# this would involve predicting aiMean from cprior, which doesn't make sense conceptually 

### create the table ----

#load the data
full = read_csv(file="models/mean-ai-ind-prior/RQI-IIIa/hypotheses.fullData.csv")
fullNEG = read_csv(file="models/mean-ai-ind-prior/RQI-IIIa/hypotheses.fullDataNEG.csv")

# bind the data
tableData = full
nrow(tableData)
d2 = fullNEG
nrow(d2)

# create a new hypothesis column to bind the data to d
d2 = d2 %>%
  mutate(hypothesis = case_when(hypothesisNEG == "cprior < 0" ~ "cprior > 0",
                                hypothesisNEG == "caiMean < 0" ~ "caiMean > 0",
                                hypothesisNEG == "caiMean:cprior < 0" ~ "caiMean:cprior > 0",
                                TRUE ~ "ERROR")) %>%
  select(-c(hypothesisNEG))

tableData = left_join(tableData,d2,by=c("block","predicate","hypothesis"))
tableData
#view(d)

# when the estimate is < 0, it is possible that there is no effect or a negative effect
# create column "bfNEW" that is the original bf when that estimate > 0
# but that is the bf of the reverse hypothesis when the estimate is <= 0

tableData = tableData %>%
  mutate(bfNEW = case_when(estimate > 0 ~ bf,
                           estimate <= 0 ~ bfNEG,
                           TRUE ~ 555)) %>%
  mutate(bfNEW = round(bfNEW, digits = 0))

# plot the bayes factors
# 100 = "very strong support" according to S. N. Goodman (1999)
# there is 100 times more evidence for H1 than H0
ggplot(tableData[tableData$bfNEW < 500,], aes(x=bfNEW)) +
  geom_histogram() +
  facet_wrap(. ~ hypothesis)
# the shapes of cai and cprior differ very much from that of cprior:cai


# now create a column that color codes the result based on the estimate and bfNEW columns
# based on Goodmann 1999
# bf 21+ "very strong to extreme" (color 1)
# bf 11-20 "strong" (color2)
# bf 2-10 "weak to moderate" (color3)
# bf 1 "no evidence" (white)

tableData = tableData %>%
  mutate(result = case_when(estimate > 0 & bfNEW >= 100  ~ "\\cellcolor{red1}",
                            estimate > 0 & bfNEW > 20 & bfNEW < 100 ~ "\\cellcolor{red1}",
                            estimate > 0 & bfNEW > 10 & bfNEW <= 20 ~ "\\cellcolor{red2}",
                            estimate > 0 & bfNEW > 5 & bfNEW <= 10 ~ "\\cellcolor{red4}",
                            estimate > 0 & bfNEW > 1 & bfNEW <= 5 ~ "\\cellcolor{red4}", 
                            estimate >= 0 & bfNEW == 1 ~ "\\cellcolor{white}", 
                            estimate < 0 & bfNEW >= 100  ~ "\\cellcolor{blue1}",
                            estimate < 0 & bfNEW > 20 & bfNEW < 100 ~ "\\cellcolor{blue1}",
                            estimate < 0 & bfNEW > 10 & bfNEW <= 20 ~ "\\cellcolor{blue2}",
                            estimate < 0 & bfNEW > 5 & bfNEW <= 10 ~ "\\cellcolor{blue4}",
                            estimate < 0 & bfNEW > 1 & bfNEW <= 5 ~ "\\cellcolor{blue4}", 
                            estimate < 0 & bfNEW == 1 ~ "\\cellcolor{white}",
                            TRUE ~ "ERROR"))

#view(tableData)

# to sort (rotated) predicates by projection mean
d <- read_csv("../../data/d.csv")
nrow(d) #10100

# change predicate names so that loops can work
d = d %>%
  mutate(short_trigger = recode(short_trigger,"be annoyed" = "be.annoyed","be right" = "be.right"))
table(d$short_trigger)

tmp = d %>%
  rename("predicate" = short_trigger) %>%
  mutate(predicate = paste0("\\rot{", predicate, "}")) %>%
  group_by(predicate) %>%
  summarize(Mean_proj = mean(projective)) %>%
  mutate(predicate = fct_reorder(as.factor(predicate),Mean_proj)) 
tmp
levels(tmp$predicate)

# now get rid of all the unneeded columns and create a new hypothesis column that doesn't reflect the order (> 0 or < 0)
# and a new block column
tableData = tableData %>%
  select(c("hypothesis","predicate","result")) %>%
  mutate(hypothesis = case_when(hypothesis == "cprior > 0" ~ "prior on proj",
                                hypothesis == "caiMean > 0" ~ "mean nai on proj",
                                hypothesis == "caiMean:cprior > 0" ~ "prior:mean nai on proj")) %>%
  #mutate(block = case_when(block == "fullData" ~ "full",
                           #block == "_projai." ~ "proj/ai",
                           #block == "_aiproj." ~ "ai/proj")) %>%
  mutate(predicate = paste0("\\rot{", predicate, "}")) %>%
  mutate(predicate = fct_relevel(predicate,rev(levels(tmp$predicate))))
tableData

# get the data into the desired shape (predicates as columns, rows: full data first, then proj/ai, then ai/proj)

tableData = tableData %>% 
  spread(predicate,result)

#x <- c("full", "proj/ai", "ai/proj")
y <- c("prior on proj", "mean nai on proj", "prior:mean nai on proj")

tableData = tableData %>%
  #mutate(block =  factor(block, levels = x)) %>%
  #arrange(block) %>%
  mutate(hypothesis = factor(hypothesis, levels = y)) %>%
  arrange(hypothesis)
tableData

# make factive predicates orange
tableData = tableData %>%
  rename("\\rot{\\color{orange}be annoyed\\color{black}}" = "\\rot{be.annoyed}",
         "\\rot{\\color{orange}discover\\color{black}}" = "\\rot{discover}",
         "\\rot{\\color{orange}know\\color{black}}" = "\\rot{know}",
         "\\rot{\\color{orange}see\\color{black}}" = "\\rot{see}",
         "\\rot{\\color{orange}reveal\\color{black}}" = "\\rot{reveal}",
         "\\rot{be right}" = "\\rot{be.right}")

tableData = tableData %>%
  #rename("{\\bf Data}" = "block") %>%
  rename("{\\bf Effect}" = "hypothesis")
tableData

# save tableData
saveRDS(tableData, "models/RQIIIa/2-mean-ai-ind-prior/tableDataPROJ")

# create new first column (Effect)
effect <- c("{\\bf (I):} prior on", "\\hspace*{.7cm}projection", "", 
            "{\\bf (II):} not-at-issue on", "\\hspace*{.7cm} projection", "",
            "{\\bf (IIIa):} prior:not-at-issue", "\\hspace*{1.1cm} on projection", "")
            #"{\\bf (IIIb):} prior on", "\\hspace*{1.1cm} not-at-issueness", "")


# remove original effect column, add new one, give it a good header
tableData = tableData %>%
  select(-c("{\\bf Effect}")) 
tableData = cbind(effect,tableData) %>%
  rename("{\\bf Effect}" = "effect")

# create Latex code for Table 1a ----

t1 = print(xtable(tableData),
           include.rownames=FALSE,
           include.colnames=TRUE,
           #tabular.environment="longtable",
           floating=FALSE,
           latex.environments=NULL,
           booktabs=FALSE,
           sanitize.text.function = function(x){x},
           #hline.after = c(0,0,3,3,6,6,9,9,12)
           #hline.after = c(0,0,3,6,9,9,12,15,18,18,21,21,24)
)

write(t1, "models/mean-ai-ind-prior/RQI-IIIa/t1")

# output of the models for the supplement ----

# there are 60 models for the analysis of questions I, II, and III.a
# for each of the 20 predicates, full data, proj/ai, ai/proj
# and another 60 models for the analysis of question III.b

# read the data (to create list of predicates)
d <- read_csv("../data/d.csv")
nrow(d) #10100
# change predicate names so that loops can work
d = d %>%
  mutate(short_trigger = recode(short_trigger,"be annoyed" = "be.annoyed","be right" = "be.right"))
table(d$short_trigger)

# define the predicates
predicates = unique(as.character(d$short_trigger))
predicates

# main analysis models for questions I, II, III.a
for (p in predicates) {
  model = readRDS(paste("../models/projection-main/model.",p,".rds",sep=""))
  tmp_table = as.data.frame(summary(model)$fixed)
  tmp_table = tmp_table %>% 
    rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
    select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
    mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
    mutate("95% CrI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
    select(-c(lower,upper)) %>%
    rename("Expected mean" = "Estimate")
  caption = paste("Full data, ",p,sep="")
  write(print(xtable(tmp_table,caption = caption),
              include.rownames=TRUE,
              include.colnames=TRUE,
              tabular.environment="tabular",
              floating=FALSE,
              latex.environments=NULL,
              booktabs=FALSE),
        file=paste("../models/latex-tables/supplement/models-for-questions-I-II-IIIa/full.",p,".tex", sep=""))
}

# by-block analysis models for questions I, II, III.a
block = c("projai.", "aiproj.")
block

for (p in predicates) {
  for (b in block) {
    model = readRDS(paste("../models/projection-byBlock/model_",b,p,".rds",sep=""))
    tmp_table = as.data.frame(summary(model)$fixed)
    tmp_table = tmp_table %>% 
      rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
      select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
      mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
      mutate("95% CrI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
      select(-c(lower,upper)) %>%
      rename("Expected mean" = "Estimate")
    caption = paste(b,"data, ",p,sep="")
    write(print(xtable(tmp_table,caption = caption),
                include.rownames=TRUE,
                include.colnames=TRUE,
                #tabular.environment="longtable",
                floating=FALSE,
                latex.environments=NULL,
                booktabs=FALSE),
          file=paste("../models/latex-tables/supplement/models-for-questions-I-II-IIIa/byBlock.", b,p,".tex", sep=""))
  }
}

# main analysis models for question III.b
for (p in predicates) {
  model = readRDS(paste("../models/ai-prior-main/model.",p,".rds",sep=""))
  tmp_table = as.data.frame(summary(model)$fixed)
  tmp_table = tmp_table %>% 
    rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
    select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
    mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
    mutate("95% CrI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
    select(-c(lower,upper)) %>%
    rename("Expected mean" = "Estimate")
  caption = paste("Full data, ",p,sep="")
  write(print(xtable(tmp_table,caption = caption),
              include.rownames=TRUE,
              include.colnames=TRUE,
              tabular.environment="tabular",
              floating=FALSE,
              latex.environments=NULL,
              booktabs=FALSE),
        file=paste("../models/latex-tables/supplement/models-for-question-IIIb/full.",p,".tex", sep=""))
}

# by-block analysis models for question III.b
block = c("projai.", "aiproj.")
block

for (p in predicates) {
  for (b in block) {
    model = readRDS(paste("../models/ai-prior-byBlock/model_",b,p,".rds",sep=""))
    tmp_table = as.data.frame(summary(model)$fixed)
    tmp_table = tmp_table %>% 
      rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
      select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
      mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
      mutate("95% CrI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
      select(-c(lower,upper)) %>%
      rename("Expected mean" = "Estimate")
    caption = paste(b,"data, ",p,sep="")
    write(print(xtable(tmp_table,caption = caption),
                include.rownames=TRUE,
                include.colnames=TRUE,
                #tabular.environment="longtable",
                floating=FALSE,
                latex.environments=NULL,
                booktabs=FALSE),
          file=paste("../models/latex-tables/supplement/models-for-question-IIIb/byBlock.", b,p,".tex", sep=""))
  }
}


# [2] mean at-issueness and mean prior ratings ----

# prepare the data -----

# read the data 
d <- read_csv("../../data/d.csv")
nrow(d) #10100
summary(d)
names(d)

# import Exp 2 data for two reasons
# 1) it has the mean prior ratings 
# 2) to calculate mean at-issueness ratings (over content/fact combinations)
d2 <- read_csv("../../../exp2/data/d.csv")
nrow(d2) #10000
summary(d2)
names(d2)

# create subsets with the same columns to bind the two datasets and calculate mean at-issueness ratings
tmp_d <- d %>%
  select(prior_type,content,short_trigger,ai)
nrow(tmp_d) #10100

tmp_d2 <- d2 %>%
  select(prior_type,content,short_trigger,ai)
nrow(tmp_d2) #10000

tmp_d = rbind(tmp_d,tmp_d2)
nrow(tmp_d) #20100
tmp_d

# calculate by-content/fact at-issueness means for each predicate
aiMeans = tmp_d %>%
  group_by(short_trigger,content,prior_type) %>%
  mutate(aiMean=mean(ai)) %>%
  ungroup() %>%
  select(-c(ai)) %>%
  distinct()
nrow(aiMeans) #800 = 20 predicates x 40 content/fact combinations

# add the aiMeans to the Exp 1 data
d = left_join(d,aiMeans)
nrow(d) #10100

# check: how many aiMeans per predicate?
tmp = d %>% 
  distinct(short_trigger,prior_type,content,aiMean)
nrow(tmp) #800

# now extract the priorMeans from exp 2 dataset
names(d2)
tmp = d2 %>%
  select(c(content,prior_type,Mean_prior)) %>%
  distinct() %>%
  rename("priorMean" = Mean_prior)
tmp

# bind the priorMeans to the Exp 1 data
d = left_join(d,tmp)
nrow(d)
names(d)

# change predicate names so that loops can work
d = d %>%
  mutate(short_trigger = recode(short_trigger,"be annoyed" = "be.annoyed","be right" = "be.right"))
table(d$short_trigger)

predicates = unique(as.character(d$short_trigger))
predicates

for (p in predicates) {
  nRow = 505
  nRowMinusOne = 505-1
  write_csv(assign(paste("data.", p, sep=""), d %>% filter(short_trigger == p) %>% 
                     #group_by(content) %>% mutate(aiMean=mean(ai)) %>%
                     select(c(short_trigger,projective,aiMean,priorMean,content,workerid)) %>%
                     #mutate(caiMean = aiMean-mean(aiMean), cprior=prior-mean(prior)) %>%
                     mutate(betaProjective = (projective*nRowMinusOne + .5)/nRow))
            ,
            file=paste("models/mean-ai-mean-prior/RQI-IIIa/data.",p,".csv",sep=""))
}

# center the variables in separate loop (doesn't work above for some reason)
for (p in predicates) {
  data = read_csv(paste("models/mean-ai-mean-prior/RQI-IIIa/data.", p, ".csv",sep=""))
  write_csv(assign(paste("data.", p, sep=""), data %>% mutate(caiMean = aiMean-mean(aiMean), cpriorMean=priorMean-mean(priorMean))),
            file=paste("models/mean-ai-mean-prior/RQI-IIIa/data.",p,".csv",sep=""))
}

# check the variables
tmp = read_csv("models/mean-ai-mean-prior/RQI-IIIa/data.acknowledge.csv")
summary(tmp$aiMean)
summary(tmp$caiMean) 
summary(tmp$cpriorMean)
summary(tmp$betaProjective)
nrow(tmp) #505

### questions I, II, III.a ----

# proj_ind ~ caiMean*cpriorMean + (1|content) 
betamodel = bf(betaProjective ~ caiMean*cpriorMean + (1|content),
               phi ~ caiMean*cpriorMean + (1|content), # beta distribution's precision 
               family = Beta())

# fit the model for each predicate
for (p in predicates) {
  data.here = read_csv(paste("models/mean-ai-mean-prior/RQI-IIIa/data.", p, ".csv",sep=""))
  saveRDS(assign(paste("model.", p, ".rds", sep=""), brm(formula = betamodel,
                                                         family=Beta(),
                                                         data=data.here, 
                                                         cores = 4, iter = 4500, warmup = 900,
                                                         control = list(adapt_delta = .99,max_treedepth=15))),
          file=paste("models/mean-ai-mean-prior/RQI-IIIa/model.",p, ".rds", sep=""))
}

# check a model
tmp <- readRDS("models/mean-ai-mean-prior/RQI-IIIa/model.know.rds")
summary(tmp)

# hypotheses
hypotheses.fullData = data.frame(block = character(), predicate = character(), hypothesis = character(), estimate = numeric(), post.prob = numeric(), bf = numeric())
hypotheses.fullData

hypotheses = c("cpriorMean > 0", "caiMean > 0", "caiMean:cpriorMean > 0")
hypotheses

for (p in predicates) {
  for (h in hypotheses) {
    model = readRDS(paste("models/mean-ai-mean-prior/RQI-IIIa/model.",p,".rds",sep=""))
    estimate = hypothesis(model,h)$hypothesis$Estimate
    post.prob = hypothesis(model,h)$hypothesis$Post.Prob
    bf = hypothesis(model,h)$hypothesis$Evid.Ratio
    hypotheses.fullData = hypotheses.fullData %>% 
      add_row(block = "fullData", predicate = p, hypothesis = h, estimate = estimate, post.prob = post.prob, bf = bf)
  }
}

hypotheses.fullData

# save
write_csv(hypotheses.fullData,file="models/mean-ai-mean-prior/RQI-IIIa/hypotheses.fullData.csv")

# now test the < 0 hypotheses (to identify when there is a negative effect)
hypotheses.fullDataNEG = data.frame(block = character(), predicate = character(), hypothesisNEG = character(), estimateNEG = numeric(), post.probNEG = numeric(), bfNEG = numeric())
hypotheses.fullDataNEG

hypotheses = c("cpriorMean < 0", "caiMean < 0", "caiMean:cpriorMean < 0")
hypotheses

for (p in predicates) {
  for (h in hypotheses) {
    model = readRDS(paste("models/mean-ai-mean-prior/RQI-IIIa/model.",p,".rds",sep=""))
    estimate = hypothesis(model,h)$hypothesis$Estimate
    post.prob = hypothesis(model,h)$hypothesis$Post.Prob
    bf = hypothesis(model,h)$hypothesis$Evid.Ratio
    hypotheses.fullDataNEG = hypotheses.fullDataNEG %>% 
      add_row(block = "fullData", predicate = p, hypothesisNEG = h, estimateNEG = estimate, post.probNEG = post.prob, bfNEG = bf)
  }
}

hypotheses.fullDataNEG

# save
write_csv(hypotheses.fullDataNEG,file="models/mean-ai-mean-prior/RQI-IIIa/hypotheses.fullDataNEG.csv")

### question III.b ----

# not run 

### create the table ----

#load the data
full = read_csv(file="models/mean-ai-mean-prior/RQI-IIIa/hypotheses.fullData.csv")
fullNEG = read_csv(file="models/mean-ai-mean-prior/RQI-IIIa/hypotheses.fullDataNEG.csv")

# bind the data
tableData = full
nrow(tableData)
d2 = fullNEG
nrow(d2)

# create a new hypothesis column to bind the data to d
d2 = d2 %>%
  mutate(hypothesis = case_when(hypothesisNEG == "cpriorMean < 0" ~ "cpriorMean > 0",
                                hypothesisNEG == "caiMean < 0" ~ "caiMean > 0",
                                hypothesisNEG == "caiMean:cpriorMean < 0" ~ "caiMean:cpriorMean > 0",
                                TRUE ~ "ERROR")) %>%
  select(-c(hypothesisNEG))

tableData = left_join(tableData,d2,by=c("block","predicate","hypothesis"))
tableData
#view(d)

# when the estimate is < 0, it is possible that there is no effect or a negative effect
# create column "bfNEW" that is the original bf when that estimate > 0
# but that is the bf of the reverse hypothesis when the estimate is <= 0

tableData = tableData %>%
  mutate(bfNEW = case_when(estimate > 0 ~ bf,
                           estimate <= 0 ~ bfNEG,
                           TRUE ~ 555)) %>%
  mutate(bfNEW = round(bfNEW, digits = 0))

# plot the bayes factors
# 100 = "very strong support" according to S. N. Goodman (1999)
# there is 100 times more evidence for H1 than H0
ggplot(tableData[tableData$bfNEW < 500,], aes(x=bfNEW)) +
  geom_histogram() +
  facet_wrap(. ~ hypothesis)
# the shapes of cai and cprior differ very much from that of cprior:cai


# now create a column that color codes the result based on the estimate and bfNEW columns
# based on Goodmann 1999
# bf 21+ "very strong to extreme" (color 1)
# bf 11-20 "strong" (color2)
# bf 2-10 "weak to moderate" (color3)
# bf 1 "no evidence" (white)

tableData = tableData %>%
  mutate(result = case_when(estimate > 0 & bfNEW >= 100  ~ "\\cellcolor{red1}",
                            estimate > 0 & bfNEW > 20 & bfNEW < 100 ~ "\\cellcolor{red1}",
                            estimate > 0 & bfNEW > 10 & bfNEW <= 20 ~ "\\cellcolor{red2}",
                            estimate > 0 & bfNEW > 5 & bfNEW <= 10 ~ "\\cellcolor{red4}",
                            estimate > 0 & bfNEW > 1 & bfNEW <= 5 ~ "\\cellcolor{red4}", 
                            estimate >= 0 & bfNEW == 1 ~ "\\cellcolor{white}", 
                            estimate < 0 & bfNEW >= 100  ~ "\\cellcolor{blue1}",
                            estimate < 0 & bfNEW > 20 & bfNEW < 100 ~ "\\cellcolor{blue1}",
                            estimate < 0 & bfNEW > 10 & bfNEW <= 20 ~ "\\cellcolor{blue2}",
                            estimate < 0 & bfNEW > 5 & bfNEW <= 10 ~ "\\cellcolor{blue4}",
                            estimate < 0 & bfNEW > 1 & bfNEW <= 5 ~ "\\cellcolor{blue4}", 
                            estimate < 0 & bfNEW == 1 ~ "\\cellcolor{white}",
                            TRUE ~ "ERROR"))

#view(tableData)

# to sort (rotated) predicates by projection mean
d <- read_csv("../../data/d.csv")
nrow(d) #10100

# change predicate names so that loops can work
d = d %>%
  mutate(short_trigger = recode(short_trigger,"be annoyed" = "be.annoyed","be right" = "be.right"))
table(d$short_trigger)

tmp = d %>%
  rename("predicate" = short_trigger) %>%
  mutate(predicate = paste0("\\rot{", predicate, "}")) %>%
  group_by(predicate) %>%
  summarize(Mean_proj = mean(projective)) %>%
  mutate(predicate = fct_reorder(as.factor(predicate),Mean_proj)) 
tmp
levels(tmp$predicate)

# now get rid of all the unneeded columns and create a new hypothesis column that doesn't reflect the order (> 0 or < 0)
# and a new block column
tableData = tableData %>%
  select(c("hypothesis","predicate","result")) %>%
  mutate(hypothesis = case_when(hypothesis == "cpriorMean > 0" ~ "mean prior on proj",
                                hypothesis == "caiMean > 0" ~ "mean nai on proj",
                                hypothesis == "caiMean:cpriorMean > 0" ~ "mean prior:mean nai on proj")) %>%
  #mutate(block = case_when(block == "fullData" ~ "full",
  #block == "_projai." ~ "proj/ai",
  #block == "_aiproj." ~ "ai/proj")) %>%
  mutate(predicate = paste0("\\rot{", predicate, "}")) %>%
  mutate(predicate = fct_relevel(predicate,rev(levels(tmp$predicate))))
tableData

# get the data into the desired shape (predicates as columns, rows: full data first, then proj/ai, then ai/proj)

tableData = tableData %>% 
  spread(predicate,result)

#x <- c("full", "proj/ai", "ai/proj")
y <- c("mean prior on proj", "mean nai on proj", "mean prior:mean nai on proj")

tableData = tableData %>%
  #mutate(block =  factor(block, levels = x)) %>%
  #arrange(block) %>%
  mutate(hypothesis = factor(hypothesis, levels = y)) %>%
  arrange(hypothesis)
tableData

# make factive predicates orange
tableData = tableData %>%
  rename("\\rot{\\color{orange}be annoyed\\color{black}}" = "\\rot{be.annoyed}",
         "\\rot{\\color{orange}discover\\color{black}}" = "\\rot{discover}",
         "\\rot{\\color{orange}know\\color{black}}" = "\\rot{know}",
         "\\rot{\\color{orange}see\\color{black}}" = "\\rot{see}",
         "\\rot{\\color{orange}reveal\\color{black}}" = "\\rot{reveal}",
         "\\rot{be right}" = "\\rot{be.right}")

tableData = tableData %>%
  #rename("{\\bf Data}" = "block") %>%
  rename("{\\bf Effect}" = "hypothesis")
tableData

# save tableData
saveRDS(tableData, "models/mean-ai-mean-prior/RQI-IIIa/tableDataPROJ")

# create new first column (Effect)
effect <- c("{\\bf (I):} prior on", "\\hspace*{.7cm}projection", "", 
            "{\\bf (II):} not-at-issue on", "\\hspace*{.7cm} projection", "",
            "{\\bf (IIIa):} prior:not-at-issue", "\\hspace*{1.1cm} on projection", "")
#"{\\bf (IIIb):} prior on", "\\hspace*{1.1cm} not-at-issueness", "")


# remove original effect column, add new one, give it a good header
tableData = tableData %>%
  select(-c("{\\bf Effect}")) 
tableData = cbind(effect,tableData) %>%
  rename("{\\bf Effect}" = "effect")

# create Latex code for Table 1a ----

t1 = print(xtable(tableData),
           include.rownames=FALSE,
           include.colnames=TRUE,
           #tabular.environment="longtable",
           floating=FALSE,
           latex.environments=NULL,
           booktabs=FALSE,
           sanitize.text.function = function(x){x},
           #hline.after = c(0,0,3,3,6,6,9,9,12)
           #hline.after = c(0,0,3,6,9,9,12,15,18,18,21,21,24)
)

write(t1, "models/mean-ai-mean-prior/RQI-IIIa/t1")

# output of the models for the supplement ----

# there are 60 models for the analysis of questions I, II, and III.a
# for each of the 20 predicates, full data, proj/ai, ai/proj
# and another 60 models for the analysis of question III.b

# read the data (to create list of predicates)
d <- read_csv("../data/d.csv")
nrow(d) #10100
# change predicate names so that loops can work
d = d %>%
  mutate(short_trigger = recode(short_trigger,"be annoyed" = "be.annoyed","be right" = "be.right"))
table(d$short_trigger)

# define the predicates
predicates = unique(as.character(d$short_trigger))
predicates

# main analysis models for questions I, II, III.a
for (p in predicates) {
  model = readRDS(paste("../models/projection-main/model.",p,".rds",sep=""))
  tmp_table = as.data.frame(summary(model)$fixed)
  tmp_table = tmp_table %>% 
    rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
    select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
    mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
    mutate("95% CrI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
    select(-c(lower,upper)) %>%
    rename("Expected mean" = "Estimate")
  caption = paste("Full data, ",p,sep="")
  write(print(xtable(tmp_table,caption = caption),
              include.rownames=TRUE,
              include.colnames=TRUE,
              tabular.environment="tabular",
              floating=FALSE,
              latex.environments=NULL,
              booktabs=FALSE),
        file=paste("../models/latex-tables/supplement/models-for-questions-I-II-IIIa/full.",p,".tex", sep=""))
}

# by-block analysis models for questions I, II, III.a
block = c("projai.", "aiproj.")
block

for (p in predicates) {
  for (b in block) {
    model = readRDS(paste("../models/projection-byBlock/model_",b,p,".rds",sep=""))
    tmp_table = as.data.frame(summary(model)$fixed)
    tmp_table = tmp_table %>% 
      rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
      select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
      mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
      mutate("95% CrI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
      select(-c(lower,upper)) %>%
      rename("Expected mean" = "Estimate")
    caption = paste(b,"data, ",p,sep="")
    write(print(xtable(tmp_table,caption = caption),
                include.rownames=TRUE,
                include.colnames=TRUE,
                #tabular.environment="longtable",
                floating=FALSE,
                latex.environments=NULL,
                booktabs=FALSE),
          file=paste("../models/latex-tables/supplement/models-for-questions-I-II-IIIa/byBlock.", b,p,".tex", sep=""))
  }
}

# main analysis models for question III.b
for (p in predicates) {
  model = readRDS(paste("../models/ai-prior-main/model.",p,".rds",sep=""))
  tmp_table = as.data.frame(summary(model)$fixed)
  tmp_table = tmp_table %>% 
    rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
    select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
    mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
    mutate("95% CrI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
    select(-c(lower,upper)) %>%
    rename("Expected mean" = "Estimate")
  caption = paste("Full data, ",p,sep="")
  write(print(xtable(tmp_table,caption = caption),
              include.rownames=TRUE,
              include.colnames=TRUE,
              tabular.environment="tabular",
              floating=FALSE,
              latex.environments=NULL,
              booktabs=FALSE),
        file=paste("../models/latex-tables/supplement/models-for-question-IIIb/full.",p,".tex", sep=""))
}

# by-block analysis models for question III.b
block = c("projai.", "aiproj.")
block

for (p in predicates) {
  for (b in block) {
    model = readRDS(paste("../models/ai-prior-byBlock/model_",b,p,".rds",sep=""))
    tmp_table = as.data.frame(summary(model)$fixed)
    tmp_table = tmp_table %>% 
      rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
      select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
      mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
      mutate("95% CrI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
      select(-c(lower,upper)) %>%
      rename("Expected mean" = "Estimate")
    caption = paste(b,"data, ",p,sep="")
    write(print(xtable(tmp_table,caption = caption),
                include.rownames=TRUE,
                include.colnames=TRUE,
                #tabular.environment="longtable",
                floating=FALSE,
                latex.environments=NULL,
                booktabs=FALSE),
          file=paste("../models/latex-tables/supplement/models-for-question-IIIb/byBlock.", b,p,".tex", sep=""))
  }
}


