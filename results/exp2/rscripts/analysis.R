# Exp2: interaction of mean prior beliefs, at-issueness and projection
# for 20 clause-embedding predicates
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

# structure of this file:

# main analyses: "projection" for questions (I, II, III.a), "ai-prior" for question (III.b)
# by-block analyses: for questions (I, II, III.a), for question (III.b)
# code to produce Table 1b
# code to produce model outputs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# main analyses ----
# 20 Bayesian mixed-effects beta-regressions on full data set, without block fixed effect
# one for each predicate

# read the data 
d <- read_csv("../data/d.csv")
nrow(d) #10000
summary(d)

# change predicate names so that loops can work
d = d %>%
  mutate(short_trigger = recode(short_trigger,"be annoyed" = "be.annoyed","be right" = "be.right"))
table(d$short_trigger)

# create separate data frame for each predicate, and center the predictors in each data frame
# calculate a new betaProjective and betaAI responses
# because beta regression cannot handle (0,1) (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)

predicates = unique(as.character(d$short_trigger))
predicates

# how many rows for each predicate?
table(d$short_trigger)

for (p in predicates) {
  nRow = 500
  nRowMinusOne = nRow-1
  write_csv(assign(paste("data.", p, sep=""), d %>% filter(short_trigger == p)) %>% 
                                                    mutate(cMean_prior = Mean_prior-mean(Mean_prior), cai = ai-mean(ai)) %>%
                                                    mutate(betaProjective = (projective*nRowMinusOne + .5)/nRow) %>%
                                                    mutate(betaAI = (ai*nRowMinusOne + .5)/nRow),
            file=paste("../models/projection-main/data.",p,".csv",sep=""))
}


#tmp = read_csv("../models/projection-main/data.know.csv")
#summary(tmp)

# fit the models

##### questions (I, II, III.a) predict projection from prior, ai and interaction ----
betamodel = bf(betaProjective ~ cMean_prior*cai + (1+cMean_prior+cai|content),
               phi ~ cMean_prior*cai + (1|content), # beta distribution's precision 
               family = Beta())

# fit the model for each predicate
for (p in predicates) {
  data.here = read_csv(paste("../models/projection-main/data.", p, ".csv",sep=""))
  saveRDS(assign(paste("model.", p, ".rds", sep=""), brm(formula = betamodel,
                                                family=Beta(),
                                                data=data.here, 
                                                cores = 4, iter = 3000, warmup = 500,
                                                control = list(adapt_delta = .95,max_treedepth=15))),
  file=paste("../models/projection-main/model.",p, ".rds", sep=""))
}

# tmp <- readRDS("../models/projection-main/model.think.rds")
# summary(tmp)

# question (III.b) predict ai from prior 
betamodel = bf(betaAI ~ cMean_prior + (1+cMean_prior|content),
               phi ~ cMean_prior + (1|content), # beta distribution's precision 
               family = Beta())

# fit the model for each predicate
for (p in predicates) {
  data.here = read_csv(paste("../models/projection-main/data.", p, ".csv",sep=""))
  saveRDS(assign(paste("model.", p, ".rds", sep=""), brm(formula = betamodel,
                                                         family=Beta(),
                                                         data=data.here, 
                                                         cores = 4, iter = 3000, warmup = 500,
                                                         control = list(adapt_delta = .95,max_treedepth=15))),
          file=paste("../models/ai-prior-main/model.",p, ".rds", sep=""))
}

# tmp <- readRDS("../models/ai-prior-main/model.think.rds")
# summary(tmp)

# 3 divergent transitions for "be.annoyed"
# run new model for "be.annoyed"

be.annoyed.data = read_csv("../models/projection-main/data.be.annoyed.csv")
m.be.annoyed = brm(formula = betamodel,
              family=Beta(),
              data=be.annoyed.data,
              cores = 4, iter = 4000, warmup = 500,
              control = list(adapt_delta = .95,max_treedepth=15))
saveRDS(m.be.annoyed, file = "../models/ai-prior-main/model.be.annoyed.rds")

# evaluate hypotheses ---- 
# create data frame to store the results of the evaluations

##### questions (I, II, III.a) ----

hypotheses.fullData = data.frame(block = character(), predicate = character(), hypothesis = character(), estimate = numeric(), post.prob = numeric(), bf = numeric())
hypotheses.fullData

hypotheses = c("cMean_prior > 0", "cai > 0", "cMean_prior:cai > 0")
hypotheses

for (p in predicates) {
  for (h in hypotheses) {
    model = readRDS(paste("../models/projection-main/model.",p,".rds",sep=""))
    estimate = hypothesis(model,h)$hypothesis$Estimate
    post.prob = hypothesis(model,h)$hypothesis$Post.Prob
    bf = hypothesis(model,h)$hypothesis$Evid.Ratio
    hypotheses.fullData = hypotheses.fullData %>% 
      add_row(block = "fullData", predicate = p, hypothesis = h, estimate = estimate, post.prob = post.prob, bf = bf)
  }
}

hypotheses.fullData

# save
write_csv(hypotheses.fullData,file="../models/projection-main/hypotheses.fullData.csv")

# now test the < 0 hypotheses (to identify when there is a negative effect)
hypotheses.fullDataNEG = data.frame(block = character(), predicate = character(), hypothesisNEG = character(), estimateNEG = numeric(), post.probNEG = numeric(), bfNEG = numeric())
hypotheses.fullDataNEG

hypotheses = c("cMean_prior < 0", "cai < 0", "cMean_prior:cai < 0")
hypotheses

for (p in predicates) {
  for (h in hypotheses) {
    model = readRDS(paste("../models/projection-main/model.",p,".rds",sep=""))
    estimate = hypothesis(model,h)$hypothesis$Estimate
    post.prob = hypothesis(model,h)$hypothesis$Post.Prob
    bf = hypothesis(model,h)$hypothesis$Evid.Ratio
    hypotheses.fullDataNEG = hypotheses.fullDataNEG %>% 
      add_row(block = "fullData", predicate = p, hypothesisNEG = h, estimateNEG = estimate, post.probNEG = post.prob, bfNEG = bf)
  }
}

hypotheses.fullDataNEG

# save
write_csv(hypotheses.fullDataNEG,file="../models/projection-main/hypotheses.fullDataNEG.csv")

##### question (III.b) ----

hypotheses.AIPRIOR = data.frame(block = character(), predicate = character(), hypothesis = character(), estimate = numeric(), post.prob = numeric(), bf = numeric())
hypotheses.AIPRIOR

hypotheses = c("cMean_prior > 0")
hypotheses

for (p in predicates) {
  for (h in hypotheses) {
    model = readRDS(paste("../models/ai-prior-main/model.",p,".rds",sep=""))
    estimate = hypothesis(model,h)$hypothesis$Estimate
    post.prob = hypothesis(model,h)$hypothesis$Post.Prob
    bf = hypothesis(model,h)$hypothesis$Evid.Ratio
    hypotheses.AIPRIOR = hypotheses.AIPRIOR %>% 
      add_row(block = "fullData", predicate = p, hypothesis = h, estimate = estimate, post.prob = post.prob, bf = bf)
  }
}

hypotheses.AIPRIOR

# save
write_csv(hypotheses.AIPRIOR,file="../models/ai-prior-main/hypotheses.AIPRIOR.csv")

# now test the < 0 hypotheses (to identify when there is a negative effect)
hypotheses.AIPRIOR.NEG = data.frame(block = character(), predicate = character(), hypothesisNEG = character(), estimateNEG = numeric(), post.probNEG = numeric(), bfNEG = numeric())
hypotheses.AIPRIOR.NEG

hypotheses = c("cMean_prior < 0")
hypotheses

for (p in predicates) {
  for (h in hypotheses) {
    model = readRDS(paste("../models/ai-prior-main/model.",p,".rds",sep=""))
    estimate = hypothesis(model,h)$hypothesis$Estimate
    post.prob = hypothesis(model,h)$hypothesis$Post.Prob
    bf = hypothesis(model,h)$hypothesis$Evid.Ratio
    hypotheses.AIPRIOR.NEG = hypotheses.AIPRIOR.NEG %>% 
      add_row(block = "fullData", predicate = p, hypothesisNEG = h, estimateNEG = estimate, post.probNEG = post.prob, bfNEG = bf)
  }
}

hypotheses.AIPRIOR.NEG

# save
write_csv(hypotheses.AIPRIOR.NEG,file="../models/ai-prior-main/hypotheses.AIPRIOR.NEG.csv")

# by-block analyses ----
# 20 Bayesian mixed-effects beta-regressions with uncentered block effect
# one for each predicate

# read the data 
d <- read_csv("../data/d.csv")
nrow(d) #10000
summary(d)

# change predicate names so that loops can work
d = d %>%
  mutate(short_trigger = recode(short_trigger,"be annoyed" = "be.annoyed","be right" = "be.right"))
table(d$short_trigger)

##### questions (I, II, III.a) ----

# create separate data frames: for each predicate, for each block order, and center the predictors
predicates = unique(as.character(d$short_trigger))
predicates

for (p in predicates) {
  write_csv(assign(paste("data_projai.", p, sep=""), d %>% filter(short_trigger == p & block_ai == "block2") %>% 
                                                            mutate(cMean_prior = Mean_prior-mean(Mean_prior), cai = ai-mean(ai))),
  file=paste("../models/projection-byBlock/data_projai.",p,".csv",sep=""))
  write_csv(assign(paste("data_aiproj.", p, sep=""), d %>% filter(short_trigger == p & block_ai == "block1") %>% 
                                                          mutate(cMean_prior = Mean_prior-mean(Mean_prior), cai = ai-mean(ai))),
  file=paste("../models/projection-byBlock/data_aiproj.",p,".csv",sep=""))
}


#tmp = read_csv("../models/projection-byBlock/data_projai.know.csv")
#summary(tmp)

# calculate a new betaProjective response
# because beta regression cannot handle (0,1) (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)

for (p in predicates) {
  # define some expressions
  nRow_projai = nrow(read_csv(paste("../models/projection-byBlock/data_projai.",p,".csv", sep="")))
  nRowMinusOne_projai = nRow_projai-1
  nRow_aiproj = nrow(read_csv(paste("../models/projection-byBlock/data_aiproj.",p,".csv", sep="")))
  nRowMinusOne_aiproj = nRow_aiproj-1
  # now create betaProjective and betaAI responses
  write_csv(assign(paste("data_projai.", p, sep=""), get(paste("data_projai.", p, sep="")) %>% 
                     mutate(betaProjective = (projective*nRowMinusOne_projai + .5)/nRow_projai) %>%
                     mutate(betaAI = (ai*nRowMinusOne_projai + .5)/nRow_projai)),
  file=paste("../models/projection-byBlock/data_projai.",p,".csv",sep=""))
  write_csv(assign(paste("data_aiproj.", p, sep=""), get(paste("data_aiproj.", p, sep="")) %>% 
                     mutate(betaProjective = (projective*nRowMinusOne_aiproj + .5)/nRow_aiproj) %>%
                     mutate(betaAI = (ai*nRowMinusOne_projai + .5)/nRow_projai)),
  file=paste("../models/projection-byBlock/data_aiproj.",p,".csv",sep=""))
}

#tmp = read_csv("../models/projection-byBlock/data_projai.know.csv")
#summary(tmp)

# fit the model: questions (I, II, III.a)

betamodel = bf(betaProjective ~ cMean_prior*cai + (1+cMean_prior+cai|content),
               phi ~ cMean_prior*cai + (1|content), # beta distribution's precision 
               family = Beta())

data = c("_projai.", "_aiproj.")
data

for (p in predicates) {
  for (d in data) {
  data.here = read_csv(paste("../models/projection-byBlock/data",d,p,".csv",sep=""))
  saveRDS(assign(paste("model",d,p, sep=""), brm(formula = betamodel,
                                                    family=Beta(),
                                                    data=data.here, 
                                                    cores = 4, iter = 4000, warmup = 1000,
                                                    control = list(adapt_delta = .95,max_treedepth=15))),
  file=paste("../models/projection-byBlock/model",d,p,".rds",sep=""))
  }
}

# tmp <- readRDS("../models/projection-byBlock/model_aiproj.be.annoyed.rds")
# summary(tmp)

# 47 divergent transitions for "be.annoyed" in "ai/proj" block
be.annoyed.data = read_csv("../models/projection-byBlock/data_aiproj.be.annoyed.csv")
m.be.annoyed.aiproj = brm(formula = betamodel,
                          family=Beta(),
                          data=be.annoyed.data, 
                          cores = 4, iter = 5000, warmup = 500,
                          control = list(adapt_delta = .99,max_treedepth=15))
saveRDS(m.be.annoyed.aiproj, file = "../models/projection-byBlock/model_aiproj.be.annoyed.rds")

# fit the model: question (III.b)

betamodel = bf(betaAI ~ cMean_prior + (1+cMean_prior|content),
               phi ~ cMean_prior + (1|content), # beta distribution's precision 
               family = Beta())

data = c("_projai.", "_aiproj.")
data

for (p in predicates) {
  for (d in data) {
    data.here = read_csv(paste("../models/projection-byBlock/data",d,p,".csv",sep=""))
    saveRDS(assign(paste("model",d,p, sep=""), brm(formula = betamodel,
                                                   family=Beta(),
                                                   data=data.here, 
                                                   cores = 4, iter = 4000, warmup = 1000,
                                                   control = list(adapt_delta = .95,max_treedepth=15))),
            file=paste("../models/ai-prior-byBlock/model",d,p,".rds",sep=""))
  }
}

# tmp <- readRDS("../models/ai-prior-byBlock/model_projai.be.annoyed.rds")
# summary(tmp)

# 3 divergent transitions for be.annoyed in aiproj block
be.annoyed.data = read_csv("../models/projection-byBlock/data_aiproj.be.annoyed.csv")
m.be.annoyed = brm(formula = betamodel,
                   family=Beta(),
                   data=be.annoyed.data, 
                   cores = 4, iter = 4000, warmup = 1000,
                   control = list(adapt_delta = .97,max_treedepth=15))
saveRDS(m.be.annoyed, file = "../models/ai-prior-byBlock/model_aiproj.be.annoyed.rds")

# evaluate the hypotheses ----

##### questions (I, II, III.a) ----

hypotheses.by.block = data.frame(block = character(), predicate = character(), hypothesis = character(), estimate = numeric(), post.prob = numeric(), bf = numeric())
hypotheses.by.block

# hypotheses
hypotheses = c("cMean_prior > 0", "cai > 0", "cMean_prior:cai > 0")
hypotheses

for (d in data) {
  for (p in predicates) {
    for (h in hypotheses) {
      tmp = readRDS(paste("../models/projection-byBlock/model",d, p, ".rds", sep=""))
      estimate = hypothesis(tmp,h)$hypothesis$Estimate
      post.prob = hypothesis(tmp,h)$hypothesis$Post.Prob
      bf = hypothesis(tmp,h)$hypothesis$Evid.Ratio
      hypotheses.by.block = hypotheses.by.block %>% 
        add_row(block = d, predicate = p, hypothesis = h, estimate = estimate, post.prob = post.prob, bf = bf)
    }
  }
}

hypotheses.by.block

# save
write_csv(hypotheses.by.block,file="../models/projection-byBlock/hypotheses.by.block.csv")

# now test the < 0 hypotheses (to identify when there is a negative effect)
hypotheses.by.blockNEG = data.frame(block = character(), predicate = character(), hypothesisNEG = character(), estimateNEG = numeric(), post.probNEG = numeric(), bfNEG = numeric())
hypotheses.by.blockNEG

hypotheses = c("cMean_prior < 0", "cai < 0", "cMean_prior:cai < 0")
hypotheses

for (d in data) {
  for (p in predicates) {
    for (h in hypotheses) {
      tmp = readRDS(paste("../models/projection-byBlock/model",d, p, ".rds", sep=""))
      estimate = hypothesis(tmp,h)$hypothesis$Estimate
      post.prob = hypothesis(tmp,h)$hypothesis$Post.Prob
      bf = hypothesis(tmp,h)$hypothesis$Evid.Ratio
      hypotheses.by.blockNEG = hypotheses.by.blockNEG %>% 
        add_row(block = d, predicate = p, hypothesisNEG = h, estimateNEG = estimate, post.probNEG = post.prob, bfNEG = bf)
    }
  }
}

hypotheses.by.blockNEG

# save
write_csv(hypotheses.by.blockNEG,file="../models/projection-byBlock/hypotheses.by.blockNEG.csv")

##### question (III.b) ----

hypotheses.by.block = data.frame(block = character(), predicate = character(), hypothesis = character(), estimate = numeric(), post.prob = numeric(), bf = numeric())
hypotheses.by.block

# hypotheses
hypotheses = c("cMean_prior > 0")
hypotheses

for (d in data) {
  for (p in predicates) {
    for (h in hypotheses) {
      tmp = readRDS(paste("../models/ai-prior-byBlock/model",d, p, ".rds", sep=""))
      estimate = hypothesis(tmp,h)$hypothesis$Estimate
      post.prob = hypothesis(tmp,h)$hypothesis$Post.Prob
      bf = hypothesis(tmp,h)$hypothesis$Evid.Ratio
      hypotheses.by.block = hypotheses.by.block %>% 
        add_row(block = d, predicate = p, hypothesis = h, estimate = estimate, post.prob = post.prob, bf = bf)
    }
  }
}

hypotheses.by.block

# save
write_csv(hypotheses.by.block,file="../models/ai-prior-byBlock/hypotheses.by.block.csv")

# now test the < 0 hypotheses (to identify when there is a negative effect)
hypotheses.by.blockNEG = data.frame(block = character(), predicate = character(), hypothesisNEG = character(), estimateNEG = numeric(), post.probNEG = numeric(), bfNEG = numeric())
hypotheses.by.blockNEG

hypotheses = c("cMean_prior < 0")
hypotheses

for (d in data) {
  for (p in predicates) {
    for (h in hypotheses) {
      tmp = readRDS(paste("../models/ai-prior-byBlock/model",d, p, ".rds", sep=""))
      estimate = hypothesis(tmp,h)$hypothesis$Estimate
      post.prob = hypothesis(tmp,h)$hypothesis$Post.Prob
      bf = hypothesis(tmp,h)$hypothesis$Evid.Ratio
      hypotheses.by.blockNEG = hypotheses.by.blockNEG %>% 
        add_row(block = d, predicate = p, hypothesisNEG = h, estimateNEG = estimate, post.probNEG = post.prob, bfNEG = bf)
    }
  }
}

hypotheses.by.blockNEG

# save
write_csv(hypotheses.by.blockNEG,file="../models/ai-prior-byBlock/hypotheses.by.blockNEG.csv")

# create data for Table 1b (based on Exp 2) ----

##### questions (I, II, III.a) ----

#load the data
full = read_csv(file="../models/projection-main/hypotheses.fullData.csv")
blockwise = read_csv(file="../models/projection-byBlock/hypotheses.by.block.csv")

# also load the data with the alternative hypotheses
fullNEG = read_csv(file="../models/projection-main/hypotheses.fullDataNEG.csv")
blockwiseNEG = read_csv(file="../models/projection-byBlock/hypotheses.by.blockNEG.csv")

# bind the data
tableData = rbind(full,blockwise)
nrow(tableData)
d2 = rbind(fullNEG,blockwiseNEG)
nrow(d2)

# create a new hypothesis column to bind the data to d
d2 = d2 %>%
  mutate(hypothesis = case_when(hypothesisNEG == "cMean_prior < 0" ~ "cMean_prior > 0",
                                hypothesisNEG == "cai < 0" ~ "cai > 0",
                                hypothesisNEG == "cMean_prior:cai < 0" ~ "cMean_prior:cai > 0",
                                TRUE ~ "ERROR")) %>%
  select(-c(hypothesisNEG))

tableData = left_join(tableData,d2,by=c("block","predicate","hypothesis"))
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

# to sort (rotated) predicates by projection mean in Exp 1
d <- read_csv("../../exp1/data/d.csv")
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
  select(c("hypothesis","block","predicate","result")) %>%
  mutate(hypothesis = case_when(hypothesis == "cMean_prior > 0" ~ "prior on proj",
                                hypothesis == "cai > 0" ~ "nai on proj",
                                hypothesis == "cMean_prior:cai > 0" ~ "prior:nai on proj")) %>%
  mutate(block = case_when(block == "fullData" ~ "full",
                           block == "_projai." ~ "proj/ai",
                           block == "_aiproj." ~ "ai/proj")) %>%
  mutate(predicate = paste0("\\rot{", predicate, "}")) %>%
  mutate(predicate = fct_relevel(predicate,rev(levels(tmp$predicate))))

# get the data into the desired shape (predicates as columns, rows: full data first, then proj/ai, then ai/proj)

tableData = tableData %>% 
  spread(predicate,result)

x <- c("full", "proj/ai", "ai/proj")
y <- c("prior on proj", "nai on proj", "prior:nai on proj")

tableData = tableData %>%
  mutate(block =  factor(block, levels = x)) %>%
  arrange(block) %>%
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
  rename("{\\bf Data}" = "block") %>%
  rename("{\\bf Effect}" = "hypothesis")


# save tableData
saveRDS(tableData, "../models/latex-tables/tableDataPROJ")

#### question (III.b) ----

# load data
full = read_csv(file="../models/ai-prior-main/hypotheses.AIPRIOR.csv")
blockwise = read_csv(file="../models/ai-prior-byBlock/hypotheses.by.block.csv")

# also load the data with the alternative hypotheses
fullNEG = read_csv(file="../models/ai-prior-main/hypotheses.AIPRIOR.NEG.csv")
blockwiseNEG = read_csv(file="../models/ai-prior-byBlock/hypotheses.by.blockNEG.csv")

# bind the data
tableData = rbind(full,blockwise)
nrow(tableData)
d2 = rbind(fullNEG,blockwiseNEG)
nrow(d2)

# create a new hypothesis column to bind the data to d
d2 = d2 %>%
  mutate(hypothesis = case_when(hypothesisNEG == "cMean_prior < 0" ~ "cMean_prior > 0",
                                TRUE ~ "ERROR")) %>%
  select(-c(hypothesisNEG))

tableData = left_join(tableData,d2,by=c("block","predicate","hypothesis"))
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
ggplot(tableData[tableData$bfNEW < 20,], aes(x=bfNEW)) +
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

# to sort (rotated) predicates by projection mean in Exp 1
d <- read_csv("../../exp1/data/d.csv")
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
  select(c("hypothesis","block","predicate","result")) %>%
  mutate(hypothesis = case_when(hypothesis == "cMean_prior > 0" ~ "prior on nai")) %>%
  mutate(block = case_when(block == "fullData" ~ "full",
                           block == "_projai." ~ "proj/ai",
                           block == "_aiproj." ~ "ai/proj")) %>%
  mutate(predicate = paste0("\\rot{", predicate, "}")) %>%
  mutate(predicate = fct_relevel(predicate,rev(levels(tmp$predicate))))

# get the data into the desired shape (predicates as columns, rows: full data first, then proj/ai, then ai/proj)

tableData = tableData %>% 
  spread(predicate,result)

x <- c("full", "proj/ai", "ai/proj")
#y <- c("prior on nai")

tableData = tableData %>%
  mutate(block =  factor(block, levels = x)) %>%
  arrange(block)
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
  rename("{\\bf Data}" = "block") %>%
  rename("{\\bf Effect}" = "hypothesis")

# save tableData
saveRDS(tableData, "../models/latex-tables/tableDataAIPRIOR")

# combine the results from the two experiments ----

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# read data 
tableData1 = readRDS("../models/latex-tables/tableDataPROJ")
tableData2 = readRDS("../models/latex-tables/tableDataAIPRIOR")

tableData = rbind(tableData1, tableData2)

# create new first column (Effect)
effect <- c("{\\bf (I):} mean prior on", "\\hspace*{.7cm}projection", "", 
            "{\\bf (II):} not-at-issue on", "\\hspace*{.7cm} projection", "",
            "{\\bf (IIIa):} mean prior", "\\hspace*{1.1cm} :not-at-issue", "\\hspace*{1.1cm} on projection",
            "{\\bf (IIIb):} mean prior on", "\\hspace*{1.1cm} not-at-issueness", "")


# remove original effect column, add new one, give it a good header
tableData = tableData %>%
  select(-c("{\\bf Effect}")) 
tableData = cbind(effect,tableData) %>%
  rename("{\\bf Effect}" = "effect")

# create Latex code for Table 1b ----

t1 = print(xtable(tableData),
           include.rownames=FALSE,
           include.colnames=TRUE,
           floating=FALSE,
           latex.environments=NULL,
           booktabs=FALSE,
           sanitize.text.function = identity,
           hline.after = c(0,0,3,3,6,6,9,9,12)
)

write(t1, "../models/latex-tables/t1")

# output of the models for supplement ----

# load the models
m = read(file="../models/main-analysis/model.know")
summary(m)


m_projai = readRDS(file="../models/beta-model-projai-PROVE.rds")
m_aiproj = readRDS(file="../models/beta-model-aiproj-PROVE.rds")

summary(m)

m = readRDS(file="../models/beta-model-main.rds")
m_projai = readRDS(file="../models/beta-model-projai.rds")
m_aiproj = readRDS(file="../models/beta-model-aiproj.rds")

mcmcReg(m)

texreg::mcmcReg(m, file="../models/fullModelOutput/m.tex")

tableApp1 = print(xtable(m),
                  #only.contents = T,
                  include.rownames=FALSE,
                  include.colnames=TRUE,
                  tabular.environment="longtable",
                  floating=FALSE,
                  hline.after = NULL,
                  latex.environments=NULL,
                  booktabs=TRUE,
                  sanitize.text.function = function(x){x},
                  comment = F
)

# write the table, print in latex document in supplement
write(tableApp1, "../models/fullModelOutput/analysis1.tex")

# CONTINUE FIXING CODE STARTING HERE

# output of the models for supplement ----

# load the models
m = readRDS(file="../models/beta-model-main-PROVE.rds")
m_projai = readRDS(file="../models/beta-model-projai-PROVE.rds")
m_aiproj = readRDS(file="../models/beta-model-aiproj-PROVE.rds")

summary(m)

m = readRDS(file="../models/beta-model-main.rds")
m_projai = readRDS(file="../models/beta-model-projai.rds")
m_aiproj = readRDS(file="../models/beta-model-aiproj.rds")

mcmcReg(m)

texreg::mcmcReg(m, file="../models/fullModelOutput/m.tex")

tableApp1 = print(xtable(m),
                  #only.contents = T,
                  include.rownames=FALSE,
                  include.colnames=TRUE,
                  tabular.environment="longtable",
                  floating=FALSE,
                  hline.after = NULL,
                  latex.environments=NULL,
                  booktabs=TRUE,
                  sanitize.text.function = function(x){x},
                  comment = F
)

# write the table, print in latex document in supplement
write(tableApp1, "../models/fullModelOutput/analysis1.tex")
                
# Latex output for Bayesian models in supplements ----

# model with centered block effect
printd = as.data.frame(summary(m.d.B.cblock)$fixed)

# create column name for first column
printd <- rownames_to_column(printd, var = "tmp")

printd = printd %>%
  mutate(tmp = recode(tmp, "Intercept" = "Intercept (prove)")) %>%
  mutate(tmp = gsub("short_trigger", "predicate_", tmp)) %>%
  rename("lower" = "l-95% CI", "upper" = "u-95% CI") %>%
  select(-c(Est.Error,Rhat,Bulk_ESS,Tail_ESS)) %>%
  mutate(across(c('Estimate','lower', 'upper'), round, 2)) %>%
  mutate("95% CI" = paste0("[",as.character(lower), ",", as.character(upper), "]")) %>%
  rename("beta" = "Estimate") %>%
  rename("95% CI" = "95% CI") %>%
  select(-c(lower,upper))
printd

# change column names
colnames(printd) <- c("","beta","95% CI")

exp1.model = print(xtable(printd),
                include.rownames=FALSE,
                include.colnames=TRUE,
                tabular.environment="longtable",
                floating=FALSE,
                latex.environments=NULL,
                booktabs=FALSE)
write(exp1.model, "../models/latex-tables/exp1.model")

# Supplement: Analysis of manipulation of prior beliefs ----
names(d)
table(d$content)

# prepare the data
d$prior_type = as.factor(as.character(d$prior_type))
d$workerid = as.factor(as.character(d$workerid))

# set lower probability fact as reference level of prior_type
d$prior_type = relevel(d$prior_type, ref="low_prior")
levels(d$prior_type)
contrasts(d$prior_type)

# analysis: does high/low prob fact predict actual prior ratings?
m.prior = lmer(prior ~ prior_type + (1+prior_type|content) + (1+prior_type|workerid), data=d, REML=F)
saveRDS(m.prior, "../models/m.prior.rds")
m.prior <- readRDS("../models/m.prior.rds")

summary(m.prior)
# prior_typehigh_prior  0.51466    0.02956 22.47640   17.41 1.54e-14 ***


