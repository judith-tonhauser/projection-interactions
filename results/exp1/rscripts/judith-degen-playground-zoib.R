# Exp1: interaction of prior beliefs, at-issueness and projection
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

# power analysis

# effect sizes of at-issueness in exp. 1a and 1b, for comparison: .37, .34
# effect sizes in exp. 2a and 2b: .25/.29 (depending on random effects structure)

# we analyze the power of two different models: 
# m -- has full maximal random effects structure and detected only marginally significant effect of at-issueness (beta = .25)
fixef(m)["cmean_ai"]
powerSim(m) # power: 60.40% (57.29, 63.45)

# m.report -- has only by-content random intercepts after model comparison (beta = .29)
fixef(m.report)["cmean_ai"]
powerSim(m.report) # power: 99.80% (99.28, 99.98)

# structure of this file:

# main analyses: "projection" for questions (I, II, III.a), "ai-prior" for question (III.b)
# by-block analyses: for questions (I, II, III.a), for question (III.b)
# code to produce Table 1 (main and auxiliary analyses)
# code to produce model outputs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

theme_set(theme_bw())

# main analyses ----
# 20 Bayesian mixed-effects beta-regressions on full data set, without block fixed effect
# one for each predicate

# read the data 
d <- read_csv("../data/d.csv")
nrow(d) #10100
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
  nRow = 505
  nRowMinusOne = 505-1
  write_csv(assign(paste("data.", p, sep=""), d %>% filter(short_trigger == p)) %>% 
                                                    mutate(cprior = prior-mean(prior), cai = ai-mean(ai)) %>%
                                                    mutate(betaProjective = (projective*nRowMinusOne + .5)/nRow) %>%
                                                    mutate(betaAI = (ai*nRowMinusOne + .5)/nRow),
            file=paste("../models/projection-main/data.",p,".csv",sep=""))
}


#tmp = read_csv("../models/projection-main/data.know.csv")
#summary(tmp)

# fit the models

##### questions (I, II, III.a) predict projection from prior, ai and interaction ----
betamodel = bf(betaProjective ~ cprior*cai + (1+cprior+cai|content),
               phi ~ cprior*cai + (1|content), # beta distribution's precision 
               family = Beta())

zoib_model <- bf(
  projective ~ cprior*cai + (1+cprior+cai|content),
  phi ~ cprior*cai + (1|content),
  zoi ~ cprior*cai + (1|content),
  coi ~ cprior*cai + (1|content), 
  family = zero_one_inflated_beta()
)


# experiment
p = "prove"

# fit the model for each predicate
for (p in predicates) {
  data.here = read_csv(paste("../models/projection-main/data.", p, ".csv",sep=""))
  # run beta model
  saveRDS(assign(paste("model.beta", p, ".rds", sep=""), brm(formula = betamodel,
                                                family=Beta(),
                                                data=data.here, 
                                                cores = 4, iter = 3000, warmup = 500,
                                                control = list(adapt_delta = .95,max_treedepth=15),
                                                save_pars = save_pars(all = TRUE))),
  file=paste("../models/projection-main/model.beta.",p, ".rds", sep=""))
  
  # run zoib model
  saveRDS(assign(paste("model.zoib.", p, ".rds", sep=""), brm(formula = zoib_model,
                                                             family=zero_one_inflated_beta(),
                                                             data=data.here, 
                                                             cores = 4, iter = 3000, warmup = 500,
                                                             control = list(adapt_delta = .95,max_treedepth=15),
                                                             save_pars = save_pars(all = TRUE))),
          file=paste("../models/projection-main/model.zoib",p, ".rds", sep=""))
}

# Model comparison loop
# which model does better -- beta or zoib?
for (p in predicates) {
  # load the two different models
  m.beta = readRDS(paste("../models/projection-main/model.beta.",p, ".rds", sep=""))
  m.zoib = readRDS(paste("../models/projection-main/model.zoib.",p, ".rds", sep=""))

  # compare models
  m.beta <- add_criterion(m.beta, criterion="loo", save_psis=TRUE)
  m.zoib <- add_criterion(m.zoib, criterion="loo", save_psis=TRUE)
loo_compare(m.beta,m.zoib) # the top model is the better performing one
  
  # posterior predictive checks
  pp_check(m.beta, type="hist", ndraws=5)
  ggsave(paste("../graphs/pp-checks/model.beta",p, ".pdf", sep=""))
  
  pp_check(m.zoib, type="hist", ndraws=5)
  ggsave(paste("../graphs/pp-checks/model.zoib",p, ".pdf", sep=""))
}



summary(m.beta)
summary(m.zoib)

waic.beta = waic(m.beta)
waic.zoib = waic(m.zoib)

loo_compare(waic.beta,waic.zoib) 

loo.beta = loo(m.beta) 
loo.zoib = loo(m.zoib)






loglik.beta = extract_log_lik(m.beta)

waic.beta = waic(m.beta)
waic.zoib = waic(m.zoib)

loo_compare(waic.beta,waic.zoib)




# tmp <- readRDS("../models/projection-main/model.think.rds")
# summary(tmp)

# question (II) predict ai from prior 
betamodel = bf(betaAI ~ cprior + (1+cprior|content),
               phi ~ cprior + (1|content), # beta distribution's precision 
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

# now evaluate the hypotheses of interest in each model
# create data frame to store the results of the evaluations

# questions (I, II, III.a)

hypotheses.fullData = data.frame(block = character(), predicate = character(), hypothesis = character(), estimate = numeric(), post.prob = numeric(), bf = numeric())
hypotheses.fullData

hypotheses = c("cprior > 0", "cai > 0", "cprior:cai > 0")
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

hypotheses = c("cprior < 0", "cai < 0", "cprior:cai < 0")
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

hypotheses = c("cprior > 0")
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

hypotheses = c("cprior < 0")
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
nrow(d) #10100
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
                                                            mutate(cprior = prior-mean(prior), cai = ai-mean(ai))),
  file=paste("../models/projection-byBlock/data_projai.",p,".csv",sep=""))
  write_csv(assign(paste("data_aiproj.", p, sep=""), d %>% filter(short_trigger == p & block_ai == "block1") %>% 
                                                          mutate(cprior = prior-mean(prior), cai = ai-mean(ai))),
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

betamodel = bf(betaProjective ~ cprior*cai + (1+cprior+cai|content),
               phi ~ cprior*cai + (1|content), # beta distribution's precision 
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

# tmp <- readRDS("../models/projection-byBlock/model_projai.acknowledge.rds")
# summary(tmp)

# fit the model: question (III.b) ----

betamodel = bf(betaAI ~ cprior + (1+cprior|content),
               phi ~ cprior + (1|content), # beta distribution's precision 
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

# tmp <- readRDS("../models/ai-prior-byBlock/model_projai.acknowledge.rds")
# summary(tmp)

# evaluate the hypotheses ----

##### questions (I, II, III.a) ----

hypotheses.by.block = data.frame(block = character(), predicate = character(), hypothesis = character(), estimate = numeric(), post.prob = numeric(), bf = numeric())
hypotheses.by.block

# hypotheses
hypotheses = c("cprior > 0", "cai > 0", "cprior:cai > 0")
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

hypotheses = c("cprior < 0", "cai < 0", "cprior:cai < 0")
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
hypotheses = c("cprior > 0")
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

hypotheses = c("cprior < 0")
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

# create data for Table 1a (based on Exp 1) ----

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
  mutate(hypothesis = case_when(hypothesisNEG == "cprior < 0" ~ "cprior > 0",
                                hypothesisNEG == "cai < 0" ~ "cai > 0",
                                hypothesisNEG == "cprior:cai < 0" ~ "cprior:cai > 0",
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

# to sort (rotated) predicates by projection mean
d <- read_csv("../data/d.csv")
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
  mutate(hypothesis = case_when(hypothesis == "cprior > 0" ~ "prior on proj",
                                hypothesis == "cai > 0" ~ "nai on proj",
                                hypothesis == "cprior:cai > 0" ~ "prior:nai on proj")) %>%
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
  mutate(hypothesis = case_when(hypothesisNEG == "cprior < 0" ~ "cprior > 0",
                                hypothesisNEG == "cai < 0" ~ "cai > 0",
                                hypothesisNEG == "cprior:cai < 0" ~ "cprior:cai > 0",
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

# to sort (rotated) predicates by projection mean
d <- read_csv("../data/d.csv")
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
  mutate(hypothesis = case_when(hypothesis == "cprior > 0" ~ "prior on nai")) %>%
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

# combine the results from the two analyses ----

# read data 
tableData1 = readRDS("../models/latex-tables/tableDataPROJ")
tableData2 = readRDS("../models/latex-tables/tableDataAIPRIOR")

tableData = rbind(tableData1, tableData2)

# create new first column (Effect)
effect <- c("{\\bf (I):} prior on", "\\hspace*{.7cm}projection", "", 
            "{\\bf (II):} not-at-issue on", "\\hspace*{.7cm} projection", "",
            "{\\bf (IIIa):} prior:not-at-issue", "\\hspace*{1.1cm} on projection", "",
            "{\\bf (IIIb):} prior on", "\\hspace*{1.1cm} not-at-issueness", "")


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
           hline.after = c(0,0,3,3,6,6,9,9,12)
           #hline.after = c(0,0,3,6,9,9,12,15,18,18,21,21,24)
)

write(t1, "../models/latex-tables/t1")

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
