# ZOIB!!
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

# PLEASE DONT WRITE INTO MY COMPUTER!

for (p in predicates) {
  nRow = 505
  nRowMinusOne = 505-1
  write_csv(assign(paste("data.", p, sep=""), d %>% filter(short_trigger == p)) %>% 
                                                    mutate(cprior = prior-mean(prior), cai = ai-mean(ai)) %>%
                                                    mutate(betaProjective = (projective*nRowMinusOne + .5)/nRow) %>%
                                                    mutate(betaAI = (ai*nRowMinusOne + .5)/nRow),
            file=paste("../models-ZOIB/projection-main/data.",p,".csv",sep=""))
}


#tmp = read_csv("../models/projection-main/data.know.csv")
#summary(tmp)

# fit the models

##### questions (I, II, III.a) predict projection from prior, ai and interaction ----
betamodel = bf(betaProjective ~ cprior*cai + (1+cprior+cai|content),
               phi ~ cprior*cai + (1|content), # beta distribution's precision 
               family = Beta())

# fit the model for each predicate
for (p in predicates) {
  data.here = read_csv(paste("../models-ZOIB/projection-main/data.", p, ".csv",sep=""))
  saveRDS(assign(paste("model.", p, ".rds", sep=""), brm(formula = betamodel,
                                                family=Beta(),
                                                data=data.here, 
                                                cores = 4, iter = 3000, warmup = 500,
                                                control = list(adapt_delta = .95,max_treedepth=15))),
  file=paste("../models-ZOIB/projection-main/model.",p, ".rds", sep=""))
}

# tmp <- readRDS("../models/projection-main/model.think.rds")
# summary(tmp)

