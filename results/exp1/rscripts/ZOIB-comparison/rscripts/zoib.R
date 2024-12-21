# Exp1: interaction of prior beliefs, at-issueness and projection
# for 20 clause-embedding predicates
# zoib.R
# to compare beta models with ZOIB models

# load required packages
library(tidyverse)
library(optimx)
library(xtable)
library(texreg) #to output models in latex 
library(forcats)
library(brms)
library(tidybayes)
library(brmsmargins)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

theme_set(theme_bw())

# main analyses ----
# 20 Bayesian mixed-effects beta-regressions on full data set, without block fixed effect
# one for each predicate

# read the data 
d <- read_csv("../../../data/d.csv")
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

# create a separate data file for each predicate
for (p in predicates) {
  nRow = 505
  nRowMinusOne = 505-1
  write_csv(assign(paste("data.", p, sep=""), d %>% filter(short_trigger == p)) %>% 
                                                    mutate(cprior = prior-mean(prior), cai = ai-mean(ai)) %>%
                                                    mutate(betaProjective = (projective*nRowMinusOne + .5)/nRow) %>%
                                                    mutate(betaAI = (ai*nRowMinusOne + .5)/nRow),
            file=paste("../models/data.",p,".csv",sep=""))
}

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


# fit the model for each predicate
for (p in predicates) {
  data.here = read_csv(paste("../models/data.", p, ".csv",sep=""))
  # run beta model
  saveRDS(assign(paste("model.beta", p, ".rds", sep=""), brm(formula = betamodel,
                                                family=Beta(),
                                                data=data.here, 
                                                cores = 4, iter = 3000, warmup = 500,
                                                control = list(adapt_delta = .95,max_treedepth=15),
                                                save_pars = save_pars(all = TRUE))),
  file=paste("../models/model.beta.",p, ".rds", sep=""))
  
  # run zoib model
  saveRDS(assign(paste("model.zoib.", p, ".rds", sep=""), brm(formula = zoib_model,
                                                             family=zero_one_inflated_beta(),
                                                             data=data.here, 
                                                             cores = 4, iter = 3000, warmup = 500,
                                                             control = list(adapt_delta = .95,max_treedepth=15),
                                                             save_pars = save_pars(all = TRUE))),
          file=paste("../models/model.zoib",p, ".rds", sep=""))
}

# model comparison loop
# which model does better -- beta or zoib?

comparison = data.frame(predicate = character(), winning.model = character(), elpd_diff = numeric(),
                se_diff = numeric())
comparison

for (p in predicates) {
  # load the two different models
  m.beta = readRDS(paste("../models/model.beta.",p, ".rds", sep=""))
  m.zoib = readRDS(paste("../models/model.zoib",p, ".rds", sep=""))

  # compare models
  m.beta <- add_criterion(m.beta, criterion="loo", save_psis=TRUE)
  m.zoib <- add_criterion(m.zoib, criterion="loo", save_psis=TRUE)
  
  print(p)
  print(loo_compare(m.beta,m.zoib)) # the top model is the better performing one
  comparison_tmp = rownames_to_column(as.data.frame(loo_compare(m.beta, m.zoib)),var = "model")
  comparison = comparison %>%
      add_row(predicate = p, winning.model = comparison_tmp[1,1], elpd_diff = abs(round(comparison_tmp[2,2],0)), 
              se_diff = abs(round(comparison_tmp[2,3],0)))
  
  # posterior predictive checks
  pp_check(m.beta, type="hist", ndraws=5)
  ggsave(paste("../graphs/model.beta.",p, ".pdf", sep=""))
  
  pp_check(m.zoib, type="hist", ndraws=5)
  ggsave(paste("../graphs/model.zoib.",p, ".pdf", sep=""))
}

# this file records the winning model and the ELPD difference between the winning and loosing models
write_csv(comparison, file="../comparison.csv")
min(comparison$elpd_diff) #224
max(comparison$elpd_diff) #625
