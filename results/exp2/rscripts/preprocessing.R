# Exp2: at-issueness/projection, with prior manipulated but not measured
# for 20 clause-embedding predicates
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
library(zoo)

theme_set(theme_bw()) 

# read in the raw data
d = read_csv("../data/experiment-trials.csv") 
nrow(d) #31200 = 600 participants x 52 trials (2x20 target + 2x6 control)
head(d)
summary(d) 

length(unique(d$workerid)) #600

# count of how often each Turker did the experiment
count = d %>%
  select(workerid) %>%
  group_by(workerid) %>%
  tally(sort=T)
#view(count) #nobody did it more than once

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) #600

# look at Turkers' comments
unique(ds$comments)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid")) 
nrow(d) #31200

# age info (gender not collected)
names(d)
table(d$age)
min(d$age,na.rm=T) #19
max(d$age,na.rm=T) #73
mean(d$age,na.rm=T) #37.5
ggplot(d, aes(x=age)) +
  geom_histogram()

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) # slide numbers from 5 to 57
d$trial = d$slide_number_in_experiment - 4 # adds the column "trial"
unique(d$trial) # trial numbers from 1 to 53 (27  missing because instruction)
d[d$trial > 26,]$trial = d[d$trial > 26,]$trial - 1
unique(d$trial) # trials from 1 to 52

# exclude non-American English speakers
length(unique(d$workerid)) #600
length(which(is.na(d$language))) #260 missing responses = 5 Turkers
table(d$language) 

# exclude anybody who didn't include English among languages spoken
d <- d %>%
  filter(!is.na(language)) %>%
  filter(language != "Kannada" & language != "Spanish" & 
           language != "Tamil" & language != "United States" & 
           language != "Bulgarian" & language != "Italian" & language != "Hindi"
         & language != "khmer" & language != "Polish" & language != "Telugu"
         & language != "Vietnamese" & language != "Nepali") %>% 
  droplevels()
length(unique(d$workerid)) #581 (data from 19 Turkers excluded)

# exclude non-American English speakers
length(unique(d$workerid)) #581
length(which(is.na(d$american))) #104 missing responses = 2 Turkers
table(d$american) 

d <- d %>%
  filter(american == "Yes") %>%
  droplevels()
length(unique(d$workerid)) #573 (data from 8 Turkers excluded)

# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(short_trigger != "MC") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 7 turkers

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

summary(lvw)
table(lvw$question_type)

ggplot(lvw,aes(x=Participant,y=response)) +
  geom_point() +
  facet_grid(~ question_type)

# exclude the Turker with ID 1, 100, 295, 301, 241
d <- droplevels(subset(d, d$workerid != "1" & d$workerid != "100" & d$workerid != "295" &
                         d$workerid != "301" & d$workerid != "241"))
length(unique(d$workerid)) #568 Turkers remain (5 excluded)

# exclude Turkers based on main clause controls

# main clauses
names(d)
table(d$question_type)
d.MC <- d %>%
  filter(short_trigger == "MC" & question_type != "prior") %>%
  droplevels()
nrow(d.MC) #6816 / 568 Turkers = 12 (6 main clause controls in projection and ai blocks)

# projection of main clause data
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #3408 / 568 Turkers = 6 main clause controls in projection block

# group projection mean 
round(mean(d.MC.Proj$response),2) #.13

# calculate each Turkers mean response to the projection of main clauses
p.means = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>% 
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High) 

ggplot(p.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- d.MC %>%
  filter(question_type == "ai") %>%
  droplevels
nrow(d.MC.AI) #3408

# group not-at-issueness mean 
round(mean(d.MC.AI$response),2) #.06

# calculate each Turkers mean response to the ai of main clauses
ai.means = d.MC.AI %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High) 

ggplot(ai.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# look at Turkers whose response mean on projection and ainess of main clauses is more than 2
# standard deviations away from the overall mean

p <- p.means[p.means$Mean > (mean(p.means$Mean) + 2*sd(p.means$Mean)),]
p # 44 Turkers 

ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 2*sd(ai.means$Mean)),]
ai # 34 Turkers

# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid) 
outliers = droplevels(outliers)
nrow(outliers) #720 / 12 = 60 Turkers

# plot outlier Turkers
ggplot(p, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("projection response mean")

ggplot(ai, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("ai response mean")

# exclude all outlier Turkers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>% 
  droplevels()
length(unique(d$workerid)) #508 Turkers (60 Turkers excluded)

# age info for all remaining Turkers
summary(d$age) #19-73, mean: 38.3
table(d$age)
ggplot(d, aes(x=age)) +
  geom_histogram()

# number of data points
nrow(d[d$short_trigger != "MC",]) #20320

# prepare for spreading:
# (1) rename the "prior" column into "prior_type" and
colnames(d)[colnames(d)=="prior"] = "prior_type"
# (2) fill in (arbitrarily) 'high_prior' for the "prior_type" of main clauses (MC)
d[d$short_trigger == "MC",]$prior_type <- "main_clause"

# spread responses over separate columns for prior probability, projectivity and at-issueness
d = d %>%
  mutate(block_ai = ifelse(question_type=="ai"&block=="block1", "block1",
                           ifelse(question_type=="ai"&block=="block2", "block2",
                                  ifelse(question_type=="projective"&block=="block1", "block2",
                                         ifelse(question_type=="projective"&block=="block2", "block1", NA))))) %>%
  na.locf(fromLast = TRUE) %>%   # replaces NA with the nearest non-NA; fromLast causes observations to be carried backward 
  select(content,question_type,short_trigger,response,workerid,prior_type,prior_fact,block_ai) %>% 
  spread(question_type,response) %>%
  unite(item,short_trigger,content,remove=F)

table(d$block_ai) 

nrow(d) #13000

table(d$short_trigger)

# fix predicate names
d = d %>%
  mutate(short_trigger=recode(short_trigger, control = "MC", be_annoyed = "be annoyed", be_right = "be right"))

# reconstruct 'eventItemNr' using 'prior_fact'
d = d %>%
  mutate(eventItemNr = ifelse(prior_fact == "Charley lives in Mexico" | prior_fact == "Charley lives in Korea", "20: Charley speaks Spanish",
                              ifelse(prior_fact == "Danny is a diabetic" | prior_fact == "Danny loves cake", "11: Danny ate the last cupcake",
                                     ifelse(prior_fact == "Emily has been saving for a year" | prior_fact == "Emily never has any money", "8: Emily bought a car yesterday",
                                            ifelse(prior_fact == "Emma is in first grade" | prior_fact == "Emma is in law school", "3: Emma studied on Saturday morning",
                                                   ifelse(prior_fact == "Frank has always wanted a pet" | prior_fact == "Frank is allergic to cats", "12: Frank got a cat",
                                                          ifelse(prior_fact == "Grace hates her sister" | prior_fact == "Grace loves her sister", "9: Grace visited her sister",
                                                                 ifelse(prior_fact == "Isabella is a vegetarian" | prior_fact == "Isabella is from Argentina", "7: Isabella ate a steak on Sunday",
                                                                        ifelse(prior_fact == "Jackson is obese" | prior_fact == "Jackson is training for a marathon", "13: Jackson ran 10 miles",
                                                                               ifelse(prior_fact == "Jayden's car is in the shop" | prior_fact == "Jayden doesn't have a driver's license", "14: Jayden rented a car",
                                                                                      ifelse(prior_fact == "Jon lives 10 miles away from work" | prior_fact == "Jon lives 2 blocks away from work", "19: Jon walks to work",
                                                                                             ifelse(prior_fact == "Josh is a 5-year old boy" | prior_fact == "Josh is a 75-year old man", "16: Josh learned to ride a bike yesterday",
                                                                                                    ifelse(prior_fact == "Josie doesn't have a passport" | prior_fact == "Josie loves France", "2: Josie went on vacation to France",
                                                                                                           ifelse(prior_fact == "Julian is Cuban" | prior_fact == "Julian is German", "18: Julian dances salsa",
                                                                                                                  ifelse(prior_fact == "Mary is a middle school student" | prior_fact == "Mary is taking a prenatal yoga class", "1: Mary is pregnant",
                                                                                                                         ifelse(prior_fact == "Mia is a college student" | prior_fact == "Mia is a nun", "6: Mia drank 2 cocktails last night",
                                                                                                                                ifelse(prior_fact == "Olivia has two small children" | prior_fact == "Olivia works the third shift", "4: Olivia sleeps until noon",
                                                                                                                                       ifelse(prior_fact == "Owen lives in Chicago" | prior_fact == "Owen lives in New Orleans", "17: Owen shoveled snow last winter",
                                                                                                                                              ifelse(prior_fact == "Sophia is a high end fashion model" | prior_fact == "Sophia is a hipster", "5: Sophia got a tattoo",
                                                                                                                                                     ifelse(prior_fact == "Tony has been sober for 20 years" | prior_fact == "Tony really likes to party with his friends", "15: Tony had a drink last night",
                                                                                                                                                            ifelse(prior_fact == "Zoe is 5 years old" | prior_fact == "Zoe is a math major", "10: Zoe calculated the tip",
                                                                                                                                                                   NA)))))))))))))))))))))

# reconstruct 'eventItem' using 'prior_fact'
d = d %>%
  mutate(eventItem = ifelse(prior_fact == "Charley lives in Mexico" | prior_fact == "Charley lives in Korea", "Charley speaks Spanish",
                            ifelse(prior_fact == "Danny is a diabetic" | prior_fact == "Danny loves cake", "Danny ate the last cupcake",
                                   ifelse(prior_fact == "Emily has been saving for a year" | prior_fact == "Emily never has any money", "Emily bought a car yesterday",
                                          ifelse(prior_fact == "Emma is in first grade" | prior_fact == "Emma is in law school", "Emma studied on Saturday morning",
                                                 ifelse(prior_fact == "Frank has always wanted a pet" | prior_fact == "Frank is allergic to cats", "Frank got a cat",
                                                        ifelse(prior_fact == "Grace hates her sister" | prior_fact == "Grace loves her sister", "Grace visited her sister",
                                                               ifelse(prior_fact == "Isabella is a vegetarian" | prior_fact == "Isabella is from Argentina", "Isabella ate a steak on Sunday",
                                                                      ifelse(prior_fact == "Jackson is obese" | prior_fact == "Jackson is training for a marathon", "Jackson ran 10 miles",
                                                                             ifelse(prior_fact == "Jayden's car is in the shop" | prior_fact == "Jayden doesn't have a driver's license", "Jayden rented a car",
                                                                                    ifelse(prior_fact == "Jon lives 10 miles away from work" | prior_fact == "Jon lives 2 blocks away from work", "Jon walks to work",
                                                                                           ifelse(prior_fact == "Josh is a 5-year old boy" | prior_fact == "Josh is a 75-year old man", "Josh learned to ride a bike yesterday",
                                                                                                  ifelse(prior_fact == "Josie doesn't have a passport" | prior_fact == "Josie loves France", "Josie went on vacation to France",
                                                                                                         ifelse(prior_fact == "Julian is Cuban" | prior_fact == "Julian is German", "Julian dances salsa",
                                                                                                                ifelse(prior_fact == "Mary is a middle school student" | prior_fact == "Mary is taking a prenatal yoga class", "Mary is pregnant",
                                                                                                                       ifelse(prior_fact == "Mia is a college student" | prior_fact == "Mia is a nun", "Mia drank 2 cocktails last night",
                                                                                                                              ifelse(prior_fact == "Olivia has two small children" | prior_fact == "Olivia works the third shift", "Olivia sleeps until noon",
                                                                                                                                     ifelse(prior_fact == "Owen lives in Chicago" | prior_fact == "Owen lives in New Orleans", "Owen shoveled snow last winter",
                                                                                                                                            ifelse(prior_fact == "Sophia is a high end fashion model" | prior_fact == "Sophia is a hipster", "Sophia got a tattoo",
                                                                                                                                                   ifelse(prior_fact == "Tony has been sober for 20 years" | prior_fact == "Tony really likes to party with his friends", "Tony had a drink last night",
                                                                                                                                                          ifelse(prior_fact == "Zoe is 5 years old" | prior_fact == "Zoe is a math major", "Zoe calculated the tip",
                                                                                                                                                                 NA)))))))))))))))))))))


# number of data points
nrow(d[d$short_trigger != "MC",]) #10000

# reduce to target data
d = d %>%
  filter(short_trigger != "MC")
nrow(d) #10000

# create contentFact
d$contentNew <- d$content
d$contentNew <- gsub("zoe", "Zoe calculated the tip", d$contentNew) 
d$contentNew <- gsub("danny", "Danny ate the last cupcake", d$contentNew)
d$contentNew <- gsub("frank", "Frank got a cat", d$contentNew)
d$contentNew <- gsub("jackson", "Jackson ran 10 miles",  d$contentNew)
d$contentNew <- gsub("jayden", "Jayden rented a car", d$contentNew)
d$contentNew <- gsub("tony", "Tony had a drink last night", d$contentNew)
d$contentNew <- gsub("josh", "Josh learned to ride a bike yesterday", d$contentNew)        
d$contentNew <- gsub("owen", "Owen shoveled snow last winter", d$contentNew)       
d$contentNew <- gsub("julian",  "Julian dances salsa", d$contentNew)
d$contentNew <- gsub("jon", "Jon walks to work", d$contentNew)                    
d$contentNew <- gsub("charley", "Charley speaks Spanish", d$contentNew)
d$contentNew <- gsub("josie",  "Josie went on vacation to France", d$contentNew)
d$contentNew <- gsub("mia", "Mia drank 2 cocktails last night", d$contentNew)
d$contentNew <- gsub("mary", "Mary is pregnant", d$contentNew)
d$contentNew <- gsub("emma", "Emma studied on Saturday morning", d$contentNew)  
d$contentNew <- gsub("olivia", "Olivia sleeps until noon", d$contentNew)
d$contentNew <- gsub("sophia", "Sophia got a tattoo", d$contentNew)
d$contentNew <- gsub("isabella", "Isabella ate a steak on Sunday", d$contentNew)
d$contentNew <- gsub("emily", "Emily bought a car yesterday", d$contentNew)
d$contentNew <- gsub("grace",  "Grace visited her sister", d$contentNew)
table(d$contentNew)

d$prior_typeNew <- d$prior_type
d$prior_typeNew <- gsub("high_prior", "factH", d$prior_typeNew)
d$prior_typeNew <- gsub("low_prior", "factL", d$prior_typeNew)
table(d$prior_typeNew)

# create contentFact
d$contentFact <- paste(d$contentNew, d$prior_typeNew, sep = "-")
table(d$contentFact)

# create subsets by whether ai-block first (after prior block) or second
table(d$block_ai)
d_aiproj = d %>%
  filter(block_ai == "block1")
d_projai = d %>%
  filter(block_ai == "block2")

# add prior data ----

# load data from which prior data will be extracted

# exp 1 of current paper
exp1 <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/attitude_preds_projection/master/results/main/exp1/data/d.csv")
nrow(exp1) #10100

# exp1 of Open Mind paper (Degen & Tonhauser 2021)
om_exp1 <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/9-prior-projection/data/cd.csv")
nrow(om_exp1) #7436

# exp2a of Open Mind paper
om_exp2a <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/1-prior/data/cd.csv")
nrow(om_exp2a) #1650

# remove controls from Open Mind paper exp1 and exp2a
om_exp1 = om_exp1 %>%
  filter(short_trigger != "MC")
nrow(om_exp1) #5720

om_exp2a = om_exp2a %>%
  filter(item != "F1" & item != "F2")
nrow(om_exp2a) #1500

# create contentFact column for Open Mind paper exp1 and exp2a
table(exp1$contentFact) # Zoe calculated the tip-factL

table(om_exp1$eventItem) # these are the contents
table(om_exp1$prior_type) # high_prior low_prior
om_exp1 = om_exp1 %>%
  mutate(factNew = case_when(prior_type == "high_prior" ~ "factH",
                            prior_type == "low_prior" ~ "factL",
                            TRUE ~ "ERROR")) %>%
  mutate(contentFact = paste(eventItem, factNew, sep = "-"))

# create contentFact
om_exp2a$prior_typeNew <- om_exp2a$itemType
om_exp2a$prior_typeNew <- gsub("H", "factH",om_exp2a$prior_typeNew)
om_exp2a$prior_typeNew <- gsub("L", "factL",om_exp2a$prior_typeNew)
table(om_exp2a$prior_typeNew)

# create content column
om_exp2a$content <- om_exp2a$item
om_exp2a$content <- gsub("H", "", om_exp2a$content)
om_exp2a$content <- gsub("L", "", om_exp2a$content)
om_exp2a$content <- gsub("10", "Zoe calculated the tip", om_exp2a$content) 
om_exp2a$content <- gsub("11", "Danny ate the last cupcake", om_exp2a$content)
om_exp2a$content <- gsub("12", "Frank got a cat", om_exp2a$content)
om_exp2a$content <- gsub("13", "Jackson ran ten miles",  om_exp2a$content)
om_exp2a$content <- gsub("14", "Jayden rented a car", om_exp2a$content)
om_exp2a$content <- gsub("15", "Tony had a drink last night", om_exp2a$content)
om_exp2a$content <- gsub("16", "Josh learned to ride a bike yesterday", om_exp2a$content)        
om_exp2a$content <- gsub("17", "Owen shoveled snow last winter", om_exp2a$content)       
om_exp2a$content <- gsub("18",  "Julian dances salsa", om_exp2a$content)
om_exp2a$content <- gsub("19", "Jon walks to work", om_exp2a$content)                    
om_exp2a$content <- gsub("20", "Charley speaks Spanish", om_exp2a$content)
om_exp2a$content <- gsub("2",  "Josie went on vacation to France", om_exp2a$content)
om_exp2a$content <- gsub("6", "Mia drank 2 cocktails last night", om_exp2a$content)
om_exp2a$content <- gsub("1", "Mary is pregnant", om_exp2a$content)
om_exp2a$content <- gsub("3", "Emma studied on Saturday morning", om_exp2a$content)  
om_exp2a$content <- gsub("4", "Olivia sleeps until noon", om_exp2a$content)
om_exp2a$content <- gsub("5", "Sophia got a tattoo", om_exp2a$content)
om_exp2a$content <- gsub("7", "Isabella ate a steak on Sunday", om_exp2a$content)
om_exp2a$content <- gsub("8", "Emily bought a car yesterday", om_exp2a$content)
om_exp2a$content <- gsub("9",  "Grace visited her sister", om_exp2a$content)
om_exp2a$content <- gsub("Jackson ran ten miles", "Jackson ran 10 miles",  om_exp2a$content)
table(om_exp2a$content)

om_exp2a$contentFact <- paste(om_exp2a$content, om_exp2a$prior_typeNew, sep = "-")
table(om_exp2a$contentFact)

# select relevant columns from each of the three data sets
tmp1 <- exp1 %>%
  select(prior,contentFact)
nrow(tmp1) #10100
tmp_om1 <- om_exp1 %>%
  select(prior,contentFact)
nrow(tmp_om1) #5720
tmp_om2a <- om_exp2a %>%
  select(response,contentFact) %>%
  rename("prior" = "response")
nrow(tmp_om2a) #1500

# combine the data
tmp.prior <- rbind(tmp1,tmp_om1,tmp_om2a)
nrow(tmp.prior) #17320
summary(tmp.prior)
length(unique(tmp.prior$contentFact)) #40

# calculate pooled prior means
tmp.prior.means = tmp.prior %>%
  group_by(contentFact) %>%
  summarize(Mean_prior = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior=Mean_prior-CILow,YMax_prior=Mean_prior+CIHigh) %>%
  select(!c(CILow,CIHigh))
nrow(tmp.prior.means) #40
summary(tmp.prior.means)
head(tmp.prior.means)

# add pooled prior means to exp2 datasets
d <- left_join(d, tmp.prior.means, by = c("contentFact" = "contentFact"))
d_aiproj <- left_join(d_aiproj, tmp.prior.means, by = c("contentFact" = "contentFact"))
d_projai <- left_join(d_projai, tmp.prior.means, by = c("contentFact" = "contentFact"))

# save the three cleaned data sets 
write_csv(d, file="../data/d.csv")
nrow(d) #10000

write_csv(d_aiproj, file="../data/d_aiproj.csv")
nrow(d_aiproj) #4820

write_csv(d_projai, file="../data/d_projai.csv")
nrow(d_projai) #5180

