# Exp1: interaction of prior beliefs, at-issueness and projection
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
nrow(d) #46800 = 600 participants x 78 trials (3x20 target + 3x6 filler/control)
head(d)
summary(d) 

length(unique(d$workerid)) #600

# count of how often each participant did the experiment
count = d %>%
  select(workerid) %>%
  group_by(workerid) %>%
  tally(sort=T)
#view(count) #nobody did it more than once

# read in the participant information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) #600

# look at participants' comments
unique(ds$comments)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid")) 
nrow(d) #46800

# age info (gender not collected)
# 3 people did not provide age info
names(d)
str(d$age)
min(d$age,na.rm=T) #18
max(d$age,na.rm=T) #73
mean(d$age,na.rm=T) #38.5
ggplot(d, aes(x=age)) +
  geom_histogram()

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) # slide numbers from 5 to 84
d$trial = d$slide_number_in_experiment - 4 # adds the column "trial"
unique(d$trial) # trial numbers from 1 to 80 (27 and 54 missing because instruction)
d[d$trial > 26,]$trial = d[d$trial > 26,]$trial - 1
unique(d$trial) # trials from 1 to 79 (53 missing because instruction)
d[d$trial > 52,]$trial = d[d$trial > 52,]$trial - 1
unique(d$trial) # trials from 1 to 78

# exclude non-American English speakers
length(unique(d$workerid)) #600
length(which(is.na(d$language))) #390 missing responses = 5 participants
table(d$language) 

# exclude anybody who didn't include English among languages spoken
d <- d %>%
  filter(!is.na(language)) %>%
  filter(language != "Spanish" & language != "Chinese" & 
           language != "Arabic" & language != "Female" & 
           language != "Turkish" & language != "Italian") %>% 
  droplevels()
length(unique(d$workerid)) #589 (data from 11 participants excluded)

# exclude non-American English speakers
length(unique(d$workerid)) #589
length(which(is.na(d$american))) #156 missing responses = 2 participants
table(d$american) # yes = 45552 / 78 = 584 participants

d <- d %>%
  filter(american == "Yes") %>%
  droplevels()
length(unique(d$workerid)) #584 (data from 5 participants excluded)

# exclude participants who always clicked on roughly the same point on the scale 
# ie participants whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(short_trigger != "MC") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 15 participants consistently clicked on roughly the same point on the scale

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

summary(lvw)
table(lvw$question_type)

# only one participant really wasn't doing the task (id = 0)
ggplot(lvw,aes(x=Participant,y=response)) +
  geom_point() +
  facet_grid(~ question_type)

# exclude the participant with ID 0
d <- droplevels(subset(d, d$workerid != "0"))
length(unique(d$workerid)) #583 participants remain

# exclude participants based on main clause controls

# main clauses
names(d)
table(d$question_type)
d.MC <- d %>%
  filter(short_trigger == "MC" & question_type != "prior") %>%
  droplevels()
nrow(d.MC) #6996 / 583 participants = 12 (6 main clause controls in projection and ai blocks)

# projection of main clause data
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #3498 / 583 participants = 6 main clause controls in projection block

# group projection mean 
round(mean(d.MC.Proj$response),2) #.21

# calculate each participants mean response to the projection of main clauses
p.means = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>% 
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High) # 

ggplot(p.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- d.MC %>%
  filter(question_type == "ai") %>%
  droplevels
nrow(d.MC.AI) #3498 / 583 participants = 6 main clause controls in ai block

# group not-at-issueness mean 
round(mean(d.MC.AI$response),2) #.09

# calculate each participants mean response to the ai of main clauses
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

# look at participants whose response mean on projection and ainess of main clauses is more than 2
# standard deviations away from the overall mean

p <- p.means[p.means$Mean > (mean(p.means$Mean) + 2*sd(p.means$Mean)),]
p # 32 participants 

ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 2*sd(ai.means$Mean)),]
ai # 48 participants

# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid) 
outliers = droplevels(outliers)
nrow(outliers) #936 / 12 = 78 participant

# plot outlier participants
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

# exclude all outlier participants identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>% 
  droplevels()
length(unique(d$workerid)) #505 participants (78 participants excluded)

# age info (for all remaining participants)
min(d$age) #20
max(d$age) #73
mean(d$age) #39.5
ggplot(d, aes(x=age)) +
  geom_histogram()

# number of data points
nrow(d[d$short_trigger != "MC",]) #30300

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
# block1 block2 
# 6474    6656

nrow(d) #13130

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
nrow(d[d$short_trigger != "MC",]) #10100

# reduce to target data
table(exp3$short_trigger)
d = d %>%
  filter(short_trigger != "MC")
nrow(d) # 10100

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

# save the three cleaned data sets 
write_csv(d, file="../data/d.csv")
nrow(d) #10100

write_csv(d_aiproj, file="../data/d_aiproj.csv")
nrow(d_aiproj) #4920

write_csv(d_projai, file="../data/d_projai.csv")
nrow(d_projai) #5120

