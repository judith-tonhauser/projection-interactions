# Exp1: interaction of prior beliefs, at-issueness and projection
# for 20 clause-embedding predicates
# graphs.R

# load required packages
library(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(RColorBrewer)
library(curl) # to read data from github repo
library(gridExtra)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

theme_set(theme_bw()) 

source('../../helpers.R')

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load the data
d <- read_csv("../data/d.csv")
nrow(d) #10100

# Fig.2: by-predicate correlations in the raw data ----

# sort predicates by projection mean
proj.means = d %>%
  group_by(short_trigger) %>%
  summarize(Mean_proj = mean(projective)) %>%
  mutate(short_trigger = fct_rev(fct_reorder(as.factor(short_trigger),Mean_proj)))
proj.means

d = d %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(proj.means$short_trigger)))
levels(d$short_trigger)

# color-code the predicates
d = d %>%
  mutate(predicateType = case_when(short_trigger == "discover" ~ "factive",
                                   short_trigger == "know" ~ "factive",
                                   short_trigger == "be annoyed" ~ "factive",
                                   short_trigger == "reveal" ~ "factive",
                                   short_trigger == "see" ~ "factive",
                                   TRUE ~ "nonfactive"))
table(d$short_trigger,d$predicateType)

# projection by prior
ggplot(d, aes(x=prior, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/projection-by-prior.pdf",height=5,width=5)

# projection by at-issueness
ggplot(d, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill = predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/projection-by-ai.pdf",height=5,width=5)

# projection by at-issueness and prior
table(d$prior_type)
ggplot(d, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType,linetype = prior_type),method="lm") +
  scale_linetype_manual(name = "Prior probability", 
                        labels = c("high", "low"),
                        values=c("solid", "dotted")) +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  guides(color = "none", linetype = "none") +
  #guides(linetype = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.key = element_rect(fill = "white"), legend.position = "right") +
  theme(legend.background = element_rect(fill ="white")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/projection-by-ai-and-prior.pdf",height=5,width=5)

# # at-issueness by prior
ggplot(d, aes(x=prior, y=ai)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/ai-by-prior.pdf",height=5,width=5)

# Supplemental figures ----

## Supplement C: Manipulation of prior beliefs ----
names(d)
table(d$eventItem)
table(d$prior_type)

means = d %>%
  group_by(eventItem,prior_type) %>%
  summarise(Mean=mean(prior),CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means

high = means %>%
  filter(prior_type == "high_prior") %>%
  mutate(eventItem = fct_reorder(eventItem,Mean))

means = means %>%
  mutate(eventItem = fct_relevel(eventItem,levels(high$eventItem))) %>% 
  mutate(prior_type = fct_relevel(prior_type,"low_prior"))
means

subjmeans = d %>%
  group_by(eventItem,workerid,prior_type) %>%
  summarize(Mean = mean(prior)) %>%
  ungroup() %>% 
  mutate(prior_type = fct_relevel(as.factor(as.character(prior_type)),"low_prior"))
subjmeans$eventItem <- factor(subjmeans$eventItem, levels = unique(levels(means$eventItem)))
levels(subjmeans$eventItem)
names(subjmeans)

ggplot(means, aes(x=eventItem, y=Mean, color=prior_type, shape=prior_type, fill=prior_type)) + 
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"), 
                     values=c("#56B4E9","#E69F00")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  coord_flip() +
  ylab("Mean prior probability rating") +
  xlab("Content") 
ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)

## Supplement D: By-block by-predicate correlations ----

# load the data
d_projai <- read_csv("../data/d_projai.csv")
nrow(d_projai) #5120
d_aiproj <- read_csv("../data/d_aiproj.csv")
nrow(d_aiproj) #4980

# sort predicates by projection mean
proj.means = d %>%
  group_by(short_trigger) %>%
  summarize(Mean_proj = mean(projective)) %>%
  mutate(short_trigger = fct_rev(fct_reorder(as.factor(short_trigger),Mean_proj)))
proj.means

d_projai = d_projai %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(proj.means$short_trigger)))
levels(d_projai$short_trigger)

d_aiproj = d_aiproj %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(proj.means$short_trigger)))
levels(d_aiproj$short_trigger)

# color-code the predicates
d_projai = d_projai %>%
  mutate(predicateType = case_when(short_trigger == "discover" ~ "factive",
                                   short_trigger == "know" ~ "factive",
                                   short_trigger == "be annoyed" ~ "factive",
                                   short_trigger == "reveal" ~ "factive",
                                   short_trigger == "see" ~ "factive",
                                   TRUE ~ "nonfactive"))

d_aiproj = d_aiproj %>%
  mutate(predicateType = case_when(short_trigger == "discover" ~ "factive",
                                   short_trigger == "know" ~ "factive",
                                   short_trigger == "be annoyed" ~ "factive",
                                   short_trigger == "reveal" ~ "factive",
                                   short_trigger == "see" ~ "factive",
                                   TRUE ~ "nonfactive"))

# proj/ai

# projection by prior
ggplot(d_projai, aes(x=prior, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/SUP-projai-projection-by-prior.pdf",height=5,width=5)

# projection by at-issueness
ggplot(d_projai, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill = predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/SUP-projai-projection-by-ai.pdf",height=5,width=5)

# projection by at-issueness and prior
ggplot(d_projai, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType,linetype = prior_type),method="lm") +
  scale_linetype_manual(name = "Prior probability", 
                        labels = c("high", "low"),
                        values=c("solid", "dotted")) +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  guides(color = "none", linetype = "none") +
  #guides(linetype = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.key = element_rect(fill = "white"), legend.position = "right") +
  theme(legend.background = element_rect(fill ="white")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/SUP-projai-projection-by-ai-and-prior.pdf",height=5,width=5)

# # at-issueness by prior
ggplot(d_projai, aes(x=prior, y=ai)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/SUP-projai-ai-by-prior.pdf",height=5,width=5)

# ai/proj

# projection by prior
ggplot(d_aiproj, aes(x=prior, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/SUP-aiproj-projection-by-prior.pdf",height=5,width=5)

# projection by at-issueness
ggplot(d_aiproj, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill = predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/SUP-aiproj-projection-by-ai.pdf",height=5,width=5)

# projection by at-issueness and prior
ggplot(d_aiproj, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType,linetype = prior_type),method="lm") +
  scale_linetype_manual(name = "Prior probability", 
                        labels = c("high", "low"),
                        values=c("solid", "dotted")) +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  guides(color = "none", linetype = "none") +
  #guides(linetype = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.key = element_rect(fill = "white"), legend.position = "right") +
  theme(legend.background = element_rect(fill ="white")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/SUP-aiproj-projection-by-ai-and-prior.pdf",height=5,width=5)

# # at-issueness by prior
ggplot(d_aiproj, aes(x=prior, y=ai)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/SUP-aiproj-ai-by-prior.pdf",height=5,width=5)

## Supplement F: Cross-experiment comparison of ratings ----

#### prior (Exp 1 vs Open Mind Exps 1 and 2) ----

# load the relevant data
exp1 <- read_csv("../data/d.csv")
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
exp1 <- exp1 %>%
  select(prior,contentFact)
nrow(exp1) #10100
om_exp1 <- om_exp1 %>%
  select(prior,contentFact)
nrow(om_exp1) #5720
om_exp2a <- om_exp2a %>%
  select(response,contentFact) %>%
  rename("prior" = "response")
nrow(om_exp2a) #1500

# calculate by-contentFact prior indicates in each dataset, label indicates differently

exp1.means <- exp1 %>%
  group_by(contentFact) %>%
  summarize(Mean_prior_Exp1 = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior_Exp1=Mean_prior_Exp1-CILow,YMax_prior_Exp1=Mean_prior_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.means)
nrow(exp1.means) #40

om_exp1.means <- om_exp1 %>%
  group_by(contentFact) %>%
  summarize(Mean_prior_OMExp1 = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior_OMExp1=Mean_prior_OMExp1-CILow,YMax_prior_OMExp1=Mean_prior_OMExp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(om_exp1.means)
nrow(om_exp1.means) #40

om_exp2a.means <- om_exp2a %>%
  group_by(contentFact) %>%
  summarize(Mean_prior_OMExp2a = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior_OMExp2a=Mean_prior_OMExp2a-CILow,YMax_prior_OMExp2a=Mean_prior_OMExp2a+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(om_exp2a.means)
nrow(om_exp2a.means) #40

# combine the data
prior.means <- left_join(exp1.means, om_exp1.means, by = c("contentFact")) %>%
  left_join(., om_exp2a.means, by = c("contentFact"))
nrow(prior.means) #40
summary(prior.means)

# Spearman rank correlations

cor.test(prior.means$Mean_prior_Exp1, prior.means$Mean_prior_OMExp1, method=c("spearman"))
# .99
cor.test(prior.means$Mean_prior_Exp1, prior.means$Mean_prior_OMExp2a, method=c("spearman"))
# .99
cor.test(prior.means$Mean_prior_OMExp1, prior.means$Mean_prior_OMExp2a, method=c("spearman"))
# .98

# how many judgments? 
count = exp1 %>%
  group_by(contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#224.0   247.8   252.5   252.5   257.2   281.0

count = om_exp1 %>%
  group_by(contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#128     136     143     143     150     158

count = om_exp2a %>%
  group_by(contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#36.0    36.0    37.5    37.5    39.0    39.0 

# plots 

# exp1 vs om exp1
ggplot(prior.means, aes(x=Mean_prior_Exp1, y=Mean_prior_OMExp1)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Prior probability rating Exp. 1") +
  ylab("Prior probability rating OM Exp. 1") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .99")
ggsave(f="../graphs/SUP-priorExp1-by-priorOMExp1.pdf",height=3,width=3)

# exp1 vs om exp2a
ggplot(prior.means, aes(x=Mean_prior_Exp1, y=Mean_prior_OMExp2a)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Prior probability rating Exp. 1") +
  ylab("Prior probability rating OM Exp. 2a") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .99")
ggsave(f="../graphs/SUP-priorExp1-by-priorOMExp2a.pdf",height=3,width=3)

# exp om 1 vs om exp2a
ggplot(prior.means, aes(x=Mean_prior_OMExp1, y=Mean_prior_OMExp2a)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Prior probability rating OM Exp. 1") +
  ylab("Prior probability rating OM Exp. 2a") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .98")
ggsave(f="../graphs/SUP-priorOMExp1-by-priorOMExp2a.pdf",height=3,width=3)


#### certainty (Exp 1 vs Exp 2) ----

# load the data
exp1 <- read_csv("../data/d.csv")
nrow(exp1) #10100

exp2 <- read_csv("../../exp2/data/d.csv")
nrow(exp2) #10000

# calculate certainty means: by-predicate, by-predicate/item, by-predicate/item/fact

exp1.Pred.means <- exp1 %>%
  group_by(short_trigger) %>%
  summarize(Mean_certainty_Pred_Exp1 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_Pred_Exp1=Mean_certainty_Pred_Exp1-CILow,YMax_certainty_Pred_Exp1=Mean_certainty_Pred_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.Pred.means)
nrow(exp1.Pred.means) #20

exp2.Pred.means <- exp2 %>%
  group_by(short_trigger) %>%
  summarize(Mean_certainty_Pred_Exp2 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_Pred_Exp2=Mean_certainty_Pred_Exp2-CILow,YMax_certainty_Pred_Exp2=Mean_certainty_Pred_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.Pred.means)
nrow(exp2.Pred.means) #20

exp1.PredItem.means <- exp1 %>%
  group_by(short_trigger,content) %>%
  summarize(Mean_certainty_PredItem_Exp1 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_PredItem_Exp1=Mean_certainty_PredItem_Exp1-CILow,YMax_certainty_PredItem_Exp1=Mean_certainty_PredItem_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.PredItem.means)
nrow(exp1.PredItem.means) #400

exp2.PredItem.means <- exp2 %>%
  group_by(short_trigger,content) %>%
  summarize(Mean_certainty_PredItem_Exp2 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_PredItem_Exp2=Mean_certainty_PredItem_Exp2-CILow,YMax_certainty_PredItem_Exp2=Mean_certainty_PredItem_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.PredItem.means)
nrow(exp2.PredItem.means) #400

exp1.PredItemFact.means <- exp1 %>%
  group_by(short_trigger,contentFact,) %>%
  summarize(Mean_certainty_PredItemFact_Exp1 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_PredItemFact_Exp1=Mean_certainty_PredItemFact_Exp1-CILow,YMax_certainty_PredItemFact_Exp1=Mean_certainty_PredItemFact_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.PredItemFact.means)
nrow(exp1.PredItemFact.means) #800

exp2.PredItemFact.means <- exp2 %>%
  group_by(short_trigger,contentFact,) %>%
  summarize(Mean_certainty_PredItemFact_Exp2 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_PredItemFact_Exp2=Mean_certainty_PredItemFact_Exp2-CILow,YMax_certainty_PredItemFact_Exp2=Mean_certainty_PredItemFact_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.PredItemFact.means)
nrow(exp2.PredItemFact.means) #800

# combine the data
pred.means <- left_join(exp1.Pred.means, exp2.Pred.means, by = c("short_trigger"))
nrow(pred.means) #20
summary(pred.means)

predItem.means <- left_join(exp1.PredItem.means, exp2.PredItem.means, by = c("short_trigger","content"))
nrow(predItem.means) #400
summary(predItem.means)

predItemFact.means <- left_join(exp1.PredItemFact.means, exp2.PredItemFact.means, by = c("short_trigger","contentFact"))
nrow(predItemFact.means) #400
summary(predItemFact.means)

# Spearman rank correlations

cor.test(pred.means$Mean_certainty_Pred_Exp1, pred.means$Mean_certainty_Pred_Exp2, method=c("spearman"))
# .98
cor.test(predItem.means$Mean_certainty_PredItem_Exp1, predItem.means$Mean_certainty_PredItem_Exp2, method=c("spearman"), exact=FALSE)
# .9
cor.test(predItemFact.means$Mean_certainty_PredItemFact_Exp1, predItemFact.means$Mean_certainty_PredItemFact_Exp2, method=c("spearman"), exact=FALSE)
# .85

# how many ratings
count = exp1 %>%
  group_by(short_trigger,content) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14.00   22.00   25.00   25.25   28.00   43.00 

count = exp2 %>%
  group_by(short_trigger,content) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14      22      25      25      28      41

count = exp1 %>%
  group_by(short_trigger,contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.00   10.00   13.00   12.62   15.00   25.00 

count = exp2 %>%
  group_by(short_trigger,contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.0    10.0    12.0    12.5    15.0    27.0

# plots 

# exp1 vs exp2 by predicate
ggplot(pred.means, aes(x=Mean_certainty_Pred_Exp1, y=Mean_certainty_Pred_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Certainty rating Exp. 1") +
  ylab("Certainty rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .98")
ggsave(f="../graphs/SUP-certainty-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by predicate/item
ggplot(predItem.means, aes(x=Mean_certainty_PredItem_Exp1, y=Mean_certainty_PredItem_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Certainty rating Exp. 1") +
  ylab("Certainty rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .9")
ggsave(f="../graphs/SUP-certainty-PredItem-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by predicate/item/fact
ggplot(predItemFact.means, aes(x=Mean_certainty_PredItemFact_Exp1, y=Mean_certainty_PredItemFact_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Certainty rating Exp. 1") +
  ylab("Certainty rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .85")
ggsave(f="../graphs/SUP-certainty-PredItemFact-Exp1-by-Exp2.pdf",height=4,width=4)

#### at-issueness (Exp 1 vs Exp 2) ----

# load the data
exp1 <- read_csv("../data/d.csv")
nrow(exp1) #10100

exp2 <- read_csv("../../exp2/data/d.csv")
nrow(exp2) #10000

# calculate ai means: by-predicate, by-item/fact by-predicate/item, by-predicate/item/fact

# by-predicate
exp1.Pred.means <- exp1 %>%
  group_by(short_trigger) %>%
  summarize(Mean_ai_Pred_Exp1 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_Pred_Exp1=Mean_ai_Pred_Exp1-CILow,YMax_ai_Pred_Exp1=Mean_ai_Pred_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.Pred.means)
nrow(exp1.Pred.means) #20

exp2.Pred.means <- exp2 %>%
  group_by(short_trigger) %>%
  summarize(Mean_ai_Pred_Exp2 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_Pred_Exp2=Mean_ai_Pred_Exp2-CILow,YMax_ai_Pred_Exp2=Mean_ai_Pred_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.Pred.means)
nrow(exp2.Pred.means) #20

# by-contentFact
exp1.ItemFact.means <- exp1 %>%
  group_by(contentFact) %>%
  summarize(Mean_ai_ItemFact_Exp1 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_ItemFact_Exp1=Mean_ai_ItemFact_Exp1-CILow,YMax_ai_ItemFact_Exp1=Mean_ai_ItemFact_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.ItemFact.means)
nrow(exp1.ItemFact.means) #40

exp2.ItemFact.means <- exp2 %>%
  group_by(contentFact) %>%
  summarize(Mean_ai_ItemFact_Exp2 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_ItemFact_Exp2=Mean_ai_ItemFact_Exp2-CILow,YMax_ai_ItemFact_Exp2=Mean_ai_ItemFact_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.ItemFact.means)
nrow(exp2.ItemFact.means) #40

# by-predicate/item
exp1.PredItem.means <- exp1 %>%
  group_by(short_trigger,content) %>%
  summarize(Mean_ai_PredItem_Exp1 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_PredItem_Exp1=Mean_ai_PredItem_Exp1-CILow,YMax_ai_PredItem_Exp1=Mean_ai_PredItem_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.PredItem.means)
nrow(exp1.PredItem.means) #400

exp2.PredItem.means <- exp2 %>%
  group_by(short_trigger,content) %>%
  summarize(Mean_ai_PredItem_Exp2 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_PredItem_Exp2=Mean_ai_PredItem_Exp2-CILow,YMax_ai_PredItem_Exp2=Mean_ai_PredItem_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.PredItem.means)
nrow(exp2.PredItem.means) #400

# by-predicate/item/fact
exp1.PredItemFact.means <- exp1 %>%
  group_by(short_trigger,contentFact) %>%
  summarize(Mean_ai_PredItemFact_Exp1 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_PredItemFact_Exp1=Mean_ai_PredItemFact_Exp1-CILow,YMax_ai_PredItemFact_Exp1=Mean_ai_PredItemFact_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.PredItemFact.means)
nrow(exp1.PredItemFact.means) #800

exp2.PredItemFact.means <- exp2 %>%
  group_by(short_trigger,contentFact) %>%
  summarize(Mean_ai_PredItemFact_Exp2 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_PredItemFact_Exp2=Mean_ai_PredItemFact_Exp2-CILow,YMax_ai_PredItemFact_Exp2=Mean_ai_PredItemFact_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.PredItemFact.means)
nrow(exp2.PredItemFact.means) #800

# combine the data
pred.means <- left_join(exp1.Pred.means, exp2.Pred.means, by = c("short_trigger"))
nrow(pred.means) #20
summary(pred.means)

itemFact.means <- left_join(exp1.ItemFact.means, exp2.ItemFact.means, by = c("contentFact"))
nrow(itemFact.means) #40
summary(itemFact.means)

predItem.means <- left_join(exp1.PredItem.means, exp2.PredItem.means, by = c("short_trigger","content"))
nrow(predItem.means) #400
summary(predItem.means)

predItemFact.means <- left_join(exp1.PredItemFact.means, exp2.PredItemFact.means, by = c("short_trigger","contentFact"))
nrow(predItemFact.means) #400
summary(predItemFact.means)

# Spearman rank correlations

cor.test(pred.means$Mean_ai_Pred_Exp1, pred.means$Mean_ai_Pred_Exp2, method=c("spearman"))
# .99
cor.test(itemFact.means$Mean_ai_ItemFact_Exp1, itemFact.means$Mean_ai_ItemFact_Exp2, method=c("spearman"))
# .19
cor.test(predItem.means$Mean_ai_PredItem_Exp1, predItem.means$Mean_ai_PredItem_Exp2, method=c("spearman"), exact=FALSE)
# .84
cor.test(predItemFact.means$Mean_ai_PredItemFact_Exp1, predItemFact.means$Mean_ai_PredItemFact_Exp2, method=c("spearman"), exact=FALSE)
# .72

# count (as above with projection)

# plots 

# exp1 vs exp2 by predicate
ggplot(pred.means, aes(x=Mean_ai_Pred_Exp1, y=Mean_ai_Pred_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Asking-whether rating Exp. 1") +
  ylab("Asking-whether rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .99")
ggsave(f="../graphs/SUP-ai-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by itemFact
ggplot(itemFact.means, aes(x=Mean_ai_ItemFact_Exp1, y=Mean_ai_ItemFact_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Asking-whether rating Exp. 1") +
  ylab("Asking-whether rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .19")
ggsave(f="../graphs/SUP-ai-ItemFact-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by predicate/item
ggplot(predItem.means, aes(x=Mean_ai_PredItem_Exp1, y=Mean_ai_PredItem_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Asking-whether rating Exp. 1") +
  ylab("Asking-whether rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .84")
ggsave(f="../graphs/SUP-ai-PredItem-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by predicate/item/fact
ggplot(predItemFact.means, aes(x=Mean_ai_PredItemFact_Exp1, y=Mean_ai_PredItemFact_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Asking-whether rating Exp. 1") +
  ylab("Asking-whether rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .72")
ggsave(f="../graphs/SUP-ai-PredItemFact-Exp1-by-Exp2.pdf",height=4,width=4)


# plot for Cologne 2024 talk ----

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

theme_set(theme_bw()) 

source('../../helpers.R')

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load the data
d <- read_csv("../data/d.csv")
nrow(d) #10100

# mean at-issueness by predicate, including the main clause controls
means = d %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(ai), CILow = ci.low(ai), CIHigh = ci.high(ai)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(short_trigger),Mean))
means
print(as_tibble(means), n = 22)
levels(means$verb)

# define colors for the predicates
cols = data.frame(V=levels(means$verb))

cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", "NF"))
         

levels(cols$V)
cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)

cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#E69F00","gray60")


cols$V <- factor(cols$V, levels = cols[order(as.character(means$verb)),]$V, ordered = TRUE)
levels(cols$V)

means$VeridicalityGroup = factor(x=
                                   ifelse(means$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", "NF"), levels=rev(c("F","NF")))

subjmeans = d %>%
  group_by(short_trigger,workerid) %>%
  summarize(Mean = mean(ai)) 
subjmeans$verb <- factor(subjmeans$short_trigger, levels = unique(levels(means$verb)))
subjmeans$VeridicalityGroup = factor(x=
                                       ifelse(subjmeans$verb %in% c("know", "discover", "reveal", "see", "be annoyed"), "F", "NF"),levels=rev(c("F","NF")))
levels(subjmeans$verb)

# plot of means, 95% CIs and participants' ratings 
ggplot(means, aes(x=verb, y=Mean)) +
  geom_violin(data=subjmeans,scale="width",aes(fill=VeridicalityGroup, alpha = .1)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax, fill=VeridicalityGroup, shape=VeridicalityGroup),width=0.1,color="black") +
  geom_point(aes(fill=VeridicalityGroup, shape=VeridicalityGroup),stroke=.5,size=3,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  #scale_alpha(range = c(.3,1)) +
  scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),labels=rev(c("factive","nonfactive","main clause\ncontrols")),name="Predicate type") +
  scale_fill_manual(values=rev(c("#E69F00","gray60")),labels=rev(c("factive","nonfactive","main clause\ncontrols")),name="Predicate type") +
  guides(fill=FALSE, shape=F, alpha=F, color=F) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="bottom") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Mean asking-whether rating") +
  xlab("Predicate") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("/Users/tonhauser.1/Desktop/cologne.pdf",height=4.5,width=7)


#### CODE ENDS HERE? ----
# comparisons of at-issueness ratings in Exps 1 and 2 ----
# load the relevant data
exp1 <- read_csv("../../main/exp1/data/d.csv")
nrow(exp1) #10100

exp2 <- read_csv("../../main/exp2/data/d.csv")
nrow(exp2) #10000

# how many judgments? 
count = exp1 %>%
  group_by(content,prior_typeNew,short_trigger) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.00   10.00   13.00   12.62   15.00   25.00

count = exp1 %>%
  group_by(content,short_trigger) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14.00   22.00   25.00   25.25   28.00   43.00

count = exp1 %>%
  group_by(short_trigger) %>%
  tally(sort=T)
summary(count$n) #505

count = exp2 %>%
  group_by(content,prior_typeNew,short_trigger) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.0    10.0    12.0    12.5    15.0    27.0 

count = exp2 %>%
  group_by(content,short_trigger) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14      22      25      25      28      41

count = exp2 %>%
  group_by(short_trigger) %>%
  tally(sort=T)
summary(count$n) #500

# select needed columns, calculate ai means
names(exp1)
# by-predicate means
exp1.Pai = exp1 %>%
  select(c("contentNew", "prior_typeNew", "short_trigger", "ai")) %>%
  group_by(short_trigger) %>%
  summarize(Exp1_PMean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(Exp1_PYMin_ai=Exp1_PMean_ai-CILow,Exp1_PYMax_ai=Exp1_PMean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh))
exp1.Pai
# by-content/fact means
exp1.CFai = exp1 %>%
  select(c("contentNew", "prior_typeNew", "short_trigger", "ai")) %>%
  group_by(contentNew, prior_typeNew) %>%
  summarize(Exp1_CFMean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(Exp1_CFYMin_ai=Exp1_CFMean_ai-CILow,Exp1_CFYMax_ai=Exp1_CFMean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh)) %>%
  rename("content" = "contentNew", "factType" = "prior_typeNew")
exp1.CFai
# by-content/predicate means
exp1.PCai = exp1 %>%
  select(c("contentNew", "prior_typeNew", "short_trigger", "ai")) %>%
  group_by(short_trigger,contentNew) %>%
  summarize(Exp1_PCMean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(Exp1_PCYMin_ai=Exp1_PCMean_ai-CILow,Exp1_PCYMax_ai=Exp1_PCMean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh)) %>%
  # rename for binding
  rename("content" = "contentNew")
exp1.PCai
# by-content/fact/predicate means
exp1.PCFai = exp1 %>%
  select(c("contentNew", "prior_typeNew", "short_trigger", "ai")) %>%
  group_by(short_trigger,contentNew, prior_typeNew) %>%
  summarize(Exp1_PCFMean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(Exp1_PCFYMin_ai=Exp1_PCFMean_ai-CILow,Exp1_PCFYMax_ai=Exp1_PCFMean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh)) %>%
  # rename for binding
  rename("content" = "contentNew", "factType" = "prior_typeNew")
exp1.PCFai

names(exp2)
# by-predicate means
exp2.Pai = exp2 %>%
  select(c("contentNew", "prior_typeNew", "short_trigger", "ai")) %>%
  group_by(short_trigger) %>%
  summarize(Exp2_PMean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(Exp2_PYMin_ai=Exp2_PMean_ai-CILow,Exp2_PYMax_ai=Exp2_PMean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh))
exp2.Pai
# by-content/fact means
exp2.CFai = exp2 %>%
  select(c("contentNew", "prior_typeNew", "short_trigger", "ai")) %>%
  group_by(contentNew, prior_typeNew) %>%
  summarize(Exp2_CFMean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(Exp2_CFYMin_ai=Exp2_CFMean_ai-CILow,Exp2_CFYMax_ai=Exp2_CFMean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh)) %>%
  rename("content" = "contentNew", "factType" = "prior_typeNew")
exp2.CFai
# by-content/predicate means
exp2.PCai = exp2 %>%
  select(c("contentNew", "prior_typeNew", "short_trigger", "ai")) %>%
  group_by(short_trigger,contentNew) %>%
  summarize(Exp2_PCMean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(Exp2_PCYMin_ai=Exp2_PCMean_ai-CILow,Exp2_PCYMax_ai=Exp2_PCMean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh)) %>%
  # rename for binding
  rename("content" = "contentNew")
exp2.PCai
# by-content/fact/predicate means
exp2.PCFai = exp2 %>%
  select(c("contentNew", "prior_typeNew", "short_trigger", "ai")) %>%
  group_by(short_trigger,contentNew, prior_typeNew) %>%
  summarize(Exp2_PCFMean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(Exp2_PCFYMin_ai=Exp2_PCFMean_ai-CILow,Exp2_PCFYMax_ai=Exp2_PCFMean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh)) %>%
  # rename for binding
  rename("content" = "contentNew", "factType" = "prior_typeNew")
exp2.PCFai

# bind the datasets
Pai = left_join(exp1.Pai, exp2.Pai, by = c("short_trigger" = "short_trigger"))
Pai

CFai = left_join(exp1.CFai, exp2.CFai, by = c("content" = "content", "factType" = "factType"))
CFai

PCai = left_join(exp1.PCai, exp2.PCai, by = c("short_trigger" = "short_trigger", "content" = "content"))
PCai

# also bind the ai.means calculated over Exps 1 and 2
ai.means <- read_csv("../data/ai.means.csv")
ai.means
ai.means = ai.means %>%
  rename("factType" = "prior_typeNew", "Mean_ai12" = "Mean_ai", "YMin_ai12" = "YMin_ai", "YMax_ai12" = "YMax_ai")

PCFai = left_join(exp1.PCFai, exp2.PCFai, by = c("short_trigger" = "short_trigger", "content" = "content", "factType" = "factType")) %>%
  left_join(., ai.means, by = c("short_trigger" = "short_trigger", "content" = "content", "factType" = "factType"))
PCFai

# plots
names(PCFai)
# by-predicate/content/fact
ggplot(PCFai, aes(x=Exp1_PCFMean_ai, y=Exp2_PCFMean_ai, color = factType, shape = factType, fill = factType)) +
  geom_errorbarh(aes(xmin=Exp1_PCFYMin_ai,xmax=Exp1_PCFYMax_ai)) +
  geom_errorbar(aes(ymin=Exp2_PCFYMin_ai,ymax=Exp2_PCFYMax_ai)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  #geom_smooth(method="lm",colour="black") +
  scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"), 
                     values=c("#56B4E9","#E69F00")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Exp1 by-content/fact/predicate mean ai") +
  ylab("Exp2 by-content/fact/predicate mean ai") +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/ai-by-PCF-exp1-exp2.pdf",height=8,width=8)

# by-PCF Exp1 against Exp12 mean
ggplot(PCFai, aes(x=Exp1_PCFMean_ai, y=Mean_ai12, color = factType, shape = factType, fill = factType)) +
  geom_errorbarh(aes(xmin=Exp1_PCFYMin_ai,xmax=Exp1_PCFYMax_ai)) +
  geom_errorbar(aes(ymin=YMin_ai12,ymax=YMax_ai12)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  #geom_smooth(method="lm",colour="black") +
  scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"), 
                     values=c("#56B4E9","#E69F00")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Exp1 by-content/fact/predicate mean ai") +
  ylab("Exp12 mean ai") +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)

# by-PCF Exp2 against Exp12 mean
ggplot(PCFai, aes(x=Exp2_PCFMean_ai, y=Mean_ai12, color = factType, shape = factType, fill = factType)) +
  geom_errorbarh(aes(xmin=Exp2_PCFYMin_ai,xmax=Exp2_PCFYMax_ai)) +
  geom_errorbar(aes(ymin=YMin_ai12,ymax=YMax_ai12)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  #geom_smooth(method="lm",colour="black") +
  scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"), 
                     values=c("#56B4E9","#E69F00")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Exp2 by-content/fact/predicate mean ai") +
  ylab("Exp12 mean ai") +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)

# by-predicate/content
ggplot(PCai, aes(x=Exp1_PCMean_ai, y=Exp2_PCMean_ai)) +
  geom_errorbarh(aes(xmin=Exp1_PCYMin_ai,xmax=Exp1_PCYMax_ai)) +
  geom_errorbar(aes(ymin=Exp2_PCYMin_ai,ymax=Exp2_PCYMax_ai)) +
  geom_point(stroke=.5,size=2,color="black") +
  #scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  #scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  #scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"), 
  #                   values=c("#56B4E9","#E69F00")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Exp1 by-content/predicate mean ai") +
  ylab("Exp2 by-content/predicate mean ai") +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/ai-by-PC-exp1-exp2.pdf",height=8,width=8)

# by-content/fact
ggplot(CFai, aes(x=Exp1_CFMean_ai, y=Exp2_CFMean_ai, color = factType, shape = factType, fill = factType)) +
  geom_errorbarh(aes(xmin=Exp1_CFYMin_ai,xmax=Exp1_CFYMax_ai)) +
  geom_errorbar(aes(ymin=Exp2_CFYMin_ai,ymax=Exp2_CFYMax_ai)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"), 
                     values=c("#56B4E9","#E69F00")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Exp1 by-content/fact mean ai") +
  ylab("Exp2 by-content/fact mean ai") +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1)
ggsave(f="../graphs/ai-by-CF-exp1-exp2.pdf",height=4,width=4)

# by-predicate
ggplot(Pai, aes(x=Exp1_PMean_ai, y=Exp2_PMean_ai)) +
  geom_errorbarh(aes(xmin=Exp1_PYMin_ai,xmax=Exp1_PYMax_ai)) +
  geom_errorbar(aes(ymin=Exp2_PYMin_ai,ymax=Exp2_PYMax_ai)) +
  geom_point(stroke=.5,size=2.5,color="black") +
  geom_text(aes(label = short_trigger), hjust = 0, nudge_x = 0.05) +
  #scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  #scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  #scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"), 
  #                   values=c("#56B4E9","#E69F00")) +
  geom_abline(intercept=0,slope=1,color="gray70",linetype="dashed") +
  xlab("Exp1 by-predicate mean ai") +
  ylab("Exp2 by-predicate mean ai") +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1)
ggsave(f="../graphs/ai-by-P-exp1-exp2.pdf",height=4,width=4)

# investigation of variability in ai ratings in exp 1 and 2 ----
# load the data
d1 <- read_csv("../data/d.csv")
nrow(d1) #10100
d1$experiment = "Exp 1"

d2 <- read_csv("../../exp2/data/d.csv")
nrow(d2) #10000
d2$experiment = "Exp 2"

view(d1)

d1_long = d1 %>%
  pivot_longer(ai:projective,names_to="rating_type",values_to="rating") %>% 
  select(short_trigger,rating,rating_type,experiment,block_ai)

d2_long = d2 %>%
  pivot_longer(ai:projective,names_to="rating_type",values_to="rating") %>% 
  select(short_trigger,rating,rating_type,experiment,block_ai)

d_long = bind_rows(d1_long,d2_long)  

p1 = d_long %>% 
  filter(rating_type == "ai" & experiment == "Exp 1") %>% 
  ggplot(aes(x=rating,fill=block_ai)) +
  geom_density(alpha=.3) +
  facet_wrap(~short_trigger)

p2 = d_long %>% 
  filter(rating_type == "ai" & experiment == "Exp 2") %>% 
  ggplot(aes(x=rating,fill=block_ai)) +
  geom_density(alpha=.3) +
  facet_wrap(~short_trigger)

grid.arrange(p1,p2,nrow=1)


d_long %>% 
  filter(rating_type == "projective") %>% 
  ggplot(aes(x=rating,fill=experiment)) +
  geom_density(alpha=.3) +
  facet_wrap(~short_trigger)

d1$diff_projai = d1$projective - d1$ai
d2$diff_projai = d2$projective - d2$ai

d1d2 = bind_rows(d1,d2)

p_exp1 = d1d2 %>% 
  # filter(block_ai == "block1") %>% 
  filter(experiment == "Exp 1") %>% 
  ggplot(aes(x=diff_projai,fill=block_ai)) +
  geom_density(alpha=.3) +
  geom_vline(xintercept=0,linetype="dashed",alpha=.5) +
  ylim(0,15) +
  facet_wrap(~short_trigger) +
  ggtitle("Exp 1")

p_exp2 = d1d2 %>% 
  # filter(block_ai == "block2") %>% 
  filter(experiment == "Exp 2") %>% 
  ggplot(aes(x=diff_projai,fill=block_ai)) +
  geom_density(alpha=.3) +
  geom_vline(xintercept=0,linetype="dashed",alpha=.5) +
  ylim(0,15) +
  facet_wrap(~short_trigger) +
  ggtitle("Exp 2")

grid.arrange(p_exp1,p_exp2,nrow=1)

d1d2 %>% 
  # filter(block_ai == "block1") %>% 
  # filter(experiment == "Exp 1") %>% 
  ggplot(aes(x=diff_projai,fill=experiment)) +
  geom_density(alpha=.3) +
  geom_vline(xintercept=0,linetype="dashed",alpha=.5) +
  facet_wrap(~short_trigger)

# plot of asking-whether ratings in ai/proj by proj/ai ----
# load the data
d_projai <- read_csv("../data/d_projai.csv")
nrow(d_projai) #5120
d_aiproj <- read_csv("../data/d_aiproj.csv")
nrow(d_aiproj) #4980

# reduce to asking-whether ratings
d_projai = d_projai %>%
  select(c(ai,short_trigger)) %>%
  group_by(short_trigger) %>%
  summarize(Mean_ai_projai = mean(ai))
d_projai

d_aiproj = d_aiproj %>%
  select(c(ai,short_trigger)) %>%
  group_by(short_trigger) %>%
  summarize(Mean_ai_aiproj = mean(ai))
d_aiproj

d = left_join(d_projai, d_aiproj, by = c("short_trigger" = "short_trigger"))

ggplot(d, aes(x=Mean_ai_aiproj, y=Mean_ai_projai)) +
  geom_smooth(method="lm",colour="black") +
  #geom_text(shape=20, size=1, alpha=.3,colour="black") +
  geom_text(aes(label = short_trigger)) +
  xlab("ai/proj") +
  ylab("proj/ai") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  coord_fixed(ratio = 1)

# plot of certainty ratings in ai/proj by proj/ai ----
# load the data
d_projai <- read_csv("../data/d_projai.csv")
nrow(d_projai) #5120
d_aiproj <- read_csv("../data/d_aiproj.csv")
nrow(d_aiproj) #4980

# reduce to certainty ratings
d_projai = d_projai %>%
  select(c(projective,short_trigger)) %>%
  group_by(short_trigger) %>%
  summarize(Mean_proj_projai = mean(projective))
d_projai

d_aiproj = d_aiproj %>%
  select(c(projective,short_trigger)) %>%
  group_by(short_trigger) %>%
  summarize(Mean_proj_aiproj = mean(projective))
d_aiproj

d = left_join(d_projai, d_aiproj, by = c("short_trigger" = "short_trigger"))

ggplot(d, aes(x=Mean_proj_aiproj, y=Mean_proj_projai)) +
  geom_smooth(method="lm",colour="black") +
  #geom_text(shape=20, size=1, alpha=.3,colour="black") +
  geom_text(aes(label = short_trigger)) +
  xlab("ai/proj") +
  ylab("proj/ai") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  coord_fixed(ratio = 1)

# plot of certainty ratings by ai ratings ----
# load the data
d_projai <- read_csv("../data/d_projai.csv")
nrow(d_projai) #5120
d_aiproj <- read_csv("../data/d_aiproj.csv")
nrow(d_aiproj) #4980

# aggregate
d_projai = d_projai %>%
  select(c(ai,projective,short_trigger)) %>%
  group_by(short_trigger) %>%
  summarize(Mean_proj = mean(projective), Mean_ai = mean(ai))
d_projai

d_aiproj = d_aiproj %>%
  select(c(ai,projective,short_trigger)) %>%
  group_by(short_trigger) %>%
  summarize(Mean_proj = mean(projective), Mean_ai = mean(ai))
d_aiproj

d_projai %>% filter(!short_trigger %in% c("think","suggest","pretend")) %>%
  ggplot(aes(x=Mean_ai, y=Mean_proj)) +
  ggtitle("projai") +
  geom_smooth(method="lm",colour="black") +
  #geom_text(shape=20, size=1, alpha=.3,colour="black") +
  geom_text(aes(label = short_trigger)) +
  xlab("asking whether (not-at-issueness)") +
  ylab("certainty (projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  coord_fixed(ratio = 1)

d_aiproj %>% filter(!short_trigger %in% c("think","suggest","pretend")) %>%
  ggplot(aes(x=Mean_ai, y=Mean_proj)) +
  ggtitle("aiproj") +
  geom_smooth(method="lm",colour="black") +
  #geom_text(shape=20, size=1, alpha=.3,colour="black") +
  geom_text(aes(label = short_trigger)) +
  xlab("asking whether (not-at-issueness)") +
  ylab("certainty (projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  coord_fixed(ratio = 1)

# create by-predicate/content/fact indicates for plotting ----

# load the data
d <- read_csv("../data/d.csv")
nrow(d) #10100
d_projai <- read_csv("../data/d_projai.csv")
nrow(d_projai) #5120
d_aiproj <- read_csv("../data/d_aiproj.csv")
nrow(d_aiproj) #4980

# create indicates for d
# prior
d_prior.means = d %>%
  group_by(short_trigger,contentFact) %>%
  summarize(Mean_prior = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior=Mean_prior-CILow,YMax_prior=Mean_prior+CIHigh) %>%
  select(!c(CILow,CIHigh))
d_prior.means

# ai
d_ai.means = d %>%
  group_by(short_trigger, contentFact) %>%
  summarize(Mean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai=Mean_ai-CILow,YMax_ai=Mean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh))
d_ai.means

# projection
d_proj.means = d %>%
  group_by(short_trigger, contentFact) %>%
  summarize(Mean_proj = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_proj=Mean_proj-CILow,YMax_proj=Mean_proj+CIHigh) %>%
  select(!c(CILow,CIHigh))
d_proj.means

# bind the data 
d.means = left_join(d_prior.means, d_proj.means, by = c("short_trigger" = "short_trigger", "contentFact" = "contentFact")) %>%
  left_join(., d_ai.means, by = c("short_trigger" = "short_trigger", "contentFact" = "contentFact"))
summary(d.means)

# order predicates by projection mean
tmp = d %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective)) %>%
  ungroup() %>%
  mutate(short_trigger = fct_reorder(as.factor(short_trigger),Mean))  
tmp

d.means = d.means %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(tmp$short_trigger)))
levels(d.means$short_trigger)

# create indicates for d_aiproj
# prior
d_aiproj_prior.means = d_aiproj %>%
  group_by(short_trigger,contentFact) %>%
  summarize(Mean_prior = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior=Mean_prior-CILow,YMax_prior=Mean_prior+CIHigh) %>%
  select(!c(CILow,CIHigh))
d_aiproj_prior.means

# ai
d_aiproj_ai.means = d_aiproj %>%
  group_by(short_trigger, contentFact) %>%
  summarize(Mean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai=Mean_ai-CILow,YMax_ai=Mean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh))
d_aiproj_ai.means

# projection
d_aiproj_proj.means = d_aiproj %>%
  group_by(short_trigger, contentFact) %>%
  summarize(Mean_proj = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_proj=Mean_proj-CILow,YMax_proj=Mean_proj+CIHigh) %>%
  select(!c(CILow,CIHigh))
d_aiproj_proj.means

# bind the data 
d_aiproj.means = left_join(d_aiproj_prior.means, d_aiproj_proj.means, by = c("short_trigger" = "short_trigger", "contentFact" = "contentFact")) %>%
  left_join(., d_aiproj_ai.means, by = c("short_trigger" = "short_trigger", "contentFact" = "contentFact"))
summary(d_aiproj.means)

# order predicates by projection mean
tmp = d_aiproj %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective)) %>%
  ungroup() %>%
  mutate(short_trigger = fct_reorder(as.factor(short_trigger),Mean))  
tmp

d_aiproj.means = d_aiproj.means %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(tmp$short_trigger)))
levels(d_aiproj.means$short_trigger)

# create indicates for d_projai
# prior
d_projai_prior.means = d_projai %>%
  group_by(short_trigger,contentFact) %>%
  summarize(Mean_prior = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior=Mean_prior-CILow,YMax_prior=Mean_prior+CIHigh) %>%
  select(!c(CILow,CIHigh))
d_projai_prior.means

# ai
d_projai_ai.means = d_projai %>%
  group_by(short_trigger, contentFact) %>%
  summarize(Mean_ai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai=Mean_ai-CILow,YMax_ai=Mean_ai+CIHigh) %>%
  select(!c(CILow,CIHigh))
d_projai_ai.means

# projection
d_projai_proj.means = d_projai %>%
  group_by(short_trigger, contentFact) %>%
  summarize(Mean_proj = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_proj=Mean_proj-CILow,YMax_proj=Mean_proj+CIHigh) %>%
  select(!c(CILow,CIHigh))
d_projai_proj.means

# bind the data 
d_projai.means = left_join(d_projai_prior.means, d_projai_proj.means, by = c("short_trigger" = "short_trigger", "contentFact" = "contentFact")) %>%
  left_join(., d_projai_ai.means, by = c("short_trigger" = "short_trigger", "contentFact" = "contentFact"))
summary(d_projai.means)

# order predicates by projection mean
tmp = d_projai %>%
  group_by(short_trigger) %>%
  summarize(Mean = mean(projective)) %>%
  ungroup() %>%
  mutate(short_trigger = fct_reorder(as.factor(short_trigger),Mean))  
tmp

d_projai.means = d_projai.means %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(tmp$short_trigger)))
levels(d_projai.means$short_trigger)

# save means
write_csv(d.means, file="../data/d.means.csv")
write_csv(d_aiproj.means, file="../data/d_aiproj.means.csv")
write_csv(d_projai.means, file="../data/d_projai.means.csv")

# data sets available for plotting
# d, d_projai, d_aiproj
# d.means, d_projai.means, d_aiproj.means

# projection by prior and predicate ----
# raw data
ggplot(d, aes(x=prior, y=projective)) +
  ggtitle("Exp 1: both blocks") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d-projection-by-prior.pdf",height=5,width=5)

ggplot(d_aiproj, aes(x=prior, y=projective)) +
  ggtitle("Exp 1: ai/proj") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_aiproj-projection-by-prior.pdf",height=5,width=5)

ggplot(d_projai, aes(x=prior, y=projective)) +
  ggtitle("Exp 1: proj/ai") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_projai-projection-by-prior.pdf",height=5,width=5)

# by-predicate/content/fact means
ggplot(d.means, aes(x=Mean_prior, y=Mean_proj)) +
  ggtitle("Exp 1: both blocks") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d.means-projection-by-prior.pdf",height=5,width=5)

ggplot(d_aiproj.means, aes(x=Mean_prior, y=Mean_proj)) +
  ggtitle("Exp 1: prior / ai / proj") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_aiproj.means-projection-by-prior.pdf",height=5,width=5)

ggplot(d_projai.means, aes(x=Mean_prior, y=Mean_proj)) +
  ggtitle("Exp 1: prior / proj / ai") +
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_projai.means-projection-by-prior.pdf",height=5,width=5)

# projection by ai and predicate ----
# raw data
ggplot(d, aes(x=ai, y=projective)) +
  ggtitle("Exp 1: all data") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("nai") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d-projection-by-ai.pdf",height=5,width=5)

ggplot(d_aiproj, aes(x=ai, y=projective)) +
  ggtitle("Exp 1: ai/proj") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("nai") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_aiproj-projection-by-ai.pdf",height=5,width=5)

ggplot(d_projai, aes(x=ai, y=projective)) +
  ggtitle("Exp 1: proj/ai") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("nai") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_projai-projection-by-ai.pdf",height=5,width=5)

# by-predicate/content/fact means
ggplot(d.means, aes(x=Mean_ai, y=Mean_proj)) +
  ggtitle("Exp 1: all data") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("nai") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d.means-projection-by-ai.pdf",height=5,width=5)

ggplot(d_aiproj.means, aes(x=Mean_ai, y=Mean_proj)) +
  ggtitle("Exp 1: prior / ai / projection") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("nai") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_aiproj.means-projection-by-ai.pdf",height=5,width=5)

ggplot(d_projai.means, aes(x=Mean_ai, y=Mean_proj)) +
  ggtitle("Exp 1: prior / projection / ai") +
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("nai") +
  ylab("projection") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_projai.means-projection-by-ai-d_projai.pdf",height=5,width=5)

# ai by prior and predicate ----

# raw data
ggplot(d, aes(x=prior, y=ai)) +
  ggtitle("Exp 1: all data") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("nai") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d-ai-by-prior.pdf",height=5,width=5)

ggplot(d_projai, aes(x=prior, y=ai)) +
  ggtitle("Exp 1: all data") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("nai") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_projai-ai-by-prior.pdf",height=5,width=5)

ggplot(d_aiproj, aes(x=prior, y=ai)) +
  ggtitle("Exp 1: all data") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("nai") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_aiproj-ai-by-prior.pdf",height=5,width=5)

# by-predicate/content/fact means
ggplot(d.means, aes(x=Mean_prior, y=Mean_ai)) +
  ggtitle("Exp 1: all data") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("nai") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d.means-ai-by-prior.pdf",height=5,width=5)

ggplot(d_aiproj.means, aes(x=Mean_prior, y=Mean_ai)) +
  ggtitle("Exp 1: prior / ai / projection") + 
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("nai") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_aiproj.means-ai-by-prior.pdf",height=5,width=5)

ggplot(d_projai.means, aes(x=Mean_prior, y=Mean_ai)) +
  ggtitle("Exp 1: prior / projection / ai") +
  geom_smooth(method="lm",colour="black") +
  geom_point(shape=20, size=1, alpha=.3,colour="black") +
  xlab("prior") +
  ylab("nai") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger)
ggsave(f="../graphs/d_projai.means-ai-by-prior.pdf",height=5,width=5)


