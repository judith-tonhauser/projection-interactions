# Exp2: interaction of prior beliefs, at-issueness and projection
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
nrow(d) #10000

# Fig.3: by-predicate correlations in the raw data ----

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
ggplot(d, aes(x=Mean_prior, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Mean prior probability ratings \n (higher mean indicates higher prior probability)") +
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
ggplot(d, aes(x=Mean_prior, y=ai)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Mean prior probability ratings \n (higher mean indicates higher prior probability)") +
  ylab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/ai-by-prior.pdf",height=5,width=5)

# Supplemental figures ----

### by-block by-predicate correlations of the factors

# load the data
d_projai <- read_csv("../data/d_projai.csv")
nrow(d_projai) #5180
d_aiproj <- read_csv("../data/d_aiproj.csv")
nrow(d_aiproj) #4820

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
ggplot(d_projai, aes(x=Mean_prior, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Mean prior probability ratings \n (higher mean indicates higher prior probability)") +
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
ggplot(d_projai, aes(x=Mean_prior, y=ai)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Mean prior probability ratings \n (higher mean indicates higher prior probability)") +
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
ggplot(d_aiproj, aes(x=Mean_prior, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Mean prior probability ratings \n (higher mean indicates higher prior probability)") +
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
ggplot(d_aiproj, aes(x=Mean_prior, y=ai)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Mean prior probability ratings \n (higher mean indicates higher prior probability)") +
  ylab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/SUP-aiproj-ai-by-prior.pdf",height=5,width=5)


#### CODE ENDS HERE

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

# create by-predicate/content/fact means for plotting ----

# load the data
d <- read_csv("../data/d.csv")
nrow(d) #10100
d_projai <- read_csv("../data/d_projai.csv")
nrow(d_projai) #5120
d_aiproj <- read_csv("../data/d_aiproj.csv")
nrow(d_aiproj) #4980

# create means for d
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

# create means for d_aiproj
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

# create means for d_projai
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
