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

# Fig.3: by-predicate correlations in the full data ----

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

## Supplement D: By-block by-predicate correlations ----

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


## Supplement G: by-predicate correlations ----

# load the data
d <- read_csv("../data/d.csv")
nrow(d) #10000

mean.proj = d %>%
  group_by(short_trigger) %>%
  summarize(Mean.Proj = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, short_trigger = fct_reorder(as.factor(short_trigger),Mean.Proj)) %>%
  select(-c(CILow,CIHigh))
mean.proj

mean.ai = d %>%
  group_by(short_trigger) %>%
  summarize(Mean.AI = mean(ai), CILow = ci.low(ai), CIHigh = ci.high(ai)) %>%
  mutate(YMin.AI = Mean.AI - CILow, YMax.AI = Mean.AI + CIHigh, short_trigger = fct_reorder(as.factor(short_trigger),mean.proj$Mean.Proj)) %>%
  select(-c(CILow,CIHigh))
mean.ai

means = left_join(mean.proj, mean.ai, by = c("short_trigger"))
means

# to color-code the predicates
means = means %>%
  mutate(predicateType = case_when(short_trigger == "discover" ~ "factive",
                                   short_trigger == "know" ~ "factive",
                                   short_trigger == "be annoyed" ~ "factive",
                                   short_trigger == "reveal" ~ "factive",
                                   short_trigger == "see" ~ "factive",
                                   TRUE ~ "nonfactive"))
table(means$short_trigger,means$predicateType)

ggplot(means, aes(x=Mean.AI, y=Mean.Proj)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "black", size=.5) +
  geom_point(aes(color=predicateType), shape=20, size=5, alpha=1) +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj,color=predicateType),width=.01) +
  geom_errorbarh(aes(xmin=YMin.AI,xmax=YMax.AI, color=predicateType),height=.01) +
  geom_text_repel(aes(label = short_trigger, color=predicateType), hjust = 0.5,  vjust = -1) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Mean asking-whether rating") +
  ylab("Mean certainty rating") + 
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) 
ggsave(f="../graphs/SUP-mean-projection-by-mean-ai.pdf",height=4,width=4)

## Supplement H: distribution of ratings ----
# (to justify fitting beta-models rather than ZOIB models)

# load the data
d <- read_csv("../data/d.csv")
nrow(d) #10000

names(d)
# projective, ai

ggplot(d, aes(projective)) +
  geom_histogram(color="black",bins = 100) +
  facet_wrap(. ~ short_trigger,nrow = 5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,1),labels=c("0","1"), limits = c(-.05,1.05)) 
ggsave(f="../graphs/SUP-rating-distributions-projective.pdf",height=5,width=5)

ggplot(d, aes(ai)) +
  geom_histogram(color="black",bins = 100) +
  facet_wrap(. ~ short_trigger,nrow = 5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,1),labels=c("0","1"), limits = c(-.05,1.05)) 
ggsave(f="../graphs/SUP-rating-distributions-ai.pdf",height=5,width=5)

