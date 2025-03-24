library(BayesFactor)
library(stats)
library(pwr)
library(readxl)
library(tidyverse)

## Sensitivity analysis / Minimal Detectable Effect Size (MDES)

# required sample size (per group)
pwr.anova.test(k = 5,
               n = NULL,
               f = 0.1,
               sig.level = 0.05,
               power = 0.8)
# required n per group: 240

# estimated power
pwr.anova.test(k = 5,
               n = 375, # average group size = 429, 362 = minimal group size
               f = 0.1,
               sig.level = 0.05,
               power = NULL)
# estimated power using minimal group size: 0.94

# minimal detectable effect size 
pwr.anova.test(k = 5,
               n = 375, # minimal group size
               f = NULL,
               sig.level = 0.05,
               power = 0.8)
# MDES using minimal group size: F = 0.081

## Bayesian analysis for null claims

# https://cran.r-project.org/web/packages/BayesFactor/vignettes/manual.html
# https://academic.oup.com/psychsocgerontology/article/75/1/45/5033832
# http://doi.apa.org/getdoi.cfm?doi=10.1037/met0000421
# https://www.nature.com/articles/s41562-021-01177-7

emptox <- read_excel("data/data_final_preprocessed.xlsx")
emptox$treatment_group <- factor(emptox$treatment_group, levels = c("control","empathy","perspective","friction","boost"))

###### ANOVA TOXICITY
# frequentist
summary(aov(Toxicity ~ treatment_group, data = emptox))

# bayesfactor
bft <- lmBF(Toxicity ~ treatment_group, data = emptox)
bft # the Bayes factor for the comparison of the alternative versus the null is 0.003
# this implies that the null hypothesis is 1/0.003 = 333 times more likely to be true than H1
plot(bft)

## Sample from the posterior of the full model
chainst = posterior(bft, iterations = 10000)

## the only "interesting" parameters
summary(chainst[,1:8])
plot(chainst[,2:6])

# post-hoc tests: boost vs. control
boostdf <- emptox%>%filter(treatment_group == "boost")
empathydf <- emptox%>%filter(treatment_group == "empathy")
perspectivedf <- emptox%>%filter(treatment_group == "perspective")
frictiondf <- emptox%>%filter(treatment_group == "friction")
controldf <- emptox%>%filter(treatment_group == "control")

t.test(boostdf$Toxicity, controldf$Toxicity)
ttestBF(boostdf$Toxicity, controldf$Toxicity, paired = FALSE)
ttestBF(boostdf$Toxicity, controldf$Toxicity, paired = FALSE, nullInterval=c(-Inf,0))
# two-sided: bf = 0.405 , ±0.05% - H0 only 2.5 times more likely than H1 (effect undetermined by data)

t.test(empathydf$Toxicity, controldf$Toxicity)
ttestBF(empathydf$Toxicity, controldf$Toxicity, paired = FALSE)
ttestBF(empathydf$Toxicity, controldf$Toxicity, paired = FALSE, nullInterval=c(-Inf,0))
# two-sided: bf = 0.048 , ±0.45% - H0 21 times more likely than H1

t.test(perspectivedf$Toxicity, controldf$Toxicity)
ttestBF(perspectivedf$Toxicity, controldf$Toxicity, paired = FALSE)
ttestBF(perspectivedf$Toxicity, controldf$Toxicity, paired = FALSE, nullInterval=c(-Inf,0))
# two-sided: bf = 0.080 , ±0.27% - H0 12 times more likely than H1

t.test(frictiondf$Toxicity, controldf$Toxicity)
ttestBF(frictiondf$Toxicity, controldf$Toxicity, paired = FALSE)
ttestBF(frictiondf$Toxicity, controldf$Toxicity, paired = FALSE, nullInterval=c(-Inf,0))
# two-sided: bf = 0.059 , ±0.37% - H0 17 times more likely than H1


###### ANOVA Comment Length

# frequentist
summary(aov(comment_length ~ treatment_group, data = emptox))

# bayesfactor
bfl <- lmBF(comment_length ~ treatment_group, data = emptox)
bfl # the Bayes factor for the comparison of the alternative versus the null is 8.888
# this implies that the alternative hypothesis is 9 times more likely to be true than the null hypothesis
plot(bfl)

## Sample from the posterior of the full model
chains = posterior(bfl, iterations = 10000)
## 1:13 are the only "interesting" parameters
summary(chainsl[,1:8])
plot(chainsl[,2:6])

############# Multilevel Model - Random Intercept for Respondents
library(lmerTest)
modml <- lmer(Toxicity ~ intervention + (1 | ResponseId), data = data_total)
summary(modml)

modml <- lmer(comment_length ~ intervention + (1 | ResponseId), data = data_total)
summary(modml)
