source("code/packages_empathy.R")

emptox <- read_excel("data/data_final_preprocessed.xlsx")
emptox$treatment_group <- factor(emptox$treatment_group, levels = c("control","empathy","perspective","friction","boost"))

##### Normality of residuals #####

# Fit the initially preregistered ANOVA model
model <- aov(Toxicity ~ treatment_group, data = emptox)

# Extract residuals
residuals <- model$residuals

# Shapiro-Wilk test for normality
subsample <- sample(residuals, size = 4999, replace = FALSE)
shapiro_test <- shapiro.test(subsample)
shapiro_test

# 1. Anderson-Darling Test
ad_test <- ad.test(residuals)
ad_test

# Q-Q plot for visual assessment of normality
qqnorm(residuals)
qqline(residuals, col = "red")

# Histogram of residuals for an additional visual check
hist(residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue")

##### Normality of residuals for comment length

model <- aov(comment_length ~ treatment_group, data = emptox)

# Extract residuals
residuals <- model$residuals

# Shapiro-Wilk test for normality
subsample <- sample(residuals, size = 4999, replace = FALSE)
shapiro_test <- shapiro.test(subsample)
shapiro_test

# Anderson-Darling Test
ad_test <- ad.test(residuals)
print(ad_test)

# Q-Q plot for visual assessment of normality
qqnorm(residuals)
qqline(residuals, col = "red")

# Histogram of residuals for an additional visual check
hist(residuals, breaks = 10, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue")


#################################################################################

## Power Analysis / MDES

### With linear gaussian logic / preregistered models 

# required sample size (per group)
pwr.anova.test(k = 5,
               n = NULL,
               f = 0.1,
               sig.level = 0.05,
               power = 0.8)
# required n per group: 240

# estimated power
pwr.anova.test(k = 5,
               n = 375, # average group size = 429, 362 = minimal group size (participants but what counts is comments)
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


### With models actually used to accommodate different error distribution

# Toxicity
mod01 <- glm(Toxicity ~ treatment_group, 
             family = inverse.gaussian(link = "log"), 
             data = emptox)

# residual variance
phi <- summary(mod01)$dispersion
resid_sd <- sqrt(phi)

n_per_group <- 5485 / 5

compute_mdes <- function(power, n, sd){
  d <- pwr.t.test(n = n, d = NULL, sig.level = 0.05, power = power, type = "two.sample")$d
  d * sd
}

mdes_tox <- compute_mdes(power = 0.8, n = n_per_group, sd = resid_sd)
mdes_tox

# Comment length
mod02 <- glm(comment_length ~ treatment_group, 
             family = inverse.gaussian(link = "log"), 
             data = emptox)

# residual variance
phi <- summary(mod02)$dispersion
resid_sd <- sqrt(phi)

n_per_group <- 5485 / 5

mdes_cl <- compute_mdes(power = 0.8, n = n_per_group, sd = resid_sd)
mdes_cl

#################################################################################


## Bayesian analysis for null claims

# https://cran.r-project.org/web/packages/BayesFactor/vignettes/manual.html
# https://academic.oup.com/psychsocgerontology/article/75/1/45/5033832
# http://doi.apa.org/getdoi.cfm?doi=10.1037/met0000421
# https://www.nature.com/articles/s41562-021-01177-7

###### TOXICITY 

# bayesfactor
emptox$logToxicity <- log(emptox$Toxicity)  # log-transform to match GLM log link

# Full model Bayes factor
bft <- lmBF(logToxicity ~ treatment_group, data = emptox)
bft # the Bayes factor for the comparison of the alternative versus the null is 0.003

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


###### Comment Length

# bayesfactor
bfl <- lmBF(comment_length ~ treatment_group, data = emptox)
bfl # the Bayes factor for the comparison of the alternative versus the null is 8.888
# this implies that the alternative hypothesis is 9 times more likely to be true than the null hypothesis

## Sample from the posterior of the full model
chainsl = posterior(bfl, iterations = 10000)
## 1:13 are the only "interesting" parameters
summary(chainsl[,1:8])
plot(chainsl[,2:6])



