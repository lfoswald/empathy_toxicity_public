source("code/packages_empathy.R")

################# GET DATA ####################################################

data <- read_excel("data/data_final_preprocessed.xlsx")
comments <- read_excel("data/comments.xlsx")

# how many would have been dropped?
data_PAP <- data%>%
  filter(Duration__in_seconds_ > 0.3*mean(Duration__in_seconds_))


########## RAW EFFECTS BOXPLOTS ###########################################

# boxplot
box_tr <- ggplot(data, aes(treatment_group, Toxicity, fill = treatment_group))+
  geom_boxplot()+
  ggtitle("Reply Toxicity")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  guides(fill="none")+
  scale_fill_brewer(palette="Pastel1")

box_lr <- ggplot(data, aes(treatment_group, comment_length, fill = treatment_group))+
  geom_boxplot()+
  ggtitle("Comment Lenght")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  guides(fill="none")+
  scale_fill_brewer(palette="Pastel1")

box_t_l_raw <- grid.arrange(box_tr, box_lr, nrow = 1, top = "Treatment Effects on")


ggsave("output/box_pooled_raw.pdf", box_t_l_raw, width = 12 ) 

########## RAW EFFECTS BOXPLOTS ###########################################

# boxplot
box_trt <- ggplot(data, aes(topic, Toxicity, fill = treatment_group))+
  geom_boxplot(alpha = 0.8)+
  ggtitle("Reply Toxicity")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  scale_x_discrete(labels=c("cc" = "Climate Change", "fem" = "Feminism",
                            "ref" = "Migration"))+
  guides(fill="none")+
  scale_fill_jcolors(palette="pal5")

box_lrt <- ggplot(data, aes(topic, comment_length, fill = treatment_group))+
  geom_boxplot(alpha = 0.8)+
  ggtitle("Comment Lenght")+
  xlab("")+
  ylab("")+
  labs(fill = "Treatment group")+
  theme_minimal()+
  scale_x_discrete(labels=c("cc" = "Climate Change", "fem" = "Feminism",
                            "ref" = "Migration"))+
  #guides(fill="none")+
  scale_fill_jcolors(palette="pal5")

box_t_l_topic <- grid.arrange(box_trt, box_lrt, nrow = 1, top = "Topic Differences", widths=c(1.5, 2))


ggsave("output/box_pooled_topic.pdf", box_t_l_topic, width = 12 , height = 6) 


# boxplot
box_t1 <- ggplot(data, aes(topic, Toxicity, fill = topic))+
  geom_boxplot()+
  ggtitle("Reply Toxicity")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  guides(fill="none")+
  scale_x_discrete(labels=c("cc" = "Climate Change", "fem" = "Feminism",
                              "ref" = "Migration"))+
  scale_fill_brewer(palette="Pastel1")

box_t2 <- ggplot(data, aes(topic, comment_length, fill = topic))+
  geom_boxplot()+
  ggtitle("Comment Lenght")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  guides(fill="none")+
  scale_x_discrete(labels=c("cc" = "Climate Change", "fem" = "Feminism",
                            "ref" = "Migration"))+
  scale_fill_brewer(palette="Pastel1")

box_topic <- grid.arrange(box_t1, box_t2, nrow = 1, top = "Topic Differences")


ggsave("output/box_simple_topic.pdf", box_topic, width = 12 ) 

############### Legitimacy Splits #####################################
data$Legitimacy <- factor(data$Legitimacy, levels = c("not at all legitimate",
                                           "rather not legitimate",
                                           "undecided",
                                           "rather legtitimate",
                                           "fully legitimate"))
legi <- ggplot(data, aes(x = Legitimacy))+
  geom_bar(aes(fill = post_direction))+
  facet_grid(UserLanguage ~ Topic)+
  ggtitle("Perceived Legitimacy of Attitude Opposing Statement")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_fill_discrete(labels=c("tendency conservative \n issue devaluation", 
                               "tendency liberal \n issue importance"),
                      name = "Statement direction")
legi
ggsave("output/legitimacy_grid.pdf", legi, width = 12, height = 10)

################ Analysis Preparation ##################################

data$intervention <- relevel(as.factor(data$treatment_group),ref = "control")
data$intervention2 <- relevel(as.factor(data$treatment_group),ref = "friction")

# compute attitudinal distance for moderation hypothesis 3

data <- data%>%
  mutate(attitude_distance = case_when(
    # climate change
    topic == "cc" & issue_attitudes_1 == 1 ~  3,
    topic == "cc" & issue_attitudes_1 == 6 ~  3,
    topic == "cc" & issue_attitudes_1 == 2 ~  2,
    topic == "cc" & issue_attitudes_1 == 5 ~  2,
    topic == "cc" & issue_attitudes_1 == 3 ~  1,
    topic == "cc" & issue_attitudes_1 == 4 ~  1,
    # refugees
    topic == "ref" & issue_attitudes_3 == 1 ~  3,
    topic == "ref" & issue_attitudes_3 == 6 ~  3,
    topic == "ref" & issue_attitudes_3 == 2 ~  2,
    topic == "ref" & issue_attitudes_3 == 5 ~  2,
    topic == "ref" & issue_attitudes_3 == 3 ~  1,
    topic == "ref" & issue_attitudes_3 == 4 ~  1,
    # feminism Germany - gender language
    UserLanguage == "DE" & topic == "fem" & issue_attitudes_2 == 1  ~ 3, 
    UserLanguage == "DE" & topic == "fem" & issue_attitudes_2 == 6  ~ 3,
    UserLanguage == "DE" & topic == "fem" & issue_attitudes_2 == 2  ~ 2, 
    UserLanguage == "DE" & topic == "fem" & issue_attitudes_2 == 5  ~ 2,
    UserLanguage == "DE" & topic == "fem" & issue_attitudes_2 == 3  ~ 1, 
    UserLanguage == "DE" & topic == "fem" & issue_attitudes_2 == 4  ~ 1,
    # feminism US - abortion
    UserLanguage == "EN" & topic == "fem" & issue_attitudes_4 == 1  ~ 3,
    UserLanguage == "EN" & topic == "fem" & issue_attitudes_4 == 6  ~ 3,
    UserLanguage == "EN" & topic == "fem" & issue_attitudes_4 == 2  ~ 2,
    UserLanguage == "EN" & topic == "fem" & issue_attitudes_4 == 5  ~ 2,
    UserLanguage == "EN" & topic == "fem" & issue_attitudes_4 == 3  ~ 1,
    UserLanguage == "EN" & topic == "fem" & issue_attitudes_4 == 4  ~ 1
  ))


# compute activity index for descriptive hypothesis 4
data$online_activity <- data$comments_online + data$social_media + data$opposing_views_b


# scale continuous variables by dividing by two standard deviations (Gelman, 2008)
# do this for all

gelman_scale <- function(x){
  ( (x - mean(x, na.rm=T)) / (2*sd(x, na.rm=TRUE)))
}

# save some raw scales for later
data$raw_comment_length <- data$comment_length
data$raw_attitude_distance <- data$attitude_distance

data[c("Toxicity", "comment_length", 
       "birthyear", "education", "polinterest", 
       "leftright", "time_online", 
       "trait_empathy", "issue_attitudes_1",
       "issue_attitudes_2", "issue_attitudes_3", 
       "issue_attitudes_4", "online_activity",
       "social_media", "comments_online", 
       "angry_views", "opposing_views_a", 
       "attitude_distance", "opposing_views_b")] <- lapply(data[c("Toxicity", 
                                                                  "comment_length", 
                                      "birthyear", "education", "polinterest", 
                                      "leftright", "time_online", 
                                      "trait_empathy", "issue_attitudes_1",
                                      "issue_attitudes_2", "issue_attitudes_3", 
                                      "issue_attitudes_4", "online_activity",
                                      "social_media", "comments_online", 
                                      "angry_views", "opposing_views_a", 
                                      "attitude_distance", "opposing_views_b")], gelman_scale)


# make train/test split before running covariate selection lasso model

full_data <- data%>%
  dplyr::select(Toxicity, comment_length, birthyear, gender, education, polinterest, leftright, topic, post_direction,
         UserLanguage, time_online, trait_empathy, issue_attitudes_1,
         issue_attitudes_2, issue_attitudes_3, issue_attitudes_4, 
         social_media, comments_online, angry_views, opposing_views_a, attitude_distance, opposing_views_b)%>%
  na.omit()

train_data <- full_data %>% sample_frac(.80)
test_data <- full_data %>% sample_frac(.20)

X_full <- full_data %>% dplyr::select(-Toxicity, -comment_length) 
X_train <- train_data %>% dplyr::select(-Toxicity, -comment_length) 
X_test <- train_data %>% dplyr::select(-Toxicity, -comment_length) 

y_full_t <- full_data$Toxicity 
y_train_t <- train_data$Toxicity
y_test_t <- train_data$Toxicity

y_full_l <- full_data$comment_length 
y_train_l <- train_data$comment_length
y_test_l <- train_data$comment_length

# define lambda grid for lasso model
grid = 10^seq(10, -2, length = 100)

################# TOXICITY ##########################################

######### Unadjusted models ############################

# one-way ANOVA 
summary(aov(Toxicity ~ treatment_group, data = data))

# OLS regression with HC2 robust standard errors
mod1 <- lm_robust(Toxicity ~ intervention, data = data)
mod1b <- lm_robust(Toxicity ~ intervention2, data = data)

tidy(mod1)
glance(mod1)

# boxplot
box_toxicity <- ggplot(data, aes(treatment_group, Toxicity, colour = treatment_group))+
  geom_boxplot()+
  theme(legend.position = "none")+
  ggtitle("Treatment Effects on Reply Toxicity")+
  xlab("Experimental Condition")+
  facet_grid(. ~ UserLanguage)
box_toxicity
ggsave("output/box_toxicity.pdf", box_toxicity, width = 12 ) 

# post-hoc tests
TukeyHSD(aov(Toxicity ~ treatment_group , data = data))

#################### Covariate selection - Toxicity ###############################
# fit lasso model to training data
lasso_mod <- glmnet(X_train, y_train_t, alpha=1, lambda = grid) 
plot(lasso_mod)

# select best lambda value
set.seed(1)
cv.out = cv.glmnet(data.matrix(X_train), y_train_t, alpha = 1) 
plot(cv.out) # Draw plot of training MSE as a function of lambda
bestlam = cv.out$lambda.min # Select lamda that minimizes training MSE
lasso_pred = predict(lasso_mod, s = bestlam, newx = data.matrix(X_test)) # Use best lambda to predict test data
mean((lasso_pred - y_test_t )^2) # Calculate test MSE

out = glmnet(X_full, y_full_t, alpha = 1, lambda = grid) # Fit lasso model on full dataset
lasso_coef = predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
lasso_coef

lasso_coef[lasso_coef != 0] # Display only non-zero coefficients
# --> those are the covariates for the Toxicity models


############### ANCOVA / covariate adjusted models ########################

summary(aov(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
              issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance, data = data))

# OLS regression with controls and HC2 robust standard errors
mod2 <- lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
                    issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance ,
                  data = data)

summary(mod2)

################# COMMENT LENGTH ##########################################

######### Unadjusted models ############################

# one-way ANOVA 
summary(aov(comment_length ~ treatment_group, data = data))
anova_test(comment_length ~ treatment_group, data = data) 

# OLS regression with HC2 robust standard errors
mod3 <- lm_robust(comment_length ~ intervention, data = data)
mod3b <- lm_robust(Toxicity ~ intervention2, data = data)

glance(mod3)
tidy(mod3)

# boxplot
box_length <- ggplot(data, aes(treatment_group, comment_length, colour = treatment_group))+
  geom_boxplot()+
  theme(legend.position = "none")+
  ggtitle("Treatment Effects on Comment Length")+
  xlab("Experimental Condition")+
  facet_grid(. ~ UserLanguage)
box_length 
ggsave("output/box_length.pdf", box_length, width = 12 ) 

# post-hoc tests
TukeyHSD(aov(comment_length ~ treatment_group , data = data))

# post-hoc tests with raw comment_length
TukeyHSD(aov(raw_comment_length ~ treatment_group , data = data))

mean(data$raw_comment_length)
sd(data$raw_comment_length)
#################### Covariate selection - Comment length ###############################
# fit lasso model to training data
lasso_mod <- glmnet(X_train, y_train_l, alpha=1, lambda = grid) 
plot(lasso_mod)

# select best lambda value
set.seed(1)
cv.out = cv.glmnet(data.matrix(X_train), y_train_l, alpha = 1) 
plot(cv.out) # Draw plot of training MSE as a function of lambda
bestlam = cv.out$lambda.min # Select lamda that minimizes training MSE
lasso_pred = predict(lasso_mod, s = bestlam, newx = data.matrix(X_test)) # Use best lambda to predict test data
mean((lasso_pred - y_test_l )^2) # Calculate test MSE

out = glmnet(X_full, y_full_l, alpha = 1, lambda = grid) # Fit lasso model on full dataset
lasso_coef = predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
lasso_coef

lasso_coef[lasso_coef != 0] # Display only non-zero coefficients
# --> those are the covariates!


############### ANCOVA / covariate adjusted models ########################

summary(aov(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
              issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance,
            data = data))
        
# OLS regression with controls
mod4 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance, data = data)

summary(mod4)

### summary of all 4 models ####

texreg(list(mod1, mod2, mod3, mod4), include.ci = FALSE, single.row = TRUE,
       #custom.coef.names=c('Intercept', 'Boost', 'Empathy', 'Friction','Perspective',
       #                    'Social media use', 'Gender', 'Post direction', 
       #                    'Language', 'Climate attitude', 'Immigration attitude',
       #                    'Abortion attitude'),
       custom.model.names = c("Toxicity","Toxicity lasso","Length","Length lasso"))

knitreg(list(mod1, mod2, mod3, mod4), include.ci = FALSE, single.row = TRUE,
         #custom.coef.names=c('Intercept', 'Boost', 'Empathy', 'Friction','Perspective',
         #                    'Social media use', 'Gender', 'Post direction', 
         #                    'Language', 'Climate attitude', 'Immigration attitude',
         #                    'Abortion attitude'),
          custom.model.names = c("Toxicity","Toxicity lasso","Length","Length lasso"))


coefplot <- plotreg(list(mod1, mod2, mod3, mod4))
coefplot
ggsave("output/coef_plot.pdf", coefplot , height = 12 ) 


### ANCOVA Assumptions #####

# 1. linearity between covariate and outcome --> grouped scatterplot
# 2. homogeneity of regression slopes --> interaction model between treatment & covariate --> n.s. = good
# 3. normality of residuals --> fit model, metrics <- augment(result), shapiro_test(metrics$.resid)
# 4. homogeniety of variances --> metrics %>% levene_test(.resid ~ treatment)
# 5. outliers --> metrics %>% filter(abs(.std.resid) > 3) %>% as.data.frame()


############### Attitude distance moderation hypothesis #######################

table(data$raw_attitude_distance)


# test moderation hypothesis
summary(aov(Toxicity ~ intervention * raw_attitude_distance, data = data))

# OLS regression 
mod5 <- lm_robust(Toxicity ~  intervention* attitude_distance, data = data)

################ Activity Hypothesis ###################################

# test activity hypothesis
mod6 <- lm_robust(Toxicity ~ online_activity, data = data)

summary(mod6)

knitreg(list(mod5, mod6))
texreg(list(mod5, mod6), include.ci = FALSE, single.row = TRUE)

################## Comparisons #################################################
# topics 
summary(aov(Toxicity ~ topic, data = data))
# languages
t.test(Toxicity ~ UserLanguage,data = data)

# data subsets
DE_data <- data%>%filter(UserLanguage == "DE")
EN_data <- data%>%filter(UserLanguage == "EN")
data_legit <- data%>%filter(legitimacy > 2)

summary(aov(Toxicity ~ intervention, data = DE_data ))
summary(aov(Toxicity ~ intervention, data = EN_data ))
summary(aov(Toxicity ~ intervention, data = data_legit ))

summary(lm_robust(Toxicity ~ intervention, data = DE_data))
################ Pooled Box Plots #########################################

# boxplot
box_t <- ggplot(data, aes(treatment_group, Toxicity, fill = treatment_group))+
  geom_boxplot()+
  ggtitle("Reply Toxicity")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  guides(fill="none")+
  scale_fill_brewer(palette="Pastel1")

box_l <- ggplot(data, aes(treatment_group, comment_length, fill = treatment_group))+
  geom_boxplot()+
  ggtitle("Comment Lenght")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  guides(fill="none")+
  scale_fill_brewer(palette="Pastel1")

box_t_l <- grid.arrange(box_t, box_l, nrow = 1, top = "Treatment Effects on")

ggsave("output/box_pooled.pdf", box_t_l, width = 12 ) 

# Coefficient Plots

source("code/3-coefficient-plots.R")

# primary hypotheses
cp1
ggsave("output/cp1_main.pdf", cp1, width = 12) 


# secondary hypotheses
cp2
ggsave("output/cp2_main.pdf", cp2, width = 8) 


# Regression Diagnostics 

#source("code/4-heterogeneity-robustness.R")
