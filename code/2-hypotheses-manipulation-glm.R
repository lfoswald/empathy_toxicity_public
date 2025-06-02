source("code/packages_empathy.R")

################# GET DATA ####################################################

data <- read_excel("data/data_final_preprocessed.xlsx")

################ Analysis Preparation ##################################

data$intervention <- factor(data$treatment_group, levels = c("control","empathy","perspective","friction","boost"))

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

data[c("birthyear", "education", "polinterest", 
       "leftright", "time_online", 
       "trait_empathy", "issue_attitudes_1",
       "issue_attitudes_2", "issue_attitudes_3", 
       "issue_attitudes_4", "online_activity",
       "social_media", "comments_online", 
       "angry_views", "opposing_views_a", 
       "attitude_distance", "opposing_views_b")] <- lapply(data[c("birthyear", "education", "polinterest", 
                                                                  "leftright", "time_online", 
                                                                  "trait_empathy", "issue_attitudes_1",
                                                                  "issue_attitudes_2", "issue_attitudes_3", 
                                                                  "issue_attitudes_4", "online_activity",
                                                                  "social_media", "comments_online", 
                                                                  "angry_views", "opposing_views_a", 
                                                                  "attitude_distance", "opposing_views_b")], gelman_scale)


# create subsamples for those who pass certain manipulation checks

# 0. entire sample
data_total <- data

# 1. who disagree with the post ("very much / rather yes")
data_disagree <- data%>%
  filter(disagree < 3)

# 2. who are in treatment condition and empathise or take perspective
data_manipulation <- data%>%
  filter(treatment_group == "control" | treatment_group == "friction" |
           (treatment_group == "boost" & (empathize < 3 | take_perspective < 3)) | 
           (treatment_group == "empathy" & (empathize < 3 | take_perspective < 3)) |
           (treatment_group == "perspective" & (empathize < 3 | take_perspective < 3)))

# 3. who disagree and empathize correctly
data_disagree_manipulation <- data%>%
  filter(disagree < 3)%>%
  filter(treatment_group == "control" | treatment_group == "friction" |
           (treatment_group == "boost" & (empathize < 3 | take_perspective < 3)) | 
           (treatment_group == "empathy" & (empathize < 3 | take_perspective < 3)) |
           (treatment_group == "perspective" & (empathize < 3 | take_perspective < 3)))

# 4. remove those with short answering times (as falsely specified in PAP)
data_pap <- data%>%
  filter(Duration__in_seconds_ > 0.3*mean(Duration__in_seconds_))


##############  0. Models - RAW ##########################################

# toxicity simple 
mod01 <- glm(Toxicity ~ intervention, family = inverse.gaussian(link = "log"), data = data_total)
# toxicity adjusted
mod02 <- lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
                    issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance ,
                  data = data_total)
# comment length simple
mod03 <- glm(comment_length ~ intervention,                              family = inverse.gaussian(link = "log"), data = data_total)
# comment length adjusted
mod04 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance, 
                  data = data_total)
# attitude distance 
mod05 <- lm_robust(Toxicity ~  intervention* attitude_distance, data = data_total)

# test activity hypothesis
mod06 <- lm_robust(Toxicity ~ online_activity, data = data_total)


##############  1. Models - data_disagree  ##########################################

# toxicity simple 
mod11 <- glm(Toxicity ~ intervention,                              family = inverse.gaussian(link = "log"), data = data_disagree)
# toxicity adjusted
mod12 <- lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
                    issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance ,
                  data = data_disagree)
# comment length simple
mod13 <- glm(comment_length ~ intervention,                              family = inverse.gaussian(link = "log"), data = data_disagree)
# comment length adjusted
mod14 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance, 
                   data = data_disagree)
# attitude distance 
mod15 <- lm_robust(Toxicity ~  intervention* attitude_distance, data = data_disagree)

# test activity hypothesis
mod16 <- lm_robust(Toxicity ~ online_activity, data = data_disagree)


##############  2. Models - data_manipulation ##########################################

# toxicity simple 
mod21 <- glm(Toxicity ~ intervention,                              family = inverse.gaussian(link = "log"), data = data_manipulation)
# toxicity adjusted
mod22 <- lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
                    issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance ,
                  data = data_manipulation)
# comment length simple
mod23 <- glm(comment_length ~ intervention,                              family = inverse.gaussian(link = "log"), data = data_manipulation)
# comment length adjusted
mod24 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance, 
                   data = data_manipulation)
# attitude distance 
mod25 <- lm_robust(Toxicity ~  intervention* attitude_distance, data = data_manipulation)

# test activity hypothesis
mod26 <- lm_robust(Toxicity ~ online_activity, data = data_manipulation)


##############  3. Models - data_disagree_manipulation ##########################################
summary(aov(Toxicity ~ intervention , data = data_disagree_manipulation))

summary(aov(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
              issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  
              angry_views + attitude_distance, data = data_disagree_manipulation))

# toxicity simple 
mod31 <- glm(Toxicity ~ intervention,                              family = inverse.gaussian(link = "log"), data = data_disagree_manipulation)
# toxicity adjusted
mod32 <- lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
                    issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance ,
                  data = data_disagree_manipulation)
# comment length simple
mod33 <- glm(comment_length ~ intervention,                              family = inverse.gaussian(link = "log"), data = data_disagree_manipulation)
# comment length adjusted
mod34 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance,
                   data = data_disagree_manipulation)
# attitude distance 
mod35 <- lm_robust(Toxicity ~  intervention* attitude_distance, data = data_disagree_manipulation)

# test activity hypothesis
mod36 <- lm_robust(Toxicity ~ online_activity, data = data_disagree_manipulation)


##############  4. Models - data_pap  ##########################################

# toxicity simple 
mod41 <- glm(Toxicity ~ intervention,                              family = inverse.gaussian(link = "log"), data = data_pap)
# toxicity adjusted
mod42 <- lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
                    issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance ,
                  data = data_pap)
# comment length simple
mod43 <- glm(comment_length ~ intervention,  family = inverse.gaussian(link = "log"), data = data_pap)
# comment length adjusted
mod44 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance, 
                   data = data_pap)
# attitude distance 
mod45 <- lm_robust(Toxicity ~  intervention* attitude_distance, data = data_pap)

# test activity hypothesis
mod46 <- lm_robust(Toxicity ~ online_activity, data = data_pap)


######### Coefficient Plots####################################################


source("code/3-coefficient-plots-manipulation-glm.R")

# primary hypotheses
cp1s
ggsave("output/cp1_manipulation_simple_glm.pdf", cp1s, width = 10, height = 5) 

# secondary hypotheses
cp2
ggsave("output/cp2_manipulation_glm.pdf", cp2, width = 8) 


