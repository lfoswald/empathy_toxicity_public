source("code/packages_empathy.R")

################# GET DATA ####################################################

data <- read_excel("data/data_final_preprocessed.xlsx")
comments <- read_excel("data/comments.xlsx")

# how many would have been dropped?
data_PAP <- data%>%
  filter(Duration__in_seconds_ > 0.3*mean(Duration__in_seconds_))

########## RAW EFFECTS BOXPLOTS ###########################################
data$treatment_group_fct <- factor(data$treatment_group, levels = c("empathy","perspective","friction","boost","control"))

# Reply Toxicity plot with half boxplot and jitter
box_tr <- ggplot(data, aes(x = treatment_group_fct, y = Toxicity, color = treatment_group_fct)) +
  geom_boxplot(width = 0.3, position = position_nudge(x = -0.2), 
               alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(color = treatment_group_fct), 
              position = position_jitternudge(nudge.x = 0.2, jitter.width = 0.3), 
              alpha = 0.2, size = 1.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, stroke = 1,
               fill = "white", position = position_nudge(x = 0.2)) +
  ggtitle("Reply Toxicity") +
  xlab("") +
  ylab("") +
  theme_bw(base_size = 15) +
  guides(fill = "none", color = "none") +
  scale_color_jcolors(palette = "pal5")

# Comment Length plot with half boxplot and jitter
box_lr <- ggplot(data, aes(x = treatment_group_fct, y = comment_length, color = treatment_group_fct)) +
  geom_boxplot(width = 0.3, position = position_nudge(x = -0.2), 
               alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(color = treatment_group_fct), 
              position = position_jitternudge(nudge.x = 0.2, jitter.width = 0.3), 
              alpha = 0.2, size = 1.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, stroke = 1,
               fill = "white", position = position_nudge(x = 0.2)) +
  ggtitle("Comment Length") +
  scale_y_continuous(limits = c(0,1000))+
  xlab("") +
  ylab("") +
  theme_bw(base_size = 15) +
  guides(fill = "none", color = "none") +
  scale_color_jcolors(palette = "pal5")

# Log Comment Length plot with half boxplot and jitter
box_lr_log <- ggplot(data, aes(x = treatment_group_fct, y = comment_length, color = treatment_group_fct)) +
  geom_boxplot(width = 0.3, position = position_nudge(x = -0.2), 
               alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(color = treatment_group_fct), 
              position = position_jitternudge(nudge.x = 0.2, jitter.width = 0.3), 
              alpha = 0.2, size = 1.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, stroke = 1,
               fill = "white", position = position_nudge(x = 0.2)) +
  ggtitle("Comment Length") +
  scale_y_log10(limits = c(1,1000))+
  xlab("") +
  ylab("") +
  theme_bw(base_size = 15) +
  guides(fill = "none", color = "none") +
  scale_color_jcolors(palette = "pal5")


# Combine the plots
box_t_l_raw <- grid.arrange(box_tr, box_lr_log, nrow = 1, top = "")
ggsave("output/box_pooled_raw.pdf", box_t_l_raw, width = 12, height = 5 ) 


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

# 2. who are in treatment condition and empathize or take perspective
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
mod03 <- glm(comment_length ~ intervention,  family = inverse.gaussian(link = "log"), data = data_total)

# comment length adjusted
mod04 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance, 
             data = data_total)

# attitude distance 
mod05 <- glm(Toxicity ~  intervention* attitude_distance, family = inverse.gaussian(link = "log"), data = data_total)

# test activity hypothesis
mod06 <- glm(Toxicity ~ online_activity, family = inverse.gaussian(link = "log"), data = data_total)


### Regression tables
texreg(list(mod01, mod02, mod03, mod04), include.ci = FALSE, single.row = TRUE,
       custom.model.names = c("Toxicity glm","Toxicity lasso","Length glm","Length lasso"))

texreg(list(mod05, mod06), include.ci = FALSE, single.row = TRUE,
       custom.model.names = c("Toxicity and attitude distance","Toxicity and online activity"))

##############  1. Models - data_disagree  ##########################################

# toxicity simple 
mod11 <- glm(Toxicity ~ intervention,  family = inverse.gaussian(link = "log"), data = data_disagree)
# toxicity adjusted
mod12 <- lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
                    issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance ,
                   data = data_disagree)
# comment length simple
mod13 <- glm(comment_length ~ intervention, family = inverse.gaussian(link = "log"), data = data_disagree)
# comment length adjusted
mod14 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance, 
             data = data_disagree)
# attitude distance 
mod15 <- glm(Toxicity ~  intervention* attitude_distance, family = inverse.gaussian(link = "log"), data = data_disagree)

# test activity hypothesis
mod16 <- glm(Toxicity ~ online_activity,family = inverse.gaussian(link = "log"),  data = data_disagree)


##############  2. Models - data_manipulation ##########################################

# toxicity simple 
mod21 <- glm(Toxicity ~ intervention, family = inverse.gaussian(link = "log"), data = data_manipulation)
# toxicity adjusted
mod22 <- lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
                    issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance ,
                   data = data_manipulation)
# comment length simple
mod23 <- glm(comment_length ~ intervention, family = inverse.gaussian(link = "log"), data = data_manipulation)
# comment length adjusted
mod24 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance, 
                   data = data_manipulation)
# attitude distance 
mod25 <- glm(Toxicity ~  intervention* attitude_distance, family = inverse.gaussian(link = "log"), data = data_manipulation)

# test activity hypothesis
mod26 <- glm(Toxicity ~ online_activity,family = inverse.gaussian(link = "log"),  data = data_manipulation)


##############  3. Models - data_disagree_manipulation ##########################################

# toxicity simple 
mod31 <- glm(Toxicity ~ intervention,  family = inverse.gaussian(link = "log"), data = data_disagree_manipulation)
# toxicity adjusted
mod32 <- lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction + UserLanguage  +
                    issue_attitudes_2 + issue_attitudes_3  + social_media +  comments_online +  angry_views + attitude_distance ,
                    data = data_disagree_manipulation)
# comment length simple
mod33 <- glm(comment_length ~ intervention,  family = inverse.gaussian(link = "log"), data = data_disagree_manipulation)
# comment length adjusted
mod34 <- lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + post_direction + time_online + UserLanguage + trait_empathy +
                    issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + social_media + comments_online + opposing_views_a + attitude_distance,
             data = data_disagree_manipulation)
# attitude distance 
mod35 <- glm(Toxicity ~  intervention* attitude_distance,family = inverse.gaussian(link = "log"),  data = data_disagree_manipulation)

# test activity hypothesis
mod36 <- glm(Toxicity ~ online_activity,family = inverse.gaussian(link = "log"),  data = data_disagree_manipulation)


##############  4. Models - data_pap  ##########################################

# toxicity simple 
mod41 <- glm(Toxicity ~ intervention, family = inverse.gaussian(link = "log"), data = data_pap)
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
mod45 <- glm(Toxicity ~  intervention* attitude_distance, family = inverse.gaussian(link = "log"), data = data_pap)

# test activity hypothesis
mod46 <- glm(Toxicity ~ online_activity,family = inverse.gaussian(link = "log"),  data = data_pap)


######### Coefficient Plots####################################################


source("code/3-coefficient-plots-manipulation-glm.R")

# primary hypotheses
cp1s
ggsave("output/cp1_manipulation_simple_glm.pdf", cp1s, width = 10, height = 5) 

# secondary hypotheses
cp2
ggsave("output/cp2_manipulation_glm.pdf", cp2, width = 8) 


