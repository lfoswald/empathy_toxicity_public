source("code/packages_empathy.R")

#### DATA IMPORT ###################################################################

raw_data <- read_sav("data/raw/Hate-speech-deliberation-experiment_November 3, 2022_03.33.sav")

#### PREPROCESSING ###################################################################

# progress
dropout_plot <- ggplot(raw_data, aes(Progress))+
  geom_bar(fill = "steelblue")+
  ggtitle("Dropout (raw data)")+
  xlab("Percent Complete")
dropout_plot
ggsave("output/dropout_plot.pdf", dropout_plot, width = 12)


# page times
d <- raw_data%>%
  dplyr::select(starts_with("t_") & ends_with("Page_Submit"))%>%
  pivot_longer(
    cols = 1:36,
    names_to = "Page",
    values_to = "Time",
    names_prefix = "t_")

p <-  gsub("_Page_Submit", "", d$Page)

times <- d %>%
  add_column(p)%>%
  group_by(p)%>%
  mutate(median_time = median(Time, na.rm = T))%>%
  slice(1)%>%
  ggplot(aes(x = fct_reorder(p, median_time), y = median_time)) +
  geom_col(fill = "indianred")+
  ggtitle("Answering Times")+
  ylab("Median Time (seconds till page submission)")+
  xlab("Page")+
  theme(axis.text.x = element_text(angle = 90, hjust=1))
times
ggsave("output/survey_times.pdf", times, width = 12)


# filter preview answers
data <- raw_data%>%
  filter(Status == 0)

# filter those who dropped out
data <- data%>%
  filter(Progress > 60)

# only let those who viewed boost long enough (< 5 sek) stay in condition
# move those who missed boosting text to control group
data <- data %>% 
  mutate(treatment_group_n = case_when(t_boost_Page_Submit < 5 ~ "control",
                                       TRUE ~ treatment_group))

# how many moved from boost to control?
data%>%
  dplyr::select(treatment_group,ResponseId)%>%
  group_by(ResponseId)%>%
  slice(1)%>%
  group_by(treatment_group)%>%
  count()

data%>%
  dplyr::select(treatment_group_n,ResponseId)%>%
  group_by(ResponseId)%>%
  slice(1)%>%
  group_by(treatment_group_n)%>%
  count()
#### DEMOGRAPHICS ###################################################################

# language
table(data$UserLanguage)

# response duration
data <- data%>%
  mutate(Duration_minutes = Duration__in_seconds_/60)

m <- round(mean(data$Duration_minutes),2)
md <- round(median(data$Duration_minutes),2)

times2 <- data%>%
  filter(Duration_minutes < 100)%>%
  ggplot(aes(x = Duration_minutes))+
  geom_histogram(bins = 100)+
  geom_vline(xintercept= m , colour="red")+
  geom_vline(xintercept= md , colour="blue")+
  geom_text(aes(m+5, m+300, label = m),color="red")+
  geom_text(aes(md+5, md+250, label = md),color="blue")+
  xlab("Total Response Time (Minutes)" )+
  coord_cartesian(xlim = c(0, 100))+
  ylab("")
times2
ggsave("output/total_times.pdf", times2, width = 12)

# LocationLongitude / LocationLatitude
#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the locations on top
lon <- as.numeric(data$LocationLongitude)
lat <- as.numeric(data$LocationLatitude)
mp <- mp+ geom_point(aes(x=lon, y=lat),color="orange2", size=1.5) 
mp

ggsave("output/location_map.pdf", mp, width = 12)

# gender
# male = 1, female = 2, other = 3
table(data$gender)

data%>%
  tabyl(gender,UserLanguage)%>%
  adorn_totals(c("col", "row"))%>%
  xtable(type = "latex")

# age 
# year of birth: 2 = 2005, 87 = 1920
data$age <- data$birthyear+16
mean(data$age,na.rm=T)
sd(data$age,na.rm=T)

age_plot <- ggplot(data, aes(age))+
  geom_histogram(fill = "seagreen")+
  xlab("Age")
age_plot
ggsave("output/age_plot.pdf",age_plot, width = 5, height = 4)

# education
table(data$education)

educ <- haven::as_factor(data$education)
educ_plot <- ggplot(data, aes(educ))+
  geom_bar(fill = "steelblue")+
  coord_flip()+
  ggtitle("Education")+
  xlab("")
educ_plot

ggsave("output/education_plot.pdf", educ_plot, width = 12) 


############## ONLINE BEHAVIOUR ################################################

display_jcolors("pal5")
jcolors('pal5')

# time online
data$time_online_f <- factor(data$time_online, levels = c(1,2,3,4),
                                labels = c("no time at all", "up to one hour weekly",
                                                        "up to one hour daily", "multiple hours daily"))
t1 <- ggplot(data, aes(time_online_f))+
  geom_bar(fill = "#17377A")+
  xlab("Time online")+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# social media
data$social_media_f <- factor(data$social_media, levels = c(1,2,3,4),
                             labels = c("not at all", "a couple of times per week",
                                        "about once per day", "multiple times per day"))
t2 <- ggplot(data, aes(social_media_f))+
  geom_bar(fill = "#17377A")+
  xlab("Social media usage")+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))


# comments online
data$comments_online_f <- factor(data$comments_online, levels = c(1,2,3,4),
                              labels = c("never", "about once per month",
                                         "about once per week", "almost daily"))
t3 <- ggplot(data, aes(comments_online_f))+
  geom_bar(fill = "#17377A")+
  xlab("Writing comments")+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

activity_plot <- grid.arrange(t1, t2, t3, nrow = 1)

ggsave("output/activity_plot.pdf", activity_plot, width = 16, height = 4)

# opposing views

data$opposing <- haven::as_factor(data$opposing_views_a)
o1 <- ggplot(data, aes(opposing))+
  geom_bar(fill = "indianred")+
  xlab("See oppoing views online")+
  ylab("")


data$angry <- haven::as_factor(data$angry_views)
o2 <- ggplot(data, aes(angry))+
  geom_bar(fill = "indianred")+
  xlab("Views make angry")+
  ylab("")


data$engage <- haven::as_factor(data$opposing_views_b)
o3 <- ggplot(data, aes(engage))+
  geom_bar(fill = "indianred")+
  xlab("Engage with oppoing views")+
  ylab("")


data$enjoy <- haven::as_factor(data$enjoy_countering)
o4 <- ggplot(data, aes(enjoy))+
  geom_bar(fill = "indianred")+
  xlab("Enjoy countering oppoing views")+
  ylab("")

  
opposing_plot <- grid.arrange(o1, o2, o3, o4, nrow = 2, top = "Opposing Views Online")
  
ggsave("output/opposing_plot.pdf", opposing_plot, width = 12 , height = 8) 


# different ways to engage with opposing views

om <- data %>% 
  dplyr::select(engagement_type_1:engagement_type_6)%>%
  rename(`Read carefully` = engagement_type_1,
         Like = engagement_type_2,
         Comment = engagement_type_3,
         `Share privately` = engagement_type_4,
         `Share publicly` = engagement_type_5,
         `Contact author` = engagement_type_6)%>%
  # change 1: convert haven_labelled variables to factors 
  mutate_if(haven::is.labelled, haven::as_factor) %>% 
  pivot_longer(
  cols = 1:6,
  names_to = "Variable",
  values_to = "Engagement"
  ) %>% 
  count(Variable, Engagement) %>% 
  ggplot(aes(x = n, y = Engagement)) +
  facet_wrap(. ~ Variable) +
  geom_col(fill = "indianred")+
  ylab("")+
  ggtitle("Engagement with Opposing Views")

om
ggsave("output/opposing_matrix.pdf", om, width = 12 , height = 8) 



########## POLITICS #############################################################

pol <- ggplot(data)+
  geom_histogram(aes(x = leftright, fill = ..x..), binwidth = 1)+
  theme_minimal()+
  scale_fill_gradient2(low='darkblue', mid='lightgrey', high='darkred', midpoint=6)+
  xlab("Political Orientation")+
  ylab("Frequency")+
  facet_grid(UserLanguage ~ .)+
  theme(legend.position = "none")+
  xlim("far left", "", "", "", "", "", "", "",
       "", "", "far right")

pol
ggsave("output/pol_lefright.pdf", pol, width = 8 , height = 8) 


data$interest <- haven::as_factor(data$polinterest)
polin <- ggplot(data, aes(interest))+
  geom_bar(fill = "steelblue")+
  facet_grid(UserLanguage ~ .)+
  xlab("Political Interest")+
  ylab("")

polin
ggsave("output/pol_interest.pdf", polin, width = 8 , height = 6) 


######## EMPATHY ###############################################################

# descriptives
em <- data %>% 
  dplyr::select(trait_empathy_1:trait_empathy_7)%>%
  rename(`Empathetic person` = trait_empathy_1,
         `Predict feelings` = trait_empathy_2,
         `Take perspective` = trait_empathy_3,
         `Adapt to feelings` = trait_empathy_4,
         `Understand upset*` = trait_empathy_5,
         `Understand offended*` = trait_empathy_6,
         `Insensitive*` = trait_empathy_7)%>%
  # change 1: convert haven_labelled variables to factors 
  mutate_if(haven::is.labelled, haven::as_factor) %>% 
  pivot_longer(
    cols = 1:7,
    names_to = "Variable",
    values_to = "Agreement"
  ) %>% 
  count(Variable, Agreement) %>% 
  ggplot(aes(x = n, y = Agreement)) +
  facet_wrap(. ~ Variable) +
  geom_col(fill = "goldenrod")+
  ylab("")+
  ggtitle("Trait Empathy")
em
ggsave("output/empathy_matrix.pdf", em, width = 12 , height = 12) 

# Itemanalysis

# recode reversed empathy items

data$trait_empathy_5r <- 6 - data$trait_empathy_5
data$trait_empathy_6r <- 6 - data$trait_empathy_6
data$trait_empathy_7r <- 6 - data$trait_empathy_7

set_label(data$trait_empathy_5r) <- "I have a hard time understanding why some things upset people so much."
set_label(data$trait_empathy_6r) <- "I can't always understand why someone felt offended by a comment."
set_label(data$trait_empathy_7r) <- "Others often say I'm insensitive, although I don't always understand why."

empathy.df <- data%>%
  dplyr::select(trait_empathy_1:trait_empathy_4, trait_empathy_5r:trait_empathy_7r)

sjt.itemanalysis(empathy.df, show.shapiro = TRUE, factor.groups.titles = "Trait Empathy")

# Exploratory Factor Analysis

# Parallel analysis
PFX <- fa.parallel(empathy.df,fa="fa")

# Scree plot
empathy.df <- data.frame(na.omit(empathy.df))
fit <- princomp(empathy.df, cor=TRUE)
summary(fit) 
plot(fit,type="lines") 
loadings(fit)

# Force e.g. 1 factor solution
PFA2 <- fa(empathy.df,1) 
PFA2
fa.diagram(PFA2)

data$trait_empathy <- (data$trait_empathy_1 + data$trait_empathy_2 + data$trait_empathy_3 + data$trait_empathy_4 +
                         data$trait_empathy_5r + data$trait_empathy_6r + data$trait_empathy_7r)/7

################ TOPIC ATTITUDES ##############################################

# descriptives
tm <- data %>% 
  dplyr::select(issue_attitudes_1:issue_attitudes_4, UserLanguage)%>%
  rename(`Pro climate change mitigation` = issue_attitudes_1,
         `Against gender-neutral language` = issue_attitudes_2,
         `Against immigration` = issue_attitudes_3,
         `Against abortion` = issue_attitudes_4)%>%
  # change 1: convert haven_labelled variables to factors 
  mutate_if(haven::is.labelled, haven::as_factor) %>% 
  pivot_longer(
    cols = 1:4,
    names_to = "Variable",
    values_to = "Agreement"
  ) %>% 
  count(Variable, Agreement, across()) %>% 
  ggplot(aes(x = n, y = Agreement)) +
  facet_grid(UserLanguage ~ Variable) +
  geom_col(fill = "seagreen")+
  ylab("")+
  ggtitle("Topic Attitudes")

tm
ggsave("output/topic_matrix.pdf", tm, width = 12 , height = 8) 

################### EMOJI ######################################################

# favourites
emo_fav_df <- data%>%
  dplyr::select(intro_favourite_emo)%>%
  mutate_if(haven::is.labelled, haven::as_factor) %>% 
  pivot_longer(
    cols = 1,
    names_to = "Emoji",
    values_to = "Selection"
  ) %>% 
  count(Emoji, Selection) %>%
  as.data.frame()%>%
  dplyr::select(Selection,n)%>%
  na.omit()%>%
  rename(word = Selection, freq = n)

fe <- wordcloud2(data=emo_fav_df)
fe


# offensives
emo_offensive_df <- data%>%
  dplyr::select(intro_offensive_emo)%>%
  mutate_if(haven::is.labelled, haven::as_factor) %>% 
  pivot_longer(
    cols = 1,
    names_to = "Emoji",
    values_to = "Selection"
  ) %>% 
  count(Emoji, Selection) %>%
  as.data.frame()%>%
  dplyr::select(Selection,n)%>%
  na.omit()%>%
  rename(word = Selection, freq = n)

oe <- wordcloud2(data=emo_offensive_df)
oe


############### COMMENTS ######################################################

# careful: use treatment_group or treatment_group_n instead of condition!! 

# sort comments to conditions (pivot to long form)
long_data <- data%>%
  pivot_longer(cols = starts_with("resp_"),
               names_to = c("treatment"),
               names_prefix = "resp_", 
               values_to = "comment"
               )%>%
  rowid_to_column()

# cleaning...
treatment_clean <- long_data%>%
  dplyr::select(treatment)%>%
  pull()%>%
  str_remove_all("_open")%>%
  str_remove_all("_op")

l_df <- cbind(long_data,treatment_clean)

long_data_prepped <- l_df%>%
  # get topic information
  separate(treatment_clean, c("topic","condition"), "_")%>%
  # get comment length
  mutate(comment_length = nchar(comment))%>%
  # get rid of all not-presented conditions
  filter(comment_length > 0)

# Get toxicity scores for comments (run separately for German and English)
# Google Perspective API details in toxicity_material_pretest.R

#tox_data_en <- long_data_prepped%>%
#  filter(UserLanguage == "EN")%>%
#  prsp_stream(text = comment,
#              text_id = rowid,
#              score_model = c("TOXICITY", "SEVERE_TOXICITY"),
#              safe_output = T,
#              languages = "en",
#              doNotStore = TRUE)
#
#tox_data_de <- long_data_prepped%>%
#  filter(UserLanguage == "DE")%>%
#  prsp_stream(text = comment,
#              text_id = rowid,
#              score_model = c("TOXICITY", "SEVERE_TOXICITY"),
#              safe_output = T,
#              languages = "de",
#              doNotStore = TRUE)
#
# clean
#long_data_final <- left_join(long_data_prepped, tox_data_en, by = c("rowid" = "text_id"))%>%
#  left_join(., tox_data_de, by  = c("rowid" = "text_id"))%>%
#  mutate(Toxicity = case_when(TOXICITY.x != "NA" ~ TOXICITY.x,
#                              TOXICITY.y != "NA" ~ TOXICITY.y),
#         Severe_toxicity = case_when(SEVERE_TOXICITY.x != "NA" ~ SEVERE_TOXICITY.x,
#                                     SEVERE_TOXICITY.y != "NA" ~ SEVERE_TOXICITY.y))%>%
#  dplyr::select(-condition,-error.x,-error.y,-TOXICITY.x,-TOXICITY.y,
#         -SEVERE_TOXICITY.x,-SEVERE_TOXICITY.y)
#
# write_xlsx(long_data_final, "data/long_data_toxicity.xlsx")

long_data_final <- read_excel("data/long_data_toxicity.xlsx")

# person and topic level toxicity
long_data_final <- long_data_final%>%
  group_by(ResponseId)%>%
  mutate(person_mean_toxicity = mean(Toxicity, na.rm=T),
         person_max_toxicity = max(Toxicity, na.rm = T),
         person_mean_severe_tox = mean(Severe_toxicity, na.rm=T),
         person_max_severe_tox = max(Severe_toxicity, na.rm=T))%>%
  ungroup()%>%
  group_by(topic)%>%
  mutate(topic_mean_toxicity = mean(Toxicity, na.rm=T),
         topic_max_toxicity = max(Toxicity, na.rm = T),
         topic_mean_severe_tox = mean(Severe_toxicity, na.rm=T),
         topic_max_severe_tox = max(Severe_toxicity, na.rm=T))
  
# person toxicity
pt <- long_data_final%>%
  pivot_longer(
    cols = person_mean_toxicity:person_max_severe_tox,
    names_to = "Scores",
    values_to = "toxicity"
  )%>%
  ggplot(. , aes(x = toxicity, color = Scores, fill = Scores))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  facet_grid(UserLanguage ~.)+
  ggtitle("Google Perspective API Toxicity Scores per Person")+
  theme(legend.position = c(0.8, 0.3))
pt
ggsave("output/person_toxicity.pdf", pt , width = 10, height = 12)


# topic toxicity
tt <- long_data_final%>%
  pivot_longer(
    cols = Toxicity:Severe_toxicity,
    names_to = "Scores",
    values_to = "toxicity"
  )%>%
  mutate(Topic = factor(topic, levels = c("ref","fem","cc"),
                        labels = c("Migration", "Feminism" , "Climate \n Change")))%>%
  ggplot(. , aes(x = toxicity, y = Topic, color = Scores))+
  geom_jitter(alpha = 0.5, size = 5, height = 0.2)+
  facet_grid(. ~ UserLanguage)+
  ggtitle("Google Perspective API Toxicity Scores per Topic")+
  ylab("Comments replying to statements on:")
  
tt
ggsave("output/topic_toxicity.pdf", tt , width = 12, height = 6)


#################### LEGITIMACY ##############################################

long_data_legit <- long_data_final%>%
  pivot_longer(cols = starts_with("legi"),
               names_to = c("legi_condition"),
               values_to = "legitimacy")%>%
  filter(!is.na(legitimacy))%>%
  filter(topic == "fem" & str_detect(legi_condition, "_fem_")|
           topic == "ref" & str_detect(legi_condition, "_ref_")|
         topic == "cc" & str_detect(legi_condition, "_cc_"))%>%
  mutate(Topic = factor(topic, levels = c("ref","fem","cc"),
                        labels = c("Migration", "Feminism" , "Climate Change")))

long_data_legit$Legitimacy <- factor(long_data_legit$legitimacy, labels = c("not at all legitimate",
                                                            "rather not legitimate",
                                                            "undecided",
                                                            "rather legtitimate",
                                                            "fully legitimate"))
legi <- ggplot(long_data_legit, aes(x = Legitimacy))+
  geom_bar(fill = "seagreen3")+
  facet_grid(UserLanguage ~ Topic)+
  ggtitle("Perceived Legitimacy of Attitude Opposing Statement")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
legi
ggsave("output/legitimacy_grid.pdf", legi, width = 12, height = 10)
  
leg <- ggplot(long_data_legit, aes(x = Legitimacy))+
  geom_bar(fill = "seagreen3")+
  ggtitle("Perceived Legitimacy of Attitude Opposing Statement")
leg
ggsave("output/legitimacy.pdf", leg, width = 7, height = 5)


################## MANIPULATION CHECK ########################################

# wrangle manipulation check data... 
long_data_mani_long <- long_data_legit%>%
  pivot_longer(cols = starts_with("mani_"),
               names_to = c("mani_condition"),
               values_to = "manipulation_check")%>%
  filter(!is.na(manipulation_check))%>%
  filter(topic == "fem" & str_detect(mani_condition, "_fem_")|
           topic == "ref" & str_detect(mani_condition, "_ref_")|
           topic == "cc" & str_detect(mani_condition, "_cc_"))

long_data_mani <- long_data_mani_long%>%
  mutate(mani_condition_clean = str_replace(mani_condition, "[^0-9.-]*", ""))%>%
  mutate(mani_type = recode(mani_condition_clean, `1` = "disagree", `2` = "get_angry",
                            `3` = "empathize", `4` = "take_perspective"))%>%
  dplyr::select(-mani_condition, -mani_condition_clean)%>%
  pivot_wider(names_from = mani_type, values_from = manipulation_check)%>%
  # and in this course, also extract which valence of the post people saw:
  mutate(post_direction = case_when(str_detect(legi_condition, "_a") ~ "a",
                                    str_detect(legi_condition, "_b") ~ "b"))

mc <- long_data_mani %>%
  ungroup()%>%
  dplyr::select(disagree:take_perspective)%>%
  # change 1: convert haven_labelled variables to factors 
  mutate_if(haven::is.labelled, haven::as_factor) %>% 
  pivot_longer(
    cols = 1:4,
    names_to = "Variable",
    values_to = "Reaction"
  ) %>% 
  count(Variable, Reaction) %>% 
  ggplot(aes(x = n, y = Reaction)) +
  facet_wrap(. ~ Variable) +
  geom_col(fill = "steelblue3")+
  ylab("")+
  ggtitle("Manipulation Check")

mc
ggsave("output/manipulation_matrix.pdf", mc, width = 12 , height = 12) 


mc2 <- ggplot(long_data_mani, aes(x = empathize)) +
  geom_bar(fill = "steelblue3")+
  facet_grid(. ~ treatment_group) +
  ylab("")+
  xlab("For a moment, I could share the feelings of the author. \n 1 = very much, 5 = not at all")+
  ggtitle("Manipulation Check - Empathy")

mc2
ggsave("output/manipulation_empathy.pdf", mc2, width = 12 , height = 6) 

mc3 <- ggplot(long_data_mani, aes(x = take_perspective)) +
  geom_bar(fill = "steelblue3")+
  facet_grid(. ~ treatment_group) +
  ylab("")+
  xlab("I can understand the perspective of the author. \n 1 = very much, 5 = not at all")+
  ggtitle("Manipulation Check - Perspective Taking")

mc3
ggsave("output/manipulation_perspective.pdf", mc3, width = 12 , height = 6) 


########### SAVE final data #######################################
data_final <- long_data_mani
write_xlsx(data_final, "data/data_final_preprocessed.xlsx")

# Write Comments to Excel 
data_final%>%
  dplyr::select(ResponseId,topic,treatment_group,treatment_group_n,comment,Toxicity)%>%
  write_xlsx("data/comments.xlsx")

