# language comparison toxicity levels 
#devtools::install_github("zumbov2/deeplr")
source("code/packages_empathy.R")
library(deeplr)

my_key <- "abcd" # replace with your key

comments_df <- read_excel("data/comments.xlsx")

# sample comments -------------------------------------------------------------
comments_sample_DE <- comments_df %>%
  filter(UserLanguage == "DE")%>%
  sample_n(1000)

comments_sample_EN <- comments_df %>%
  filter(UserLanguage == "EN")%>%
  sample_n(1000)

# translate comments with DeepL ------------------------------------------------
deeplr::usage2(my_key)

#comments_sample_EN$comments_ENtoDE <- deeplr::translate2(
#  text = comments_sample_EN$comment,
#  target_lang = c("DE"),
#  auth_key = my_key
#)

#comments_sample_DE$comments_DEtoEN <- deeplr::translate2(
#  text = comments_sample_DE$comment,
#  target_lang = c("EN"),
#  auth_key = my_key
#)

saveRDS(comments_sample_EN, file = "data/comments_translated_ENDE.rds")
saveRDS(comments_sample_DE, file = "data/comments_translated_DEEN.rds")

# toxicity assessment - Google Perspective API ---------------------------------
# English comments translated to German 
tox_data_ende <- comments_sample_EN%>%
  mutate(new_language = "ENDE",
         rowid = row_number())%>%
  prsp_stream(text = comments_ENtoDE,
              text_id = rowid,
              score_model = c("TOXICITY"),
              safe_output = T,
              languages = "de",
              doNotStore = TRUE)


# German comments translated to English 
tox_data_deen <- comments_sample_DE%>%
  mutate(new_language = "DEEN",
         rowid = row_number())%>%
  prsp_stream(text = comments_DEtoEN,
              text_id = rowid,
              score_model = c("TOXICITY"),
              safe_output = T,
              languages = "en",
              doNotStore = TRUE)

# combine data -----------------------------------------------------------------
comments_sample_DE <- comments_sample_DE%>%
  mutate(new_language = "DEEN",
         rowid = row_number())
comments_sample_EN <- comments_sample_EN%>%
  mutate(new_language = "ENDE",
         rowid = row_number())

data_DE <- left_join(comments_sample_DE, tox_data_deen, by = c("rowid" = "text_id"))
data_EN <- left_join(comments_sample_EN, tox_data_ende, by = c("rowid" = "text_id"))

data_DE <- data_DE%>%rename(comments_translated = comments_DEtoEN)
data_EN <- data_EN%>%rename(comments_translated = comments_ENtoDE)

data <- rbind(data_DE, data_EN)

saveRDS(data, file = "data/comments_translated_toxicity.rds")
write_xlsx(data, "data/comments_translated_toxicity.xlsx")


# Toxicity comparison  --------------------------------------------------------
t.test(Toxicity ~ UserLanguage, data = data)
t.test(TOXICITY ~ new_language, data = data)

# boxplot
box_l1 <- ggplot(data, aes(UserLanguage, Toxicity, fill = UserLanguage))+
  geom_boxplot()+
  ggtitle("Original")+
  xlab("")+
  ylab("Reply Toxicity")+
  theme_minimal()+
  guides(fill="none")+
  scale_x_discrete(labels=c("DE" = "German original", "EN" = "English original"))+
  scale_fill_jcolors(palette="pal5")

box_l2 <- ggplot(data, aes(new_language, TOXICITY, fill = new_language))+
  geom_boxplot()+
  ggtitle("Translated (DeepL)")+
  xlab("")+
  ylab("")+
  theme_minimal()+
  guides(fill="none")+
  scale_x_discrete(labels=c("DEEN" = "English translated \n from German", "ENDE" = "German translated \n from English"))+
  scale_fill_jcolors(palette="pal5")

box_topic <- grid.arrange(box_l1, box_l2, nrow = 1, top = "Language Comparison")

ggsave("output/box_language_comparison.pdf", box_topic, width = 9 ) 

