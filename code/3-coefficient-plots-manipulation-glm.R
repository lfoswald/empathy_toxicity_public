# Coefficient Plots (Different Variations of Sample / Robustness Check)

# Specify confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

### collect model information into dataframes

########################### RAW DATA ##########################################

# Toxicity simple
m01_modFrame <- data.frame(Variable = rownames(summary(mod01)$coef),
                                Coefficient = summary(mod01)$coef[, 1],
                                SE = summary(mod01)$coef[, 2],
                                Outcome = "Toxicity",
                                Data = "Raw Data",
                                Model = "Toxicity")[-1,] # [-1,] removes itercept, delete to include

# Comment length simple
m03_modFrame <- data.frame(Variable = rownames(summary(mod03)$coef),
                            Coefficient = summary(mod03)$coef[, 1],
                            SE = summary(mod03)$coef[, 2],
                            Outcome = "Comment length",
                          Data = "Raw Data",
                            Model = "Comment length")[-1,] 

# Attitude distance
m05_modFrame <- data.frame(Variable = rownames(summary(mod05)$coef),
                          Coefficient = summary(mod05)$coef[, 1],
                          SE = summary(mod05)$coef[, 2],
                          Outcome = "Toxicity",
                          Data = "Raw Data",
                          Model = "Attitude distance")[-1,] 
# Online activity
m06_modFrame <- data.frame(Variable = rownames(summary(mod06)$coef),
                          Coefficient = summary(mod06)$coef[, 1],
                          SE = summary(mod06)$coef[, 2],
                          Outcome = "Toxicity",
                          Data = "Raw Data",
                          Model = "Online activity")[-1,] 


################ 1. Disagreement ###################################

# Toxicity simple
m11_modFrame <- data.frame(Variable = rownames(summary(mod11)$coef),
                           Coefficient = summary(mod11)$coef[, 1],
                           SE = summary(mod11)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Disagreement",
                           Model = "Toxicity")[-1,] # [-1,] removes itercept, delete to include

# Comment length simple
m13_modFrame <- data.frame(Variable = rownames(summary(mod13)$coef),
                           Coefficient = summary(mod13)$coef[, 1],
                           SE = summary(mod13)$coef[, 2],
                           Outcome = "Comment length",
                           Data = "Disagreement",
                           Model = "Comment length")[-1,] 

# Attitude distance
m15_modFrame <- data.frame(Variable = rownames(summary(mod15)$coef),
                           Coefficient = summary(mod15)$coef[, 1],
                           SE = summary(mod15)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Disagreement",
                           Model = "Attitude distance")[-1,] 
# Online activity
m16_modFrame <- data.frame(Variable = rownames(summary(mod16)$coef),
                           Coefficient = summary(mod16)$coef[, 1],
                           SE = summary(mod16)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Disagreement",
                           Model = "Online activity")[-1,] 


##################### 2. Manipulation Check #################################

# Toxicity simple
m21_modFrame <- data.frame(Variable = rownames(summary(mod21)$coef),
                           Coefficient = summary(mod21)$coef[, 1],
                           SE = summary(mod21)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Manipulation",
                           Model = "Toxicity")[-1,] # [-1,] removes itercept, delete to include

# Comment length simple
m23_modFrame <- data.frame(Variable = rownames(summary(mod23)$coef),
                           Coefficient = summary(mod23)$coef[, 1],
                           SE = summary(mod23)$coef[, 2],
                           Outcome = "Comment length",
                           Data = "Manipulation",
                           Model = "Comment length")[-1,] 

# Attitude distance
m25_modFrame <- data.frame(Variable = rownames(summary(mod25)$coef),
                           Coefficient = summary(mod25)$coef[, 1],
                           SE = summary(mod25)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Manipulation",
                           Model = "Attitude distance")[-1,] 
# Online activity
m26_modFrame <- data.frame(Variable = rownames(summary(mod26)$coef),
                           Coefficient = summary(mod26)$coef[, 1],
                           SE = summary(mod26)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Manipulation",
                           Model = "Online activity")[-1,] 


##################### 3. Disagreement + Manipulation Check #################################

# Toxicity simple
m31_modFrame <- data.frame(Variable = rownames(summary(mod31)$coef),
                           Coefficient = summary(mod31)$coef[, 1],
                           SE = summary(mod31)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Disagreement + Manipulation",
                           Model = "Toxicity")[-1,] # [-1,] removes itercept, delete to include

# Comment length simple
m33_modFrame <- data.frame(Variable = rownames(summary(mod33)$coef),
                           Coefficient = summary(mod33)$coef[, 1],
                           SE = summary(mod33)$coef[, 2],
                           Outcome = "Comment length",
                           Data = "Disagreement + Manipulation",
                           Model = "Comment length")[-1,] 


# Attitude distance
m35_modFrame <- data.frame(Variable = rownames(summary(mod35)$coef),
                           Coefficient = summary(mod35)$coef[, 1],
                           SE = summary(mod35)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Disagreement + Manipulation",
                           Model = "Attitude distance")[-1,] 
# Online activity
m36_modFrame <- data.frame(Variable = rownames(summary(mod36)$coef),
                           Coefficient = summary(mod36)$coef[, 1],
                           SE = summary(mod36)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Disagreement + Manipulation",
                           Model = "Online activity")[-1,] 

##################### 4. PAP Answering Times  #################################

# Toxicity simple
m41_modFrame <- data.frame(Variable = rownames(summary(mod41)$coef),
                           Coefficient = summary(mod41)$coef[, 1],
                           SE = summary(mod41)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Answering Time PAP",
                           Model = "Toxicity")[-1,] # [-1,] removes itercept, delete to include

# Comment length simple
m43_modFrame <- data.frame(Variable = rownames(summary(mod43)$coef),
                           Coefficient = summary(mod43)$coef[, 1],
                           SE = summary(mod43)$coef[, 2],
                           Outcome = "Comment length",
                           Data = "Answering Time PAP",
                           Model = "Comment length")[-1,] 

# Attitude distance
m45_modFrame <- data.frame(Variable = rownames(summary(mod45)$coef),
                           Coefficient = summary(mod45)$coef[, 1],
                           SE = summary(mod45)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Answering Time PAP",
                           Model = "Attitude distance")[-1,] 
# Online activity
m46_modFrame <- data.frame(Variable = rownames(summary(mod46)$coef),
                           Coefficient = summary(mod46)$coef[, 1],
                           SE = summary(mod46)$coef[, 2],
                           Outcome = "Toxicity",
                           Data = "Answering Time PAP",
                           Model = "Online activity")[-1,] 


allModelFrame <- data.frame(rbind(m01_modFrame, m03_modFrame,
                                  m11_modFrame, m13_modFrame,
                                  m21_modFrame, m23_modFrame,
                                  m31_modFrame, m33_modFrame,
                                  m41_modFrame, m43_modFrame))


secondaryModelFrame <- data.frame(rbind(m05_modFrame,m06_modFrame,
                                        m15_modFrame,m16_modFrame,
                                        m25_modFrame,m26_modFrame,
                                        m35_modFrame,m36_modFrame,
                                        m45_modFrame,m46_modFrame))

# reorder variables (rev because coords are flipped)
allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels=c( "interventionfriction",
                                          "interventionperspective","interventionempathy","interventionboost"))

secondaryModelFrame$Variable <- factor(secondaryModelFrame$Variable, 
                                 levels=c("interventionperspective:attitude_distance",
                                          "interventionfriction:attitude_distance",
                                          "interventionempathy:attitude_distance",
                                          "interventionboost:attitude_distance",
                                          "attitude_distance",
                                          "online_activity",
                                          "interventionfriction",
                                          "interventionperspective","interventionempathy","interventionboost"))

allModelFrame$Model <- factor(allModelFrame$Model,  levels=c("Toxicity", "Comment length"))




## Simplified cp1 

allModelFramesimple <- allModelFrame%>%
  filter(str_detect(Variable,"intervention"))

# Plot Primary Hypotheses
cp1s <- ggplot(allModelFramesimple, aes(x = Variable, colour = Data)) + 
  facet_wrap( ~ Model)+
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Data),
                  lwd = 1/2, position = position_dodge(width = 1/2), fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Raw Data", "Disagreement", "Manipulation",
                                              "Disagreement + Manipulation", "Answering Time PAP")) +
  scale_shape_manual(values = c(21, 22, 24, 16,17),limits=c("Raw Data", "Disagreement", "Manipulation",
                                                            "Disagreement + Manipulation", "Answering Time PAP"))+
  scale_x_discrete(labels = c("interventionfriction" = "Friction",
                              "interventionperspective" = "Perspective-taking",
                              "interventionempathy" = "Empathy",
                              "interventionboost" = "Boost"))+
  xlab("")+
  ylab("")+
  coord_flip() + 
  theme_bw(base_size = 15)+ 
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22,24, 16, 17)))) 

cp1s

# Plot Secondary Hypotheses
cp2 <- ggplot(secondaryModelFrame, aes(x = Variable,colour = Data)) + 
  facet_grid( ~ Model)+
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Data),
                  lwd = 1/2, position = position_dodge(width = 1/2), fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Raw Data", "Disagreement", "Manipulation",
                                              "Disagreement + Manipulation", "Answering Time PAP")) +
  scale_shape_manual(values = c(21, 22, 24, 16, 17),limits=c("Raw Data", "Disagreement", "Manipulation",
                                                 "Disagreement + Manipulation", "Answering Time PAP"))+
  scale_x_discrete(labels = c("interventionperspective:attitude_distance" = "Perspective X Distance",
                              "interventionfriction:attitude_distance" = "Friction X Distance",
                              "interventionempathy:attitude_distance" = "Empathy X Distance",
                              "interventionboost:attitude_distance" = "Boost X Distance",
                              "attitude_distance" = "Attitude distance",
                              "online_activity" = "Online activity",
                              "time_online" = "Time spent online",
                              "interventionfriction" = "Friction",
                              "interventionperspective" = "Perspective-taking",
                              "interventionempathy" = "Empathy",
                              "interventionboost" = "Boost"))+
  coord_flip() + 
  theme_bw()+ 
  ggtitle("Secondary Hypotheses - Outcome: Toxicity")+
  guides(colour = guide_legend(override.aes = list(shape = c(21, 22, 24, 16, 17)))) 

cp2
