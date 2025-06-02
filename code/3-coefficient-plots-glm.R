# Coefficient Plots

# Specify confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

### collect model information into dataframes

# Toxicity simple
m1_modFrame <- data.frame(Variable = rownames(summary(mod1_invgauss)$coef),
                                Coefficient = summary(mod1_invgauss)$coef[, 1],
                                SE = summary(mod1_invgauss)$coef[, 2],
                                Outcome = "Toxicity",
                                Model = "Toxicity")[-1,] # [-1,] removes intercept, delete to include

# Comment length simple
m3_modFrame <- data.frame(Variable = rownames(summary(mod3_invgauss)$coef),
                            Coefficient = summary(mod3_invgauss)$coef[, 1],
                            SE = summary(mod3_invgauss)$coef[, 2],
                            Outcome = "Comment length",
                            Model = "Comment length")[-1,] 

# Attitude distance
m5_modFrame <- data.frame(Variable = rownames(summary(mod5)$coef),
                          Coefficient = summary(mod5)$coef[, 1],
                          SE = summary(mod5)$coef[, 2],
                          Outcome = "Toxicity",
                          Model = "Attitude distance")[-1,] 
# Online activity
m6_modFrame <- data.frame(Variable = rownames(summary(mod6)$coef),
                          Coefficient = summary(mod6)$coef[, 1],
                          SE = summary(mod6)$coef[, 2],
                          Outcome = "Toxicity",
                          Model = "Online activity")[-1,] 


allModelFrame <- data.frame(rbind(m1_modFrame,m3_modFrame))
secondaryModelFrame <- data.frame(rbind(m5_modFrame,m6_modFrame))

# reorder variables (rev because coords are flipped)
allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels=c("interventionfriction",
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

allModelFrame$Model <- factor(allModelFrame$Model, 
                              levels=c("Toxicity", "Comment length"))


## Simplified cp1 

allModelFramesimple <- allModelFrame%>%
  filter(str_detect(Variable,"intervention"))

# Plot Primary Hypotheses
cp1s <- ggplot(allModelFramesimple, aes(x = Variable,colour = Outcome)) + 
  facet_grid( ~ Model)+
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                  lwd = 1/2, position = position_dodge(width = 1/2), fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Toxicity", "Comment length")) +
  scale_shape_manual(values = c(21, 22),limits=c("Toxicity", "Comment length"))+
  scale_x_discrete(labels = c("interventionfriction" = "Friction",
                              "interventionperspective" = "Perspective-taking",
                              "interventionempathy" = "Empathy",
                              "interventionboost" = "Boost"))+
  coord_flip() + 
  theme_bw(base_size = 13)+ 
  ggtitle("Treatment Effects")+
  guides(colour = "none", shape = "none")
#guides(colour = guide_legend(override.aes = list(shape = c(21, 22)))) 

cp1s


# Plot Secondary Hypotheses
cp2 <- ggplot(secondaryModelFrame, aes(x = Variable,colour = Outcome)) + 
  facet_grid( ~ Model)+
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = Outcome),
                  lwd = 1/2, position = position_dodge(width = 1/2), fill = "WHITE") + 
  scale_color_jcolors(palette="pal5",limits=c("Toxicity", "Comment length")) +
  scale_shape_manual(values = c(21, 22),limits=c("Toxicity", "Comment length"))+
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
  guides(colour = "none", shape = "none")
  #guides(colour = guide_legend(override.aes = list(shape = c(21, 22)))) 

cp2
