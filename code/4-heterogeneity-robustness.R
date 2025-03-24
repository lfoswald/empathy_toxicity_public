### Regression Diagnostics #####

# M1

#normal distrib errors
mod1 <- lm(Toxicity ~ intervention, data = data)
hist(residuals(mod1), xlab = 'Residuals') #looks fine

#constant variance
plot(mod1$fitted.values, residuals(mod1)) #use robust se

#high leverage observations 
#high leverage_obs
lev_obs <- function(modl, data) {
  d1 <- cooks.distance(modl)
  r <- stdres(modl)
  a <- cbind(data, d1, r)
  return(a)
}

out_data <- lev_obs(mod1, data)
nrow(out_data[out_data$d1 > 4/nobs(mod1), ])#num outliers

#non-outlier regression
m1.no.outliers <-  lm_robust(Toxicity ~ intervention,
                             data = out_data[out_data$d1 <= 4/nobs(mod1),]) 
summary(m1.no.outliers)


# M2
mod2 <- lm(Toxicity ~ intervention  + gender + education + topic + post_direction +
              UserLanguage  + issue_attitudes_2 + issue_attitudes_3  + social_media +
              comments_online +  angry_views + attitude_distance ,data = data)

#normal distrib errors
mod2b <- lm(Toxicity ~ intervention  + gender + education + topic + post_direction +
           UserLanguage  + issue_attitudes_2 + issue_attitudes_3  + social_media +
           comments_online +  angry_views + attitude_distance ,
           data = data, na.action=na.exclude)

hist(residuals(mod2), xlab = 'Residuals') #looks fine

#constant variance
plot(mod2$fitted.values, residuals(mod2)) #use robust se

#high leverage observations 
out_data <- lev_obs(mod2b, data)
nrow(out_data[out_data$d1 > 4/nobs(mod2b), ])#num outliers

#non-outlier regression
m2.no.outliers <-  lm_robust(Toxicity ~ intervention  + gender + education + topic + post_direction +
                               UserLanguage  + issue_attitudes_2 + issue_attitudes_3  + social_media +
                               comments_online +  angry_views + attitude_distance ,
                             data = out_data[out_data$d1 <= 4/nobs(mod2b),]) 

summary(m2.no.outliers)

# M3

#normal distrib errors
mod3 <- lm(comment_length ~ intervention, data = data)
hist(residuals(mod3), xlab = 'Residuals') #looks fine

#constant variance
plot(mod3$fitted.values, residuals(mod3)) #use robust se

#high leverage observations 
out_data <- lev_obs(mod3, data)
nrow(out_data[out_data$d1 > 4/nobs(mod3), ])#num outliers

#non-outlier regression
m3.no.outliers <-  lm_robust(comment_length ~ intervention,
                             data = out_data[out_data$d1 <= 4/nobs(mod3),]) 

summary(m3.no.outliers)

# M4

mod4 <- lm(comment_length ~ intervention + birthyear + gender+ education + polinterest + 
              post_direction + time_online + UserLanguage + trait_empathy +
              issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + 
              social_media + comments_online + opposing_views_a + attitude_distance, data = data)

#normal distrib errors
mod4b <- lm(comment_length ~ intervention + birthyear + gender+ education + polinterest + 
  post_direction + time_online + UserLanguage + trait_empathy +
  issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + 
  social_media + comments_online + opposing_views_a + attitude_distance, data = data,
  na.action=na.exclude)

hist(residuals(mod4), xlab = 'Residuals') #looks fine

#constant variance
plot(mod4$fitted.values, residuals(mod4)) #use robust se

#high leverage observations 
out_data <- lev_obs(mod4b, data)
nrow(out_data[out_data$d1 > 4/nobs(mod4b), ])#num outliers

#non-outlier regression
m4.no.outliers <-  lm_robust(comment_length ~ intervention + birthyear + gender+ education + polinterest + 
                               post_direction + time_online + UserLanguage + trait_empathy +
                               issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + 
                               social_media + comments_online + opposing_views_a + attitude_distance,
                             data = out_data[out_data$d1 <= 4/nobs(mod4b),]) 

summary(m4.no.outliers)


texreg(list(m1.no.outliers, m2.no.outliers, m3.no.outliers, m4.no.outliers),
       include.ci = FALSE, single.row = TRUE)


### Effect Heterogeneity ####

mod2_het <- lm_robust(Toxicity ~ intervention * (gender + education + topic + post_direction +
             UserLanguage  + issue_attitudes_2 + issue_attitudes_3  + social_media +
             comments_online +  angry_views + attitude_distance) , data = data)

mod4_het <- lm(comment_length ~ intervention * (birthyear + gender+ education + polinterest + 
             post_direction + time_online + UserLanguage + trait_empathy +
             issue_attitudes_2 + issue_attitudes_3 + issue_attitudes_4  + 
             social_media + comments_online + opposing_views_a + attitude_distance) , data = data)


texreg(list(mod2_het,mod4_het),include.ci = FALSE, single.row = TRUE)


