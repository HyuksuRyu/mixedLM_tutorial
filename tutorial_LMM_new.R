library(lme4)
library(dplyr)
politeness = read.csv(file.choose()) #./material/politeness_data.csv
politeness = read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv") #./material/politeness_data.csv
head(politeness)
politeness$scenario = factor(politeness$scenario)
# if change order of attitude: polite, informal
#politeness$attitude = factor(politeness$attitude, levels = c("pol", "inf"))
summary(politeness)
#politeness = tbl_df(politeness)


# Let's see BIG PICTURE
# mean pitch in terms of subject
politeness %>% 
  group_by(subject) %>%
  summarise(mean=mean(frequency, na.rm=TRUE))
  # na.rm=TRUE option
  # there is a row which contains NA
which(is.na(politeness))

# box plot in terms of subject
with(politeness, boxplot(frequency ~ subject, ylab = 'Frequency(Hz)', xlab ='subject', col='lightgrey'))

# mean pitch in terms of scenario
politeness %>% 
  group_by(scenario) %>%
  summarise(mean=mean(frequency, na.rm=TRUE))
# box plot in terms of subject
with(politeness, boxplot(frequency ~ scenario, ylab = 'Frequency(Hz)', xlab ='scenario', col='lightgrey'))

# mean pitch in terms of attitude and gender
politeness %>% 
  group_by(gender, attitude) %>%
  summarise(mean=mean(frequency, na.rm = TRUE))
boxplot(frequency ~ attitude * gender, col=c("white", "lightgrey"), politeness)


###################
# mixed modeling  #
###################
# w/ fixed effect only
lmer(frequency ~ attitude, data=politeness) # error! need random effect
# add random effect
politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
# let's see what the result is
(politeness.model.summary = summary(politeness.model)) # unlike the tutorial, summary() function is necessary

# random effect
politeness.model.summary$varcor
## std.dev: scenario < subject --> scenario less variation which is confirmed by boxplot
## residual: variability not due to scenario or subject
## "random" deviation / some factors that affect pitch outside the purview

# fixed effect
politeness.model.summary$coefficients
## pitch is lower in polite than in informal speech by approx. 20Hz
## t-value = Estimate/Std.Error
# intercept = average frequency in terms of attitude
politeness %>% 
  group_by(attitude) %>%
  summarise(mean(frequency, na.rm=TRUE))

# renewal of the model: add gender b/c relationship b/w sex & pitch - predictable and systematic
politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data = politeness)
(politeness.model.summary = summary(politeness.model))
# random effect - focus on residuals
politeness.model.summary$varcor # std.dev of subject lowered
# fixed effect
politeness.model.summary$coefficients
# Estimate of intercept - much higher : based on informal & "FEMALE"

#####################
# Significance test #
#####################
# Comparison b/w null and full model
politeness.null = lmer(frequency ~ gender + (1|subject) + (1|scenario), data = politeness, REML=FALSE)
politeness.model = lmer(frequency ~ gender + attitude + (1|subject) + (1|scenario), data = politeness, REML=FALSE)
anova(politeness.null, politeness.model)

# interaction test
politeness.interaction = lmer(frequency ~ gender * attitude + (1|subject) + (1|scenario), data = politeness, REML=FALSE)
anova(politeness.model, politeness.interaction) # no significant inter-dependence
anova(politeness.model, politeness.interaction, politeness.null)

#######################################
# Rnadom slopes vs. random intercepts #
#######################################
coef(politeness.model) # different intercept - random intercept model
# random slope model
politeness.model = lmer(frequency ~ attitude + gender +  (1+attitude|subject) + (1+attitude+gender|scenario), 
                        data=politeness, REML=FALSE)
coef(politeness.model) # random slope model

# anova test for random slope model
politeness.null = lmer(frequency ~ gender +  (1+attitude|subject) + (1+attitude|scenario), 
                        data=politeness, REML=FALSE) # need to set up identical random factor setting
anova(politeness.null, politeness.model)


###############
# Assumptions #
###############

# influential data points
getDFBeta = function(df, id){
  all.res = numeric(nrow(df))
  for(i in 1:nrow(df)){
    myfullmodel = lmer(frequency ~ gender + attitude + (1+attitude|subject) + (
      1+attitude|scenario), df[-i, ])
    all.res[i] = fixef(myfullmodel)[id]
  }
  return(all.res)
}

(all.res.attitude = getDFBeta(politeness, 2))
(all.res.gender = getDFBeta(politeness, 3))

# for all aspects as a data frame
all.res = data.frame()
for(i in 1:nrow(politeness)){
  model.part = lmer(frequency ~ gender + attitude + (1+attitude|subject) + (
    1+attitude|scenario), politeness[-i, ])
  all.res[i,1] = fixef(model.part)[1]
  all.res[i,2] = fixef(model.part)[2]
  all.res[i,3] = fixef(model.part)[3]
}
colnames(all.res) = attributes(fixef(politeness.model))$names
head(all.res)


