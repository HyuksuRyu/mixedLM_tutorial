require(xtable)
require(lattice)
require(lme4)
politeness= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
politeness$scenario = factor(politeness$scenario)
with(politeness, plot(frequency~subject, col ='lightgrey'))
with(politeness, plot(frequency~scenario, col='lightgrey'))
## install.packages("lme4") # Linear Mixed Effect Model
## library(lme4)
politeness=read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
summary(politeness)
politeness$scenario = factor(politeness$scenario)
(idx = which(is.na(politeness$frequency)))
politeness = politeness[-idx,]
summary(politeness)
boxplot(frequency ~ attitude*gender,politeness, col = c("white", "lightgrey"))
politeness.model = lmer(frequency ~ attitude + (1|subject) + 
	(1|scenario), data=politeness, REML=FALSE)
## summary(politeness.model)
summary(politeness.model)
summary(politeness.model)$AICtab
summary(politeness.model)$coefficients
politeness.model.2 = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness)
## summary(politeness.model.2)
summary(politeness.model.2)
summary(politeness.model.2)$coefficients
politeness.null = lmer(frequency ~ gender + (1|subject) 
	+ (1|scenario), data=politeness, REML=FALSE)
politeness.model = lmer(frequency ~ attitude + gender 
	+ (1|subject) + (1|scenario), data=politeness, REML=FALSE)
## anova(politeness.null, politeness.model)
anova(politeness.null, politeness.model)
politeness.model = lmer(frequency ~ attitude + gender + (1+attitude|subject)
	+ (1+attitude|scenario), data=politeness, REML=FALSE)
politeness.inter = lmer(frequency ~ attitude * gender + (1+attitude|subject)
	+ (1+attitude|scenario), data=politeness, REML=FALSE)
anova(politeness.model, politeness.inter)
## coef(politeness.model)
coef(politeness.model)
politeness.model = lmer(frequency ~ attitude + gender 
                        + (1+attitude|subject) + (1+attitude|scenario),
                        data=politeness, REML=FALSE)
## coef(politeness.model)
coef(politeness.model)
politeness.model = lmer(frequency ~ attitude + gender 
                        + (1+attitude|subject)+(1+attitude|scenario), 
                        data=politeness, REML=FALSE)
politeness.null = lmer(frequency ~ gender 
                       + (1+attitude|subject) + (1+attitude|scenario), 
                       data=politeness, REML = FALSE)
anova(politeness.null, politeness.model)
## all.res.attitude = numeric(nrow(politeness))
## all.res.gender = numeric(nrow(politeness))
## 
## for(i in 1:nrow(politeness)){
##   myfullmodel = lmer(frequency ~ attitude + gender +
##                        (1+attitude|subject)+(1+attitude|scenario),
##                    data=politeness[-i,], REML=FALSE)
##   # 1- intercept, 2-attitude, 3-gender
##   all.res.attitude[i] = fixef(myfullmodel)[2]
##   all.res.gender[i] = fixef(myfullmodel)[3] #
## }
citation()
citation('lme4')
