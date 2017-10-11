require(lme4)
politeness= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
politeness$scenario = factor(politeness$scenario)
head(politeness)
dim(politeness)

summary(politeness)

with(politeness, plot(frequency~subject, col ='lightgrey'))
with(politeness, plot(frequency~scenario, col='lightgrey'))

# check na.rm
idx = which(is.na(politeness$frequency))
politeness = politeness[-idx,]
dim(politeness)

boxplot(frequency ~ attitude*gender,politeness, col = c("white", "lightgrey"))

lmer(frequency ~ attitude, data=politeness)

politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness, REML =FALSE)
coef(politeness.model)
str(politeness.model)

attributes(summary(politeness.model))
coefficients(summary(politeness.model))
summary(politeness.model)$methTitle
summary(politeness.model)$objClass
summary(politeness.model)$devcomp
summary(politeness.model)$isLmer
summary(politeness.model)$useScale
summary(politeness.model)$logLik
summary(politeness.model)$family
summary(politeness.model)$link
summary(politeness.model)$ngrps
summary(politeness.model)$coefficients
summary(politeness.model)$sigma
summary(politeness.model)$vcov
summary(politeness.model)$varcor
summary(politeness.model)$AICtab
summary(politeness.model)$call
summary(politeness.model)$residuals

summary(politeness.model)


lm.model = lm(frequency ~ attitude, data=politeness)
coef(lm.model)
str(lm.model)
attributes(lm.model)$names

summary(politeness.model)

summary(politeness)
with(politeness, mean(frequency[attitude=='inf']))
## 202.588 Hz

politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness)
summary(politeness.model)

## Likelihood test
# comparing attitude
politeness.null = lmer(frequency ~ gender + (1|subject) + (1|scenario), data=politeness, REML=FALSE)
politeness.model = lmer(frequency~attitude+gender+(1|subject)+(1|scenario), data=politeness, REML=FALSE)
anova(politeness.null, politeness.model)
anova(politeness.model, politeness.null)

coef(politeness.model)

# random slope model
politeness.model = lmer(frequency ~ attitude + gender + (1+attitude|subject)+(1+attitude|scenario), data=politeness, REML=FALSE)
summary(politeness.model)
coef(politeness.model)

politeness.null = lmer(frequency ~ gender + (1+attitude|subject) + (1+attitude|scenario), data=politeness, REML = FALSE)
summary(politeness.null)

anova(politeness.null, politeness.model)

# random slope model - complex
politeness.model.complex = lmer(frequency ~ attitude + gender + (1+attitude+gender|subject)+(1+attitude+gender|scenario), data=politeness, REML=FALSE)
summary(politeness.model.complex)
coef(politeness.model.complex)


# influential data point for attitude
all.res = numeric(nrow(politeness))
for(i in 1:nrow(politeness)){
  myfullmodel = lmer(frequency ~ attitude + gender + (1+attitude|subject)+(1+attitude|scenario), data=politeness[-i,], REML=FALSE)
  all.res[i] = fixef(myfullmodel)[1]
}

all.res

fixef(politeness.model)

