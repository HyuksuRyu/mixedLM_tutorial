<<<<<<< HEAD
## ----include=FALSE-------------------------------------------------------
require(xtable)
require(lattice)
require(lme4)

## ----default, echo=FALSE-------------------------------------------------
politeness= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
politeness$scenario = factor(politeness$scenario)

## ----box_subject,include=FALSE-------------------------------------------
with(politeness, plot(frequency~subject, col ='lightgrey'))

## ----box_item,include=FALSE----------------------------------------------
with(politeness, plot(frequency~scenario, col='lightgrey'))

## ----eval=FALSE----------------------------------------------------------
## install.packages("lme4") # Linear Mixed Effect Model

## ----eval=FALSE----------------------------------------------------------
## library(lme4)

## ----size='scriptsize'---------------------------------------------------
politeness=read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

## ----size='scriptsize'---------------------------------------------------
summary(politeness)

## ----size='scriptsize'---------------------------------------------------
politeness$scenario = factor(politeness$scenario)

## ----size='scriptsize'---------------------------------------------------
(idx = which(is.na(politeness$frequency)))
politeness = politeness[-idx,]

## ----size='scriptsize'---------------------------------------------------
summary(politeness)

## ----box_freq,include=FALSE----------------------------------------------
boxplot(frequency ~ attitude*gender,politeness, col = c("white", "lightgrey"))

## ----size='scriptsize'---------------------------------------------------
politeness.model = lmer(frequency ~ attitude + (1|subject) + 
	(1|scenario), data=politeness, REML=FALSE)

## ----size='scriptsize', eval=FALSE---------------------------------------
## summary(politeness.model)

## ----size='scriptsize', echo=FALSE---------------------------------------
summary(politeness.model)

## ----echo=FALSE, size='scriptsize'---------------------------------------
summary(politeness.model)$AICtab

## ----echo=FALSE, size='scriptsize'---------------------------------------
summary(politeness.model)$coefficients

## ----size='scriptsize'---------------------------------------------------
politeness.model.2 = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness)

## ----eval=FALSE, size='scriptsize'---------------------------------------
## summary(politeness.model.2)

## ----echo=FALSE, size='scriptsize'---------------------------------------
summary(politeness.model.2)

## ----size='scriptsize', echo=FALSE---------------------------------------
summary(politeness.model.2)$coefficients

## ----size='scriptsize'---------------------------------------------------
politeness.null = lmer(frequency ~ gender + (1|subject) 
	+ (1|scenario), data=politeness, REML=FALSE)

## ----size='scriptsize'---------------------------------------------------
politeness.model = lmer(frequency ~ attitude + gender 
	+ (1|subject) + (1|scenario), data=politeness, REML=FALSE)

## ----size='scriptsize', eval=FALSE---------------------------------------
## anova(politeness.null, politeness.model)

## ----size='scriptsize', echo=FALSE---------------------------------------
anova(politeness.null, politeness.model)

## ----size='scriptsize'---------------------------------------------------
politeness.model = lmer(frequency ~ attitude + gender + (1+attitude|subject)
	+ (1+attitude|scenario), data=politeness, REML=FALSE)
politeness.inter = lmer(frequency ~ attitude * gender + (1+attitude|subject)
	+ (1+attitude|scenario), data=politeness, REML=FALSE)
anova(politeness.model, politeness.inter)

## ----size='footnotesize', eval=FALSE-------------------------------------
## coef(politeness.model)

## ----size='scriptsize', echo=FALSE---------------------------------------
coef(politeness.model)

## ----size='scriptsize'---------------------------------------------------
politeness.model = lmer(frequency ~ attitude + gender 
                        + (1+attitude|subject) + (1+attitude|scenario),
                        data=politeness, REML=FALSE)

## ----size='footnotesize', eval=FALSE-------------------------------------
## coef(politeness.model)

## ----size='scriptsize', echo=FALSE---------------------------------------
coef(politeness.model)

## ----size='scriptsize'---------------------------------------------------
politeness.model = lmer(frequency ~ attitude + gender 
                        + (1+attitude|subject)+(1+attitude|scenario), 
                        data=politeness, REML=FALSE)
politeness.null = lmer(frequency ~ gender 
                       + (1+attitude|subject) + (1+attitude|scenario), 
                       data=politeness, REML = FALSE)

## ----size='scriptsize'---------------------------------------------------
anova(politeness.null, politeness.model)

## ----eval=FALSE, size='scriptsize'---------------------------------------
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

## ----size='scriptsize', echo=FALSE---------------------------------------
citation()

## ----size='scriptsize'---------------------------------------------------
citation('lme4')

=======
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

>>>>>>> c286215cd9ecb56424f605abcea76c595ed63f49
