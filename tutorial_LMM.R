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

