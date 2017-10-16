## ----include=FALSE-------------------------------------------------------
require(xtable)
require(lattice)

## ------------------------------------------------------------------------
pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male",3))
my.df = data.frame(sex, pitch)

## ------------------------------------------------------------------------
my.df

## ------------------------------------------------------------------------
xmdl = lm(pitch ~ sex, my.df)

## ----eval=FALSE----------------------------------------------------------
## summary(xmdl)

## ----echo=FALSE, size='tiny'---------------------------------------------
summary(xmdl)

## ----echo=FALSE, size='scriptsize'---------------------------------------
coef(summary(xmdl))

## ----echo=FALSE, fig.height=4.5------------------------------------------
xyplot(pitch~sex, data=my.df, col='black',ylim=c(-10,310),
	panel=function(x,y){
		panel.xyplot(x,y, col='black',pch=19)
		panel.abline(a=226.83+98.33, b=-98.33)
		panel.abline(h=226.83, lty='dotted')
		panel.abline(v=1, lty='dotted')
		panel.arrows(x0=2, y0=226.83, x1=2,y1=226.83-98.33)
	})

## ----size='tiny'---------------------------------------------------------
age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
my.df = data.frame(age, pitch)
xmdl = lm(pitch~age, my.df)
summary(xmdl)

## ----echo=FALSE, size='scriptsize'---------------------------------------
summary(xmdl)$coefficients

## ----echo=FALSE,fig.height=5---------------------------------------------
plot(pitch~age, data=my.df, pch=19, xlim=c(-5,90), ylim=c(175,290))
abline(xmdl)
#abline(h=267.0765, lty='dashed')
lines(x=c(0,0), y=c(0,267.0765), lty='dashed')
for(i in seq(10,80,by=10)){
	lines(x=c(i,i),y=c(0,267.0765-i*0.9099), lty='dashed')
	lines(x=c(i-10,i), y=c(267.0765-(i-10)*0.9099,267.0765-(i-10)*0.9099), lty='dashed')
	arrows(x0=i, y0=267.0765-(i-10)*0.9099,x1=i,y1=267.0765-i*0.9099, length=0.1)
}

## ------------------------------------------------------------------------
my.df$age.c = my.df$age - mean(my.df$age)
xmdl = lm(pitch~age.c, my.df)

## ----echo=FALSE,size='scriptsize'----------------------------------------
summary(xmdl)$coefficients

## ----echo=FALSE,fig.height=4.3-------------------------------------------
xmdl = lm(pitch~age,my.df)
plot(pitch~age,my.df, pch=19, xlim = c(-2,82), ylim = c(175, 285))
abline(xmdl, lty='dashed')
for(i in 1:dim(my.df)[1]){
	with(my.df,lines(x=c(age[i],age[i]),y=c(pitch[i],xmdl$fitted.values[i]),col='red',lwd=1.5))
}

## ----residual_plot,fig.height=4.3, echo=-2, size='footnotesize'----------
plot(fitted(xmdl), residuals(xmdl),pch=19, 
	xlab = 'Fitted values', ylab='Residuals' )
abline(h=0, lty='dashed')

## ----echo=FALSE,fig.height=4---------------------------------------------
plot(rnorm(100),rnorm(100),xlab="Fitted value", ylab="Residuals" )
abline(h=0, lty="dashed")

## ----eval=FALSE, size='scriptsize'---------------------------------------
hist(residuals(xmdl))

## ----eval=FALSE, size='scriptsize'---------------------------------------
qqnorm(residuals(xmdl))

## ----size='scriptsize'---------------------------------------------------
dfbeta(xmdl)

