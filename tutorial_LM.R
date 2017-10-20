library(dplyr)

# pitch ~ sex
pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male",3))
my.df = data.frame(sex, pitch)

my.df

# building LM
xmdl = lm(pitch ~ sex, my.df)

summary(xmdl)

coef(summary(xmdl))
plot(xmdl)


# pitch ~ age
age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
my.df = data.frame(age, pitch)
xmdl = lm(pitch~age, my.df)
summary(xmdl)

coef(summary(xmdl))

## data centering
my.df = my.df %>%
  mutate(age.c = age - mean(age))
xmdl = lm(pitch~age.c, my.df)

summary(xmdl)$coefficients

# scatter plot
xmdl = lm(pitch~age,my.df)
plot(pitch~age,my.df, pch=19, xlim = c(-2,82), ylim = c(175, 285))
abline(xmdl, lty='dashed')
for(i in 1:dim(my.df)[1]){
	with(my.df,lines(x=c(age[i],age[i]),y=c(pitch[i],xmdl$fitted.values[i]),col='red',lwd=1.5))
}

# residual plot
plot(fitted(xmdl), residuals(xmdl),pch=19, 
	xlab = 'Fitted values', ylab='Residuals' )
abline(h=0, lty='dashed')


# influential data
dfbeta(xmdl)

