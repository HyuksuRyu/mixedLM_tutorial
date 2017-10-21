require(dplyr)

# model 1
# pitch ~ sex
pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male",3))
my.df = data.frame(sex, pitch)

my.df

## building lm
xmdl = lm(pitch ~ sex, my.df)

summary(xmdl)

coef(summary(xmdl))

# pitch ~ age
age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
my.df = data.frame(age, pitch)
xmdl = lm(pitch~age, my.df)
summary(xmdl)

summary(xmdl)$coefficients

plot(pitch~age, data=my.df, pch=19, xlim=c(-5,90), ylim=c(175,290))
abline(xmdl)

my.df = my.df %>%
  mutate(age.c = age - mean(age))
my.df$age.c = my.df$age - mean(my.df$age)
xmdl = lm(pitch~age.c, my.df)

summary(xmdl)$coefficients

# residual plot
plot(fitted(xmdl), residuals(xmdl),pch=19, 
	xlab = 'Fitted values', ylab='Residuals' )
abline(h=0, lty='dashed')

## hist(residuals(xmdl))
## qqnorm(residuals(xmdl))

# influential data
dfbeta(xmdl)

