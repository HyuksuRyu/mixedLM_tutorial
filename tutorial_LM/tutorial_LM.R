pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male",3))
my.df = data.frame(sex, pitch)

my.df

xmdl = lm(pitch ~ sex, my.df)

summary(xmdl)

summary(xmdl)

