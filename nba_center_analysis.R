data = read.csv("CLEANall_center_data.txt", header=T)

data$PPM = data$Points/data$Minutes

######### Plots for Part 3 Exploratory Analysis ##########

hist(data$Height, main="Heights of NBA Centers", xlab="Height in Inches", ylab="Number of Players")
hist(data$Weight, main="Weights of NBA Centers", xlab="Weight in Pounds", ylab="Numberof Players")
hist(data$Minutes, main="Minutes Played per Season by NBA Centers", xlab="Minutes Played", ylab="Number of Players")
hist(data$Age, main="Age of NBA Centers", xlab="Age in Years", ylab="Number of Players")
hist(data$PPM, main="Points Per Minute of NBA Centers", xlab="Points Per Minute", ylab="Number of Players")
summary(data$Minutes)
summary(data$Height)
summary(data$Weight)
summary(data$Age)
summary(data$PPM)

IQR(data$Minutes)
IQR(data$Height)
IQR(data$Weight)
IQR(data$Age)
IQR(data$PPM)
plot(data$Height, data$Weight, main ="NBA Center Heigh vs Weight", xlab="Height in Inches", ylab="Weight in Pounds")


summary(filtered$Minutes)
summary(filtered$Height)
summary(filtered$Weight)
sd(filtered$Weight)
summary(filtered$Age)
sd(filtered$Age)
summary(filtered$PPM)
sd(filtered$PPM)

IQR(filtered$Minutes)
IQR(filtered$Height)
IQR(filtered$Weight)
IQR(filtered$Age)
IQR(filtered$PPM)

pairs(data[c(4,5,6)]) #pair plots for Age, Weight, Height

######### Filtered Data #########

min_percentile_10 = quantile(data$Minutes, c(0.1))
filtered = subset(data, Minutes > min_percentile_10)


pairs(filtered[c(4,5,6)]) #pair plots for Age, Weight, Height on filtered data
cor(filtered$Age, filtered$Height)
cor(filtered$Age, filtered$Weight)

######### Checking mean PPM across seasons #########

s1 = subset(filtered, Season == "2004-05" |  Season == "2005-06")
s2 = subset(filtered, Season == "2006-07" |  Season == "2007-08")

t.test(s1$PPM, s2$PPM)

s3 = subset(filtered, Season == "2008-09" |  Season == "2009-10")
s4 = subset(filtered, Season == "2010-11" |  Season == "2011-12")

t.test(s3$PPM, s4$PPM)

old_seasons = subset(filtered, Season == "2004-05" |  Season == "2005-06" | Season == "2006-07" | Season == "2007-08")

new_seasons = subset(filtered, Season == "2008-09" |  Season == "2009-10" | Season == "2010-11" | Season == "2011-12")

#test if PPM for the first for years is different than the last four years
t.test(old_seasons$PPM, new_seasons$PPM)

######### Individual Plots #########

lm.weight = lm(PPM ~ Weight, data = filtered)
plot(filtered$Weight, filtered$PPM)
abline(lm.weight, col="blue", lwd=2)

lm.height = lm(PPM ~ Height, data = filtered)
plot(filtered$Height, filtered$PPM)
abline(lm.height, col="blue", lwd=2)

lm.age = lm(PPM ~ Age, data = filtered)
plot(filtered$Age, filtered$PPM)
abline(lm.age, col="blue", lwd=2)

######### Transformations of Age-Only Model ########

lm.age = lm(PPM ~ Age, data = filtered)
plot(filtered$Age, filtered$PPM)
abline(lm.age, col="blue", lwd=2)
plot(hatvalues(lm.age), resid(lm.age))

qqnorm(resid(lm.age))
qqline(resid(lm.age))

h = hatvalues(lm.age)
r = resid(lm.age)

lm.age.log = lm(PPM ~ log(Age), data = filtered)
plot(hatvalues(lm.age.log), resid(lm.age.log))


lm.age.log.both = lm(log(PPM) ~ log(Age), data = filtered)
plot(hatvalues(lm.age.log.both), resid(lm.age.log.both))
qqnorm(resid(lm.age.log.both))
qqline(resid(lm.age.log.both))

######### Multivariable Models #########

lm.ppm = lm(PPM ~ Age + Height + Weight, data= filtered)
lm.ppm.sqrt = lm(filtered$PPM ~ filtered$Height + filtered$Weight +sqrt(filtered$Age))
lm.ppm.exp = lm(filtered$PPM ~ filtered$Height + filtered$Weight +exp(filtered$Age))
lm.ppm.log = lm(filtered$PPM ~ filtered$Height + filtered$Weight +log(filtered$Age))


####### Confidence Intervals on Beta-Hats ########

se = summary(lm.ppm)$coefficients[,2]
beta.hat = summary(lm.ppm)$coefficients[,1]

se.age=se[2]
se.height=se[3]
se.weight=se[4]



beta.hat.age = beta.hat[2]
beta.hat.height = beta.hat[3]
beta.hat.weight = beta.hat[4]

ci.bound.age = se.age*qt(0.975, 654)
ci.bound.weight = se.weight*qt(0.975, 654)
ci.bound.height = se.height*qt(0.975, 654)

ci.full.age = c(beta.hat.age - ci.bound.age, beta.hat.age + ci.bound.age)
ci.full.weight = c(beta.hat.weight - ci.bound.weight, beta.hat.weight +ci.bound.weight)
ci.full.height = c(beta.hat.height - ci.bound.height, beta.hat.height + ci.bound.height)

####### Confidence Interval on Age Model ######

se.age2 = summary(lm.age)$coefficients[2,2]
beta.hat.age2 = summary(lm.age)$coefficients[2,1]

ci.bound.age2 = se.age2*qt(0.975, 654)

ci.partial.age = c(beta.hat.age2 - ci.bound.age2, beta.hat.age2 + ci.bound.age2)

####### Confidence Interval on final Log Transformed Age Model #######

sse.age.log = sum(resid(lm.age.log.both)^2)
s.squared.age.log = sse.log.age / (658 - 1 - 3)
s.age.log   = sqrt(s.squared.age.log)

sxx.age.log = var(log(filtered$Age))*(length(log(filtered$Age))-1)

se.age.log = s.age.log / sqrt(sxx.age.log)

beta.hat.age.log = -0.5308

ci.bound.age.log = se.age.log*qt(0.975, 654)

ci.age.log = c(beta.hat.age.log - ci.bound.age.log, beta.hat.age.log + ci.bound.age.log)

#### Checking Collinearity ####

vif(lm.ppm)

######### Checking Homoschedastic #########
plot(hatvalues(lm.ppm), resid(lm.ppm))

boxplot(filtered$Height)  #Reject outlier (Yao)
boxplot(filtered$Weight)  # Reject outlier (Shaq and N'Dong)

plot(hatvalues(lm.age), resid(lm.age))



h = hatvalues(lm.ppm)
r = resid(lm.ppm)

for (i in 1:length(h)) {
	if(h[i] > 0.02){
		if(r[i] > 0.1){
			print(filtered[i,])
		}
	}
}



######### Check the normality assumption ######

qqnorm(resid(lm.ppm)) # If this plot is linear, then the normality assumption is reasonable
qqline(resid(lm.ppm))



#### Check if the model including just age is as good as the full model ######
anova(lm.age, lm.ppm)

####### Prediction for Dwight Howard for this season #######



d.howard = data.frame(Age=27)
predict(lm.age, d.howard, interval="predict", data=filtered)
predict(lm.age, d.howard, interval="confidence", data=filtered)



