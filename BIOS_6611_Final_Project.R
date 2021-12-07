#Load the data 
setwd("/Users/Trent/Desktop/School/CU_Anschutz_Fall_2021/Statistical Methods (6611)/Final Project")
acupuncture <- read.csv("acupuncture.csv")

#Drop uncompleted
library(tidyverse)
Acupuncture.clean <- acupuncture %>% drop_na(pk2)
acupuncture.analyze <- select(Acupuncture.clean, -c(pk5,f5))

acupuncture.analyze$sev.diff <- acupuncture.analyze$pk1 - acupuncture.analyze$pk2
acupuncture.analyze$freq.diff <- acupuncture.analyze$f1 - acupuncture.analyze$f2
##Check for normality
par(mfrow = c(2,2))
#headache Severity Baseline
hist(Acupuncture.clean$pk1, main = "Headache Severity Baseline")
#headache Severity 1 year
hist(Acupuncture.clean$pk5, main = "Headache Severity 1 year")
#headache Frequency Baseline
hist(Acupuncture.clean$f1, main = "Headache Frequency Baseline")
#headache Frequency 1 year
hist(Acupuncture.clean$f5, main = "Headache Frequency 1 year")


###Assumption of normality may be violated, so going to use bootstrap/permutation testing for estimates
##Severity 
#Subset treatment and control
acupuncture.treat <- subset(acupuncture.analyze, acupuncture.analyze$group==1)
acupuncture.control <- subset(acupuncture.analyze, acupuncture.analyze$group==0)

#Get just outcome
severity.pre <- acupuncture.treat$pk1
severity.post <- acupuncture.treat$pk2

#Permutation Test (Start with comparing treatment group)
observed_diff_sev <- mean(severity.pre) - mean(severity.post)

##Matched Two-sample Bootstrap (compare before and after treatment)
#Setup
set.seed(404)
n <- length(acupuncture.treat$pk1)
B <- 10^4
#differences in pairs
sev.diffpair <- severity.pre - severity.post
#Set up storage vector
sev.diffpair.mean <- numeric(B)
#Bootstrap Sample
for (i in 1:B){
  diff.boot <- sample(sev.diffpair, n, replace=T) 
  sev.diffpair.mean[i] <- mean(diff.boot)
} 

#Get Estimate
mean(sev.diffpair.mean)

#Get 95% CI
quantile(sev.diffpair.mean,c(0.025,0.975))

hist(sev.diffpair.mean, main = "Bootstrap Dist of Headache Severity (Treatment Only)")
abline(v = observed_diff_sev, col='blue')

##Mann-Whitney U test (Paired Data)
wilcox.test(sev.diff ~ 1, data = acupuncture.analyze)


##Two-sample Bootstrap (compare treatment and control)
#Setup
set.seed(404)
n.treat <- 161
n.control <- 140
B <- 10^4

sev.diff.mean <- numeric(B)
for (i in 1:B){
  # resample basic cable:
  control.boot <- sample(acupuncture.control$pk2, n.treat, replace=TRUE)
  # resample extended cable
  treat.boot <- sample(acupuncture.treat$pk2, n.control, replace=TRUE)
  # calculate difference in means
  sev.diff.mean[i] <- mean(control.boot)-mean(treat.boot)
}

#Calculate Mean, Bias, and SE
mean(acupuncture.control$pk2)-mean(acupuncture.treat$pk2) #sample difference

mean(sev.diff.mean) #bootstrap estimated difference

mean(sev.diff.mean)-(mean(acupuncture.control$pk2)-mean(acupuncture.treat$pk2)) #bias

sd(sev.diff.mean) #bootstrap SE

quantile(sev.diff.mean,c(0.025, 0.975))

hist(sev.diff.mean, main=expression(paste('Bootstrap distribution of ', bar(X)[c] - bar(X)[t])), 
     xlab=expression(bar(X)[c] - bar(X)[t]))
abline(v=mean(sev.diff.mean), col='purple', lwd=2) 

qqnorm(sev.diff.mean); qqline(sev.diff.mean)

#Mann Whitney U Test Between Groups
wilcox.test(pk2~group, data = acupuncture.analyze)





##Frequency
freq.pre <- acupuncture.analyze$f1
freq.post <- acupuncture.analyze$f2

observed_diff_freq <- mean(freq.pre) - mean(freq.post)

##Matched Two-sample Bootstrap Freq (compare before and after treatment)
#Setup
set.seed(404)
n <- length(acupuncture.treat$f1)
B <- 10^4
#differences in pairs
freq.diffpair <- freq.pre - freq.post
#Set up storage vector
freq.diffpair.mean <- numeric(B)
#Bootstrap Sample
for (i in 1:B){
  diff.boot <- sample(freq.diffpair, n, replace=T) 
  freq.diffpair.mean[i] <- mean(diff.boot)
} 

#Get Estimate
mean(freq.diffpair.mean)

#Get 95% CI
quantile(freq.diffpair.mean,c(0.025,0.975))

par(mfrow = c(1,1))
hist(freq.diffpair.mean, main = "Bootstrap Dist of Headache Frequency (Treatment Only)")
abline(v = mean(freq.diffpair.mean), col='blue')

##Mann-Whitney U test (Paired Data)
wilcox.test(freq.diff ~ 1, data = acupuncture.analyze)


##Two-sample Bootstrap FREQ (compare treatment and control)
#Setup
set.seed(404)
n.treat <- 161
n.control <- 140
B <- 10^4

freq.diff.mean <- numeric(B)
for (i in 1:B){
  # resample basic cable:
  control.boot <- sample(acupuncture.control$f2, n.treat, replace=TRUE)
  # resample extended cable
  treat.boot <- sample(acupuncture.treat$f2, n.control, replace=TRUE)
  # calculate difference in means
  freq.diff.mean[i] <- mean(control.boot)-mean(treat.boot)
}

#Calculate Mean, Bias, and SE
mean(acupuncture.control$f2)-mean(acupuncture.treat$f2) #sample difference

mean(freq.diff.mean) #bootstrap estimated difference

mean(freq.diff.mean)-(mean(acupuncture.control$f2)-mean(acupuncture.treat$f2)) #bias

sd(freq.diff.mean) #bootstrap SE

quantile(freq.diff.mean,c(0.025, 0.975))

hist(freq.diff.mean, main=expression(paste('Bootstrap distribution of ', bar(X)[c] - bar(X)[t])), 
     xlab=expression(bar(X)[c] - bar(X)[t]))
abline(v=mean(freq.diff.mean), col='purple', lwd=2) 

qqnorm(freq.diff.mean); qqline(freq.diff.mean)

#Mann Whitney U Test Between Groups
wilcox.test(f2~group, data = acupuncture.analyze)

