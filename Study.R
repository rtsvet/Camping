dat <- read.csv("~/RProjects/Camping/oura_2022-05-18_2022-06-22.csv")

View(dat)

library(report)
library(dplyr)
library("ggplot2")
library("GGally")
library(multcomp)

sumTible <- summarize(groupedDat,
          mean.RHR = mean(Average.Resting.Heart.Rate),
          sd.RHR = sd(Average.Resting.Heart.Rate),
          mean.Tem.Dev = mean(Temperature.Deviation),
          sd.Tem.Dev = sd(Temperature.Deviation), 
          mean.RR = mean(Respiratory.Rate),
          sd.RR = sd(Respiratory.Rate),
          mean.HRV = mean(Average.HRV),
          sd.HRV = sd(Average.HRV),
          mean.DeepSleep = mean(Deep.Sleep.Duration),
          sd.DeepSleep = sd(Deep.Sleep.Duration))


########################
# the histograms
########################
par(mfrow=c(2,4))
hist(dat$Total.Sleep.Duration)
hist(dat$Deep.Sleep.Duration)
hist(dat$REM.Sleep.Duration)
hist(dat$Light.Sleep.Duration)
hist(dat$Average.HRV)
hist(dat$Average.Resting.Heart.Rate)
hist(dat$Lowest.Resting.Heart.Rate)
hist(dat$Temperature.Deviation)

########################
# the boxplots
########################
par(mfrow=c(2,4))

boxplot(dat$Total.Sleep.Duration ~ dat$Camping, xlab = "Camping",
        ylab = "Total.Sleep.Duration")
boxplot(dat$Deep.Sleep.Duration ~ dat$Camping, xlab = "Camping",
        ylab = "Deep Sleep Duratione")
boxplot(dat$REM.Sleep.Duration ~ dat$Camping, xlab = "Camping",
        ylab = "REM.Sleep.Duration")
boxplot(dat$Light.Sleep.Duration ~ dat$Camping, xlab = "Camping",
        ylab = "Light.Sleep.Duration")
boxplot(dat$Average.HRV ~ dat$Camping, xlab = "Camping",
        ylab = "Average.HRV")
boxplot(dat$Average.Resting.Heart.Rate ~ dat$Camping, xlab = "Camping",
        ylab = "Average.Resting.Heart.Rate")
boxplot(dat$Lowest.Resting.Heart.Rate ~ dat$Camping, xlab = "Camping",
        ylab = "Lowest Resting Heart Rate")
boxplot(dat$Temperature.Deviation ~ dat$Camping, xlab = "Camping",
        ylab = "Temperature.Deviation")

###################
## Correlations
###################

sleep <- dat[c(4,8,6,9,10,11,5, 12,13)]
summary(sleep)
ggpairs(sleep)


############################################
# HRV Model
################################
hrv_aov <- aov(Average.HRV ~ Camping, data = dat)
summary(hrv_aov)
report(hrv_aov)
report(t)
tt <- t.test(dat%>%filter(Camping==1)%>%pull(var = "Average.HRV"), 
            dat%>%filter(Camping==0)%>%pull(var = "Average.HRV"))
report(tt)

# adding a blocking variable Week of the day
hrv_aov <- aov(Average.HRV ~ factor(Camping)*factor(Weekday), data = dat)
summary(hrv_aov)
report(hrv_aov)

## https://www.scribbr.com/statistics/akaike-information-criterion/ 
## also https://rpubs.com/mikhilesh/645015

# try Ch square because the sample is realyl small
dat$Hrv.Increase <-as.factor(ifelse(dat$Average.HRV > mean(dat$Average.HRV), "inc", "dec"))

table <-  table(as.factor(dat$Camping), dat$Hrv.Increase)
names(dimnames(table)) <- c("Camping", "HRV")
table
#chisq.test(table)
fisher.test(table)

##############################################
# Av RHR
avrhr_aov <- aov(Average.Resting.Heart.Rate ~ Camping, data = dat)
report(avrhr_aov)

# lowest RHR
lorhr_aov <- aov(Lowest.Resting.Heart.Rate ~ Camping, data = dat)
report(lorhr_aov)

#  REM Sleep
aov <- aov(Lowest.Resting.Heart.Rate ~ Camping, data = dat)
report(aov)

# Total Sleep duration
aov <- aov(Total.Sleep.Duration ~ Camping, data = dat)
report(aov)

# Light Sleep duration
aov <- aov(Light.Sleep.Duration ~ Camping, data = dat)
report(aov)


# Temperature Deviation
aov <- aov(Temperature.Deviation ~ Camping, data = dat)
report(aov)
tt <- t.test(dat%>%filter(Camping==1)%>%pull(var = "Temperature.Deviation"), 
       dat%>%filter(Camping==0)%>%pull(var = "Temperature.Deviation"))
report(tt)

########################################################################
# Factor Analysis is this inly temperature?
###########
par(mfrow=c(1,1))

library(FactoMineR)
library(psych)
library(GPArotation)
library(REdaS)

bart_spher(sleep)
KMO(sleep) # ashould be more than 0.7 but here is LESS :(
KMO(sleep[c(1,2,3,4,7, 8)]) # ashould be more than 0.7

# rearanging
sleep <- sleep[c(1,2,3,4,7, 8)]
bart_spher(sleep)


fa(sleep, nfactors = 3, rotate = "oblimin")
fa(sleep, nfactors = 2, rotate = "oblimin")


FactAnal <- fa(sleep, nfactors = 2, rotate = "oblimin") 

fa.diagram(FactAnal, simple = TRUE)

FactAnal
