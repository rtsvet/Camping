dat <- read.csv("~/RProjects/Camping/oura_2022-05-18_2022-06-22.csv")

View(dat)

library(report)
library(dplyr)
library("ggplot2")
library("GGally")
library(multcomp)

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


#####################
# HRV Model

hrv_aov <- aov(Average.HRV ~ Camping, data = dat)
summary(hrv_aov)
report(hrv_aov)

# Av RHR
avrhr_aov <- aov(Average.Resting.Heart.Rate ~ Camping, data = dat)
report(avrhr_aov)

# lowest RHR
lorhr_aov <- aov(Lowest.Resting.Heart.Rate ~ Camping, data = dat)
report(lorhr_aov)

# lowest Deep Sleep
dsd_aov <- aov(Lowest.Resting.Heart.Rate ~ Camping, data = dat)
report(dsd_aov)

#  REM Sleep
aov <- aov(Lowest.Resting.Heart.Rate ~ Camping, data = dat)
report(aov)

# Total Sleep duration
aov <- aov(Total.Sleep.Duration ~ Camping, data = dat)
report(aov)

# Light Sleep duration
aov <- aov(Light.Sleep.Duration ~ Camping, data = dat)
report(aov)






