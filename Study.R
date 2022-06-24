dat <- read.csv("~/RProjects/Camping/oura_2022-05-18_2022-06-22.csv")

View(dat)

library(report)
library(dplyr)
library("ggplot2")
library("GGally")

# HRV Model
boxplot(dat$Average.HRV ~ dat$Camping)
hrv_aov <- aov(Average.HRV ~ Camping, data = dat)
summary(hrv_aov)
report(hrv_aov)

# Av RHR
boxplot(dat$Average.Resting.Heart.Rate ~ dat$Camping)
avrhr_aov <- aov(Average.Resting.Heart.Rate ~ Camping, data = dat)
report(avrhr_aov)

# lowest RHR
boxplot(dat$Lowest.Resting.Heart.Rate ~ dat$Camping, xlab = "Camping",
        ylab = "Lowest Resting Heart Rate")
lorhr_aov <- aov(Lowest.Resting.Heart.Rate ~ Camping, data = dat)
report(lorhr_aov)

# lowest Deep Sleep
boxplot(dat$Deep.Sleep.Duration ~ dat$Camping, xlab = "Camping",
        ylab = "Deep Sleep Duratione")
dsd_aov <- aov(Lowest.Resting.Heart.Rate ~ Camping, data = dat)
report(dsd_aov)

# lowest REM Sleep
boxplot(dat$REM.Sleep.Duration ~ dat$Camping, xlab = "Camping",
        ylab = "REM.Sleep.Duration")
aov <- aov(Lowest.Resting.Heart.Rate ~ Camping, data = dat)
report(aov)
