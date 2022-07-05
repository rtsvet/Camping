dat <- read.csv("~/RProjects/Camping/oura_2022-05-18_2022-06-22.csv")
View(dat)

library(report)
library(dplyr)
library("ggplot2")
library("GGally")
library(multcomp)

groupedDat <- group_by(dat, Camping)
summarize(groupedDat,
          mean.RHR = mean(Av.RHR),
          sd.RHR = sd(Av.RHR),
          mean.Tem.Dev = mean(Temp.Deviation),
          sd.Tem.Dev = sd(Temp.Deviation), 
          mean.RR = mean(RR),
          sd.RR = sd(RR),
          mean.HRV = mean(Av.HRV),
          sd.HRV = sd(Av.HRV),
          mean.DeepSleep = mean(Deep.Sleep),
          sd.DeepSleep = sd(Deep.Sleep))


summarize(groupedDat,
          mean.DeepSleep = mean(Deep.Sleep),
          sd.DeepSleep = sd(Deep.Sleep),
          mean.REM.Sleep = mean(REM.Sleep),
          sd.REM.Sleep = sd(REM.Sleep),
          mean.Restfulness = mean(Restfulness),
          sd = sd(Restfulness))

########################
# the histograms
########################
par(mfrow=c(2,4))
hist(dat$Total.Sleep)
hist(dat$Deep.Sleep)
hist(dat$REM.Sleep)
hist(dat$Light.Sleep)
hist(dat$Av.HRV)
hist(dat$Av.RHR)
hist(dat$Lo.RHR)
hist(dat$Temp.Deviation)

########################
# the boxplots
########################
par(mfrow=c(2,4))

boxplot(dat$Total.Sleep ~ dat$Camping, xlab = "Camping",
        ylab = "Total.Sleep.Duration")
boxplot(dat$Deep.Sleep ~ dat$Camping, xlab = "Camping",
        ylab = "Deep Sleep Duratione")
boxplot(dat$REM.Sleep ~ dat$Camping, xlab = "Camping",
        ylab = "REM.Sleep.Duration")
boxplot(dat$Light.Sleep ~ dat$Camping, xlab = "Camping",
        ylab = "Light.Sleep.Duration")
boxplot(dat$Av.HRV ~ dat$Camping, xlab = "Camping",
        ylab = "Average HRV")
boxplot(dat$Av.RHR ~ dat$Camping, xlab = "Camping",
        ylab = "Av.RHR")
boxplot(dat$Lo.RHR ~ dat$Camping, xlab = "Camping",
        ylab = "Lowest Resting Heart Rate")
boxplot(dat$Temp.Deviation ~ dat$Camping, xlab = "Camping",
        ylab = "Temperature Deviation")

###################
## Correlations
###################

sleep <- dat[c(4,8,6,7,9,10,11,5, 12,13)]
summary(sleep)
ggpairs(sleep)


############################################
# HRV Model
################################
hrv_aov <- aov(Av.HRV ~ Camping, data = dat)
summary(hrv_aov)
report(hrv_aov)

tt <- t.test(dat%>%filter(Camping==1)%>%pull(var = "Av.HRV"), 
            dat%>%filter(Camping==0)%>%pull(var = "Av.HRV"))
report(tt)

# adding a blocking variable Week of the day
hrv_aov <- aov(Av.HRV ~ factor(Camping)*factor(Weekday), data = dat)
summary(hrv_aov)
report(hrv_aov)

## https://www.scribbr.com/statistics/akaike-information-criterion/ 
## also https://rpubs.com/mikhilesh/645015

# try Ch square because the sample is really small
dat$Hrv.Increase <-as.factor(ifelse(dat$Av.HRV > mean(dat$Av.HRV), "inc", "dec"))

table <-  table(as.factor(dat$Camping), dat$Hrv.Increase)
names(dimnames(table)) <- c("Camping", "Av.HRV")
table
chisq.test(table)
fisher.test(table)

##############################################
# Av RHR
avrhr_aov <- aov(Av.RHR ~ Camping, data = dat)
report(avrhr_aov)

# lowest RHR
lorhr_aov <- aov(Lo.RHR ~ Camping, data = dat)
report(lorhr_aov)

#  REM Sleep
aov <- aov(Lo.RHR ~ Camping, data = dat)
report(aov)

# Total Sleep duration
aov <- aov(Total.Sleep ~ Camping, data = dat)
report(aov)

# Light Sleep duration
aov <- aov(Light.Sleep ~ Camping, data = dat)
report(aov)


# Temperature Deviation
aov <- aov(Temp.Deviation ~ Camping, data = dat)
report(aov)
tt <- t.test(dat%>%filter(Camping==1)%>%pull(var = "Temp.Deviation"), 
       dat%>%filter(Camping==0)%>%pull(var = "Temp.Deviation"))
report(tt)

########
# Deep Sleep

aov <- aov(Deep.Sleep ~ Camping, data = dat)
report(aov)


dat$Deep.Sleep.Increase <-as.factor(ifelse(dat$Deep.Sleep >= mean(dat$Deep.Sleep), "hi", "lo"))

table <-  table(as.factor(dat$Camping), dat$Deep.Sleep.Increase)
names(dimnames(table)) <- c("Camping", "Deep.Sleep")
table
chisq.test(table)
fisher.test(table)

# define a ratio - deep sleep score = dss
dat$dss <- dat$Deep.Sleep/dat$Total.Sleep
aov <- aov(dss ~ Camping, data = dat)
report(aov)

dat$dss.Increase <- as.factor( ifelse(dat$dss > mean(dat$dss), "hi", "lo") )
                             
table <-  table(as.factor(dat$Camping), dat$dss.Increase)
names(dimnames(table)) <- c("Camping", "DS Score incr")
table
chisq.test(table)
fisher.test(table)

# try to remove the influence of the seep duration
aov <- aov(Deep.Sleep ~ Camping + Total.Sleep, data = dat)
report(aov)

dat$tss.Increase <- as.factor( ifelse(dat$Total.Sleep > mean(dat$Total.Sleep), "hi", "lo") )
aov <- aov(Deep.Sleep ~ Camping * tss.Increase, data = dat)
report(aov)


########################################################################
#
# Factor Analysis is this inly temperature?
#
########################################################################

par(mfrow=c(1,1))

library(FactoMineR)
library(psych)
library(GPArotation)
library(REdaS)

bart_spher(sleep)
KMO(sleep) # should be more than 0.7 but here is LESS :(
KMO(sleep[c(1,2, 3, 7,8)]) # should be more than 0.7

# rearranging
sl <- sleep[c(1,2,3, 7, 8)]
bart_spher(sl)
KMO(sl)

# Determine Number of Factors to Extract
library(nFactors)
cor(sl)
ev <- eigen(cor(sl)) # get eigenvalues
ap <- parallel(subject=nrow(sl),var=ncol(sl), rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)



# Do factor analysis
FactAnal <- fa(sl, nfactors = 2, rotate = "oblimin") 
fa.diagram(FactAnal, simple = TRUE)
FactAnal


psaDiagr <- PCA(sl)

##### Different approach
# https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/factor-analysis/A-simple-example-of-FA/index.html#:~:text=In%20the%20R%20software%20factor,specified%20by%20the%20argument%20factors%20.

faca <- factanal(sl, factors = 2)
faca$uniquenesses
faca
# The last section of the function output shows the results of a hypothesis test.
# The null hypothesis, H0, is that the number of factors in the model (here 2),
# is sufficient to capture the full dimensionality of the data set.

Lambda <- faca$loadings
Psi <- diag(faca$uniquenesses)
S <- faca$correlation
Sigma <- Lambda %*% t(Lambda) + Psi

round(S - Sigma, 6)

faca.none <- factanal(sl, factors = 2, rotation = "none")
faca.varimax <- factanal(sl, factors = 2, rotation = "varimax")
faca.promax <- factanal(sl, factors = 2, rotation = "promax")

par(mfrow = c(1,3))
plot(faca.none$loadings[,1], 
     faca.none$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)

plot(faca.varimax$loadings[,1], 
     faca.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")

text(faca.varimax$loadings[,1]-0.08, 
     faca.varimax$loadings[,2]+0.08,
     colnames(food),
     col="blue")
abline(h = 0, v = 0)

plot(faca.promax$loadings[,1], 
     faca.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")
abline(h = 0, v = 0)

