library(tidyverse)
install.packages("HSAUR")

data("Forbes2000", package="HSAUR")

seq(from=0, to=10, by=2)
Forbes2000[, "name"][1] #indicate a certain value
table(Forbes2000[, "category"]) #seeing number of obs by levels

csvForbes2000 <- read.table("Forbes2000.csv", 
                            header = TRUE, sep = ",", row.names = 1, 
                            colClasses = c("character", "integer", "character", 
                                           "factor", "factor","numeric", "numeric", 
                                           "numeric", "numeric"))

#---------------------------------------------
#----------------PART 1----------------------- 
#---------------------------------------------

sample(1:60, 3)
Carbon <- read.table("http://stat4ds.rwth-aachen.de/data/Carbon.dat", header=TRUE)

breaks <- seq(2.0, 10.0, by=1.0) # frequency dist. intervals of width 1 between 2 and 10
freq <- table(cut(Carbon$CO2, breaks, right=F))
#The table function counts how many types of data there is 
#in a certain column, 4 apples, 3 oranges, ...
freq <- summary(cut(Carbon$CO2, breaks, right=FALSE))
#The cut function divides the range of x into intervals
freq
cbind(freq, freq/nrow(Carbon))
hist(Carbon$CO2, xlab="CO2", ylab="Proportion", freq=F)
plot(density(Carbon$CO2))

summary(Carbon$CO2) # 1st Qu = lower quartile, 3rd Qu = upper quartile
c(mean(Carbon$CO2), sd(Carbon$CO2), quantile(Carbon$CO2, 0.90))
boxplot(Carbon$CO2, xlab="CO2 values", horizontal=TRUE)

Crime <- read.table("http://stat4ds.rwth-aachen.de/data/Murder2.dat", header=TRUE)
boxplot(Crime$murder ~ Crime$nation, xlab="Murder rate", horizontal=TRUE)
tapply(Crime$murder, Crime$nation, summary)
?tapply

Students <- read.table("http://stat4ds.rwth-aachen.de/data/Students.dat", header=TRUE)
Carbon_West <-read.table("http://stat4ds.rwth-aachen.de/data/Carbon_West.dat", header=TRUE)

summary(Carbon_West)
breaks <- seq(2.0, 17.0, by=1)
freq <- table(cut(Carbon_West$CO2, breaks, right=F))
freq
a <- cbind(freq, freq/nrow(Carbon_West))

hist(Carbon_West$CO2, freq=F)
plot(density(Carbon$CO2))

Murder <-read.table("http://stat4ds.rwth-aachen.de/data/Murder.dat", header=TRUE) %>% 
  filter(!Murder$state == "DC")
summary(Murder$murder)
cbind(mean(Murder$murder), sd(Murder$murder))
boxplot(Murder$murder)

Income <-read.table("http://stat4ds.rwth-aachen.de/data/Income.dat", header=TRUE)
hist(Income$income)
summary(Income$income)
plot(density(Income$income))
a <- filter(Income, Income$race=="H")
boxplot(a$income)
table(Income$race)

Houses <-read.table("http://stat4ds.rwth-aachen.de/data/Houses.dat", header=TRUE)
summary(Houses$price)
summary(a$price)
sd(a$price)
a <- filter(Houses, Houses$new==1)
breaks <- seq(31, 881, by=100)
freq <- table(cut(Houses$price, breaks, right=F))
freq
cbind(freq, freq/nrow(Houses))
hist(Houses$price)
sd(Houses$price)
boxplot(Houses$price)
boxplot(a$price)
PriceH <- hist(Houses$price)
PriceH$breaks
tapply(Houses$price, Houses$new, summary)

plot(x=Houses$price, y=Houses$size)
cor(Houses$price, Houses$size)
Houses %>% lm(formula=price ~ size) %>% summary()
summary(lm(Houses$price ~ Houses$size))

cor(Students$hsgpa, Students$cogpa)
plot(Students$hsgpa, Students$cogpa)
summary(lm(Students$hsgpa~ Students$cogpa))

Happy <-read.table("http://stat4ds.rwth-aachen.de/data/Happy.dat", header=TRUE)
table(Happy$marital, Happy$happiness)

attach(Students)
table(Students$relig, Students$abor)

UN <-read.table("http://stat4ds.rwth-aachen.de/data/UN.dat", header=TRUE)
summary(UN$Prison)
hist(UN$Prison)
boxplot(UN$Prison)
cor(cbind(UN$GDP, UN$HDI, UN$GII, UN$Fertility,
          UN$CO2, UN$Homicide, UN$Prison, UN$Internet))
summary(lm(UN$CO2~ UN$GDP))

ScotsRaces <- read.table("http://stat4ds.rwth-aachen.de/data/ScotsRaces.dat", header=TRUE)

attach(ScotsRaces$Races)
par(mfrow=c(2,2)) # a matrix of 2x2 plots in one graph
boxplot(ScotsRaces$timeM); boxplot(ScotsRaces$timeW)
hist(ScotsRaces$timeM); hist(ScotsRaces$timeW)
summary(ScotsRaces$timeM) # (output not shown)
summary(ScotsRaces$timeW) # (output not shown)
dev.off() # reset the graphical parameter mfrow
plot(ScotsRaces$timeM, ScotsRaces$timeW)
cor(ScotsRaces$timeM, ScotsRaces$timeW) # output: [1] 0.9958732
summary(lm(ScotsRaces$timeW ~ ScotsRaces$timeM))

Sheep <- read.table("http://stat4ds.rwth-aachen.de/data/Sheep.dat", header=TRUE)

attach(Sheep)
tapply(weight, survival, summary) # (output not shown)
tapply(weight, survival, sd) # (output not shown)
boxplot(weight ~ survival, xlab="weight", horizontal=TRUE)

PartyID <- read.table("http://stat4ds.rwth-aachen.de/data/PartyID.dat", header=TRUE)
mosaicplot(PartyID)
mosaicplot(table(PartyID$race, PartyID$id))

#------------------------------------------
#---------------PART 2---------------------
#------------------------------------------

#simulating the tomorrow's weather

a <- replicate(10^2, (sample(9, 1))) #the closer our sample is from our population, the closer to a uniform distribution we'll be 
ggplot() + geom_bar(aes(a))
a <- replicate(10^5, (sample(9, 1))) 
ggplot() + geom_bar(aes(a))
sum(a==8)/length(a) 
table(a)

sample(1:9, 7, 
       replace=T) #So we have 7 days (our sample) and number 8 means will rain

#[1] 8 4 3 6 2 1 2
#Just one day a week will rain, probability of 1/9 per day

rbinom(1, 7, 0.20) #1 simulation of 7 coin flips, prob = 0.20 of heads
#outcome 2 means that will be 2 heads in average if we toss a coin 7 times
rbinom(7, 1, 0.20) #7 simulations of 1 coin flipping
#[1] 0 0 1 0 0 0 1, 1 indicates when we had a head
a <- replicate(10^3, rbinom(7, 9, 0.20)) 
ggplot() + geom_bar(aes(a))

1-dbinom(x=0, size=6, prob=1/6) #prob of getting at least 1 sixes when 6 fair dice are rolled
1-(dbinom(0, 12, 1/6) + dbinom(1, 12, 1/6)) #prob of getting at least 2 sixes when 12 fair dice are rolled
1-(dbinom(0, 18, 1/6) + dbinom(1, 18, 1/6)+dbinom(2, 18,1/6)) #so on...
#formula choose(n,x)*(p^(x))*(q^(n-x))









