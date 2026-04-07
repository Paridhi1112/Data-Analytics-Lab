library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("C:/Users/nikhi/OneDrive/Desktop/DA 6600 (R)")


# read data
mkp.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(mkp.data)

# print summary of variables in dataframe
summary(mkp.data$MKP.new)

# print values in variable
mkp.data$MKP.new


######## Optional ########
## If you want to reference the variable without using the dataframe:

# attach dataframe
attach(mkp.data)

# print values in variable
MKP.new


########################



### Explore Variable ###

MKP <- mkp.data$MKP.new

# find NAs in variable - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(MKP)

MKP[which(NAs)]

# print values in variable
ECS <- mkp.data$ECS.new

ECS

# find NAs inv variavle - outputs vector of logical values, true if NA, false otherwise
NAs <- is.na(ECS)

# print NAs
ECS[which(NAs)]

# take subset of NOT NAs from variable
ECS.noNA <- ECS[!NAs]

ECS.noNA

# filter for only values above 30
ECS.above30 <- ECS.noNA[ECS.noNA>30]

ECS.above30

# stats
summary(ECS.above30)

# boxplot of variable(s)
boxplot(MKP, ECS.above30, names = c("MKP","ECS"))


### Histograms ###

# histogram (frequency distribution)
hist(MKP)

# define sequence of values over which to plot histogram
x <- seq(0, 100, 10)

# histogram (frequency distribution) over range
hist(MKP, x, prob=TRUE)

# print estimated density curve for variable
lines(density(MKP,na.rm=TRUE,bw=1.)) # or try bw=“SJ”

# print rug
rug(MKP)

x <- seq(0, 100, 10)

# histogram (frequency distribution) over rabge
hist(MKP, x, prob=TRUE) 

# print estimated density curve for variable
lines(density(MKP,na.rm=TRUE, bw="SJ"))

# print rug
rug(MKP)


# histogram (frequency distribution) over rabge
hist(MKP.new, x, prob=TRUE) 

# range
x1<-seq(20,80,1)

# generate probability density values for a normal distribution with given mean and sd
d1 <- dnorm(x1,mean=45, sd=11,log=FALSE)

# print density values
lines(x1,d1)

# generate probability density values for a normal distribution with given mean and sd
d2 <- dnorm(x1,mean=64, sd=11,log=FALSE) 

# print density values
lines(x1,d2) 

# print density values
lines(x1,.5*d2)

### Empirical Cumulative Distribution Function ###

# plot ecdfs
plot(ecdf(MKP), do.points=FALSE, verticals=TRUE) 

plot(ecdf(ECS), do.points=FALSE, verticals=TRUE) 


### Quantile-quantile Plots ###

# print quantile-quantile plot for variable with theoretical normal distribuion
qqnorm(MKP); qqline(MKP)


# print quantile-quantile plot for random numbers from a normal distribution with theoretical normal distribution
x <- rnorm(500)
qqnorm(x); qqline(x)


# print quantile-quantile plot for variable with any theoretical distribution
qqplot(rnorm(180), MKP, xlab = "Q-Q plot for norm dsn") 
qqline(MKP)

# print quantile-quantile plot for 2 variables
qqplot(MKP, ECS, xlab = "Q-Q plot for MKP vs ECS") 

qqplot(x, MKP, xlab = "Q-Q plot for MKP vs ECS") 
qqline(MKP)

y <- rnorm(500)

qqplot(x, y, xlab = "Q-Q plot for MKP vs ECS") 
qqline(y)


## Statistical Tests

x <- rnorm(500)
y <- rnorm(500)

hist(x)
hist(y)

shapiro.test(x)
shapiro.test(y)

ad.test(x)
ad.test(y)

ks.test(x,y)

wilcox.test(x,y)

var.test(x,y)
t.test(x,y)

