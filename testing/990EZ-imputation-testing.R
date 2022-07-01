###############################
### Need to impute total employees for 990EZ filers
#############################

library(dplyr)
library(readr)


#Get filtered data 
dat <- read_csv("data-wrangle/data-by-sector.csv")


#split by 990 and 990EZ
dat.990 <- dat %>%
  filter(FormType == "990")

dat.EZ <- dat %>%
  filter(FormType == "990EZ")

##########################
### testing MICE 
#########################

# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
library(haven)
library(dplyr)
library(mice)
library(VIM)

data <- airquality
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA

data <- data[-c(5,6)]
summary(data)

pMiss <- function(x){sum(is.na(x))/length(x)}
apply(data,2,pMiss)
apply(data,1,pMiss)

md.pattern(data)

aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

marginplot(data[c(1,2)])

tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500, printFlag = F)
summary(tempData)

tempData$imp$Ozone

completedData <- complete(tempData,1)

#exploreing imputed vs original
#want red and blue to be about the same shape 

xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)
densityplot(tempData)
stripplot(tempData, pch = 20, cex = 1.2)



####################
### Mice for our data
####################

dat <- as.data.frame(dat)
tempData <- mice(dat, printFlag = F)
#not gonna work


#######################
### Regression for our data
#####################

#make things factors
dat.model <- dat.990
dat.model$FormYr <- as.factor(dat.model$FormYr )
dat.model$State <- as.factor(dat.model$State)
dat.model$MajorGroup <- as.factor(dat.model$MajorGroup)
dat.model$NTEE <- as.factor(dat.model$NTEE)
dat.model$NTEE.CC <- as.factor(dat.model$NTEE.CC)
dat.model$UNIV <- as.factor(dat.model$UNIV)
dat.model$HOSP <- as.factor(dat.model$HOSP)

dat.model <- na.omit(dat.model)


#run glm
mod <- glm(TotalEmployee ~ FormYr + State + MajorGroup +UNIV + HOSP + TotalExpense, family="poisson", data = dat.model )

summary(mod)

#absolutly terrible fit 
dat.plot <- data.frame(dat.model$TotalEmployee, mod$fitted.values) %>%
  filter(dat.model.TotalEmployee < 20000)
  
plot(dat.plot[, 1], dat.plot[, 2])

# Predict 
dat.pred <- dat.EZ
dat.pred$FormYr <- as.factor(dat.pred$FormYr )
dat.pred$State <- as.factor(dat.pred$State)
dat.pred$MajorGroup <- as.factor(dat.pred$MajorGroup)
dat.pred$NTEE <- as.factor(dat.pred$NTEE)
dat.pred$NTEE.CC <- as.factor(dat.pred$NTEE.CC)
dat.pred$UNIV <- as.factor(dat.pred$UNIV)
dat.pred$HOSP <- as.factor(dat.pred$HOSP)

predict(mod, dat.pred, type = "response" )
predict.glm(mod, dat.pred)


predict.glm(mod)


#next thing to do is filter out large orgs and only look at orgs with smaller assets

###############################
### Only looking at small orgs
###############################


dat.raw <- read_csv("data-raw/step-04-ceo-final.csv")


#split by 990 and 990EZ
dat.990 <- dat.raw %>%
  filter(FORMTYPE == "990")

dat.EZ <- dat.raw %>%
  filter(FORMTYPE == "990EZ")

## filter 990 data to match criteria for 990EZ filers 

dat.990.filter <- dat.990 %>%
  filter(GROSSRECEIPTS < 200000) %>%
  filter(TOTALASSETSENDYEAR < 500000) %>% 
  filter(TOTALEXPCURRENT > 90000) %>% #need to ask jesse about 
  filter(TOTEMPLOYEE > 0) #ask jesse about this 

dim(dat.990.filter) #good size data to work with 

#only keep data i want 
dat.model <- dat.990.filter %>%
  select(FormYr,FORMTYPE, NTEEFINAL, NTEE1, UNIV, HOSP, TOTEMPLOYEE, TOTNETASSETSENDYEAR, GROSSRECEIPTS, TOTALEXPCURRENT )

dat.model$FormYr <- as.factor(dat.model$FormYr )
dat.model$NTEEFINAL <- as.factor(dat.model$NTEEFINAL )
dat.model$NTEE1 <- as.factor(dat.model$NTEE1 )
dat.model$UNIV <- as.factor(dat.model$UNIV )
dat.model$HOSP <- as.factor(dat.model$HOSP )

dat.model <- na.omit(dat.model)


mod <- glm(TOTEMPLOYEE ~ FormYr + NTEEFINAL + UNIV +  HOSP + TOTNETASSETSENDYEAR + GROSSRECEIPTS  + TOTALEXPCURRENT , 
           family="poisson", data = dat.model )

dat.plot <- data.frame(dat.model$TOTEMPLOYEE, mod$fitted.values) %>%
  filter(dat.model.TOTEMPLOYEE < 100)

plot(dat.plot[, 1], dat.plot[, 2])
abline(0,1, col = 2)

#somewhat better to fit model with $90k < TOTALEXPCURRENT 











