
library(readr)
dat.raw <- read_csv("data-raw/step-04-ceo-final.csv")


urls <- dat.raw$URL
sum(is.na(urls))

library(xml2)
test <- read_xml(urls[1231])
t <- xmlParse(test)
r <- xmlToList(t)$ReturnData$IRS990

## options for where zipcode is 
r$ReturnData$IRS990$AddressPrincipalOfficerUS$ZIPCode # 2
r[["ReturnData"]][["IRS990"]][["USAddress"]] #1231

"Address" %in% names(r)



#### 2016 
index.2016 <- sample(which(dat.raw$FormYr==2016), 10)
test <- read_xml(urls[index.2016[4]])
t <- xmlParse(test)
r <- xmlToList(t)$ReturnData$IRS990
r$USAddress$ZIPCd 

#### 2015 
index.2015 <- sample(which(dat.raw$FormYr==2015), 10)
test <- read_xml(urls[index.2015[4]])
t <- xmlParse(test)
#one or the other
r <- xmlToList(t)$ReturnData$IRS990EZ
r <- xmlToList(t)$ReturnData$IRS990

r$USAddress$ZIPCd 

