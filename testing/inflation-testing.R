### blscrapR pacakge

library(devtools)
install_github("keberwein/blscrapeR")
library(blscrapeR)
df <- inflation_adjust(2009)
tail(df)


###### priceR
#https://cran.r-project.org/web/packages/priceR/priceR.pdf
install.packages("priceR")
library(priceR)


test <- dat.raw[1,]
inflation_dataframe <- retrieve_inflation_data("US")
countries_dataframe <- show_countries()
adjust_for_inflation(price = 100, #test$TOTNETASSETSBEGYEAR, 
                       from_date = 2009, country = "US", to_date = 2021,
                     inflation_dataframe = inflation_dataframe)


### raw dat 
library(readr)
dat.raw <- read_csv("data-raw/step-04-ceo-final.csv")
View(dat.raw)


