##########################
### Making inflation adjusted step-04-final.csv
###########################

library(dplyr)
library(blscrapeR)

dat <- read_csv("data-raw/step-04-ceo-final.csv")

### Getting the inflation adjustments 
inflation <- data.frame(year = 2009:2019,
                        rate.2022 = NA)

for(i in 1:11){
  df <- inflation_adjust(inflation$year[i])
  row <- which(df$year == 2022)
  inflation$rate.2022[i] <- df$adj_value[row]
}

### Get inflation function 

get_2022_dollar <- function(old.price, year){
  inf <- inflation$rate.2022[which(inflation$year == year)]
  new.price = old.price * inf
  return(new.price)
}

### Adjusting the table appropriately 

#columns that need to be adjusted 
money.col <- c("TOTNETASSETSBEGYEAR", "TOTNETASSETSENDYEAR",
               "TOTLIABNABEGYEAR", "TOTLIABNAENDYEAR" ,
               "GROSSRECEIPTS" ,
               "CONTRIBPRIOR", "CONTRIBCURRENT", "PSRPRIOR", "PSRCURRENT", 
               "INVINCPRIOR", "INVINCCURRENT", "OTHERREVPRIOR", "OTHERREVCURRENT",
               "TOTALREVPRIOR", "TOTALREVCURRENT", "SALARIESPRIOR", "SALARIESCURRENT",
               "TOTFUNDEXP", "TOTALEXPPRIOR", "TOTALEXPCURRENT", "REVLESSEXPPRIOR",
               "REVLESSEXPCURRENT", "TOTALASSETSBEGYEAR, TOTALASSETSENDYEAR", "TOTALLIABBEGYEAR",
               "TOTALLIABENDYEAR", "NETASSETSBEGYEAR", "OTHERASSETSCHANGES", "NETASSETSENDYEAR",
               "CASHBEGYEAR", "CASHENDYEAR", "SAVINVBEGYEAR", "SAVINVENDYEAR",
               "TOTALPROGSERVEXP", "TOTCOMP" )

#making the data set
dat.inflation.adj <- dat
for(i in length(money.col)){
  var.name <- money.col[i]
  for(j in 1:dim(dat.inflation.adj)[1]){
    old.dollar <- as.numeric(dat.inflation.adj[j, var.name])
    if(! is.na(old.dollar)){
      dat.inflation.adj[i, c(var.name)] <- get_2022_dollar(old.price = old.dollar,
                                             year = dat.inflation.adj$FormYr[j])
    }#end if
  }# end for j
}#end for i


### save data set 
write_csv(dat.inflation.adj, "data-wrangle//step-04-ceo-final-inflation-adjusted.csv")

