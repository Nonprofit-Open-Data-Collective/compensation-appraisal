########################
### Libraries
#######################

library(dplyr)
library(ggplot2)


########################
### Load needed materials
##########################

### Load Data 
dat <- read_csv("data-wrangle/data-by-sector.csv")

### Load dat_filtering function 
source("testing/applying-filters-func.R")



############################
### Seperate data and Plot for bins
#############################

### Create data sets by Major Group

dat.I <- dat_filtering(major.group = "I")
dat.II <- dat_filtering(major.group = "II")
dat.III <- dat_filtering(major.group = "III")
dat.IV <- dat_filtering(major.group = "IV")
dat.V <- dat_filtering(major.group = "V")
dat.VI <- dat_filtering(major.group = "VI")
dat.VII <- dat_filtering(major.group = "VII")
dat.VIII <- dat_filtering(major.group = "VIII")
dat.IX <- dat_filtering(major.group = "IX")
dat.X <- dat_filtering(major.group = "X")


### Plot by Major Group 

## Group I

quants.I <- unname(quantile(dat.I$TotalExpense))

dat.I %>% 
  mutate(bin = case_when(TotalExpense > quants.I[4] ~ "XL",
                         TotalExpense < quants.I[4]  &  TotalExpense > quants.I[3]  ~ "L", 
                         TotalExpense < quants.I[3]  &  TotalExpense > quants.I[2]  ~ "M",
                         TotalExpense < quants.I[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70) +
  ggtitle( "Histogram of Major Group .I" )



## Group II

quants.II <- unname(quantile(dat.II$TotalExpense))

dat.II %>% 
  mutate(bin = case_when(TotalExpense > quants.II[4] ~ "XL",
                         TotalExpense < quants.II[4]  &  TotalExpense > quants.II[3]  ~ "L", 
                         TotalExpense < quants.II[3]  &  TotalExpense > quants.II[2]  ~ "M",
                         TotalExpense < quants.II[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70)+
  ggtitle( "Histogram of Major Group .II" )


## Group III

quants.III <- unname(quantile(dat.III$TotalExpense))

dat.III %>% 
  mutate(bin = case_when(TotalExpense > quants.III[4] ~ "XL",
                         TotalExpense < quants.III[4]  &  TotalExpense > quants.III[3]  ~ "L", 
                         TotalExpense < quants.III[3]  &  TotalExpense > quants.III[2]  ~ "M",
                         TotalExpense < quants.III[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70)+
  ggtitle( "Histogram of Major Group .III" )


## Group IV

quants.IV <- unname(quantile(dat.IV$TotalExpense))

dat.IV %>% 
  mutate(bin = case_when(TotalExpense > quants.IV[4] ~ "XL",
                         TotalExpense < quants.IV[4]  &  TotalExpense > quants.IV[3]  ~ "L", 
                         TotalExpense < quants.IV[3]  &  TotalExpense > quants.IV[2]  ~ "M",
                         TotalExpense < quants.IV[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70)+
  ggtitle( "Histogram of Major Group .IV" )


## Group V

quants.V <- unname(quantile(dat.V$TotalExpense))

dat.V %>% 
  mutate(bin = case_when(TotalExpense > quants.V[4] ~ "XL",
                         TotalExpense < quants.V[4]  &  TotalExpense > quants.V[3]  ~ "L", 
                         TotalExpense < quants.V[3]  &  TotalExpense > quants.V[2]  ~ "M",
                         TotalExpense < quants.V[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70)+
  ggtitle( "Histogram of Major Group .V" )



## Group VI

quants.VI <- unname(quantile(dat.VI$TotalExpense))

dat.VI %>% 
  mutate(bin = case_when(TotalExpense > quants.VI[4] ~ "XL",
                         TotalExpense < quants.VI[4]  &  TotalExpense > quants.VI[3]  ~ "L", 
                         TotalExpense < quants.VI[3]  &  TotalExpense > quants.VI[2]  ~ "M",
                         TotalExpense < quants.VI[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70)+
  ggtitle( "Histogram of Major Group .VI" )



## Group VII

quants.VII <- unname(quantile(dat.VII$TotalExpense))

dat.VII %>% 
  mutate(bin = case_when(TotalExpense > quants.VII[4] ~ "XL",
                         TotalExpense < quants.VII[4]  &  TotalExpense > quants.VII[3]  ~ "L", 
                         TotalExpense < quants.VII[3]  &  TotalExpense > quants.VII[2]  ~ "M",
                         TotalExpense < quants.VII[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70)+
  ggtitle( "Histogram of Major Group .VII" )

## Group VIII

quants.VIII <- unname(quantile(dat.VIII$TotalExpense))

dat.VIII %>% 
  mutate(bin = case_when(TotalExpense > quants.VIII[4] ~ "XL",
                         TotalExpense < quants.VIII[4]  &  TotalExpense > quants.VIII[3]  ~ "L", 
                         TotalExpense < quants.VIII[3]  &  TotalExpense > quants.VIII[2]  ~ "M",
                         TotalExpense < quants.VIII[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70)+
  ggtitle( "Histogram of Major Group .VIIIII" )



## Group IX

quants.IX <- unname(quantile(dat.IX$TotalExpense))

dat.IX %>% 
  mutate(bin = case_when(TotalExpense > quants.IX[4] ~ "XL",
                         TotalExpense < quants.IX[4]  &  TotalExpense > quants.IX[3]  ~ "L", 
                         TotalExpense < quants.IX[3]  &  TotalExpense > quants.IX[2]  ~ "M",
                         TotalExpense < quants.IX[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70)+
  ggtitle( "Histogram of Major Group .IX" )


## Group X

quants.X <- unname(quantile(dat.X$TotalExpense))

dat.X %>% 
  mutate(bin = case_when(TotalExpense > quants.X[4] ~ "XL",
                         TotalExpense < quants.X[4]  &  TotalExpense > quants.X[3]  ~ "L", 
                         TotalExpense < quants.X[3]  &  TotalExpense > quants.X[2]  ~ "M",
                         TotalExpense < quants.X[2] ~ "S" )) %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot()+
  geom_histogram(aes(x = TotalExpense, color = bin), bins = 70)+
  ggtitle( "Histogram of Major Group .X" )





