##########################
### Making Data used for shiny app
###########################


#### Libraries #### 
library(dplyr)
library(readr)
library(stringr)


#### Read in Data ####
dat <- read_csv("data-raw/step-04-ceo-final.csv")

#### Explanations for columns in data-by-sector #### 

# FormYr = year filed with the IRS. Options: 2009, 2010, ... , 2019
# FormType = 990 of 990EZ
# Name = name or orginization
# EIN = EIN
# State = 2 letter abbreviation for state. Options: AK, AL, ... , WY
# MajorGroup = Major group from NTEE codes from #https://nccs.urban.org/project/national-taxonomy-exempt-entities-ntee-codes
    # 1. Arts, Culture, and Humanities - A
    # 2. Education - B
    # 3. Environment and Animals - C, D
    # 4. Health - E, F, G, H
    # 5. Human Services - I, J, K, L, M, N, O, P
    # 6. International, Foreign Affairs - Q
    # 7. Public, Societal Benefit - R, S, T, U, V, W
    # 8. Religion Related - X
    # 9. Mutual/Membership Benefit - Y
    # 10. Unknown, Unclassified - Z
# NTEE = NTEE code. Options: A, B, ... , Z
# NTEE.CC = NTEE-CC code. Options A01, A02, ... , Z99
# UNIV = T or F for if organization is a university
# HOSP = T or F for if organization is a hospital
# TotalExpense = Total Expenses for that organization with the FormYr
# TotalEmployee = Total (full time equivalent) employees for that organization with the FormYr
# GrossReceipts = Gross Receipts for that organization with the FormYr
# TotalAssets = Total assests for that orginization at the end of the FormYr
# CEOCompensation= Total CEO compensation for that organization with the FormYr
# Gender = Gender of the CEO. Options: M, F, U (unknown)
# TransitionYr = TRUE if the orginization had a trasition of CEO's during that FormYr, FALSE otherwise


#### Get 2020 to 2022 inflation rate #### 
# https://www.bls.gov/data/inflation_calculator.htm
# $1 in Jan 2020 is worth $1.09 in Jan 2022
rate.2020.to.2022 <- 1.09


#### Filtering Data #### 

dat.sec <- dat %>%
  #get rid of anybody who is also a CFO
  filter(CFO == F) %>%
  #Select only the columns we care about 
  select(EIN, FilerName1, 
    NTEE1, NTEEFINAL, TOTALEXPCURRENT, TOTEMPLOYEE, UNIV, HOSP, 
    TOTCOMP, #TOTCOMP is totalComp adjusted to 2020 inflation rate
    gender, FormYr, STATE, FORMTYPE,
    GROSSRECEIPTS,TOTALASSETSENDYEAR,
    transitions, TRANS.D) %>%
  #Creating Major Group
  rename(NTEE = NTEE1) %>%
  rename(NTEE.CC = NTEEFINAL)%>%
  mutate(MajorGroup = case_when(NTEE %in% LETTERS[1] ~ 1, 
                                NTEE %in% LETTERS[2] ~ 2,
                                NTEE %in% LETTERS[3:4] ~ 3,
                                NTEE %in% LETTERS[5:8] ~ 4,
                                NTEE %in% LETTERS[9:16] ~ 5, 
                                NTEE %in% LETTERS[17] ~ 6, 
                                NTEE %in% LETTERS[18:23] ~ 7, 
                                NTEE %in% LETTERS[24] ~ 8,
                                NTEE %in% LETTERS[25] ~ 9,
                                NTEE %in% LETTERS[26] ~ 10)) %>%
  #Renaming columns
  rename(Name = FilerName1) %>%
  rename(TotalExpense = TOTALEXPCURRENT) %>%
  rename(TotalEmployee = TOTEMPLOYEE) %>%
  rename(TransitionYr = transitions) %>%
  rename(CEOCompensation = TOTCOMP) %>%
  rename(GrossReceipts = GROSSRECEIPTS) %>%
  rename(TotalAssests = TOTALASSETSENDYEAR) %>%
  rename(Gender = gender) %>%
  rename(State = STATE) %>%
  rename(FormType = FORMTYPE) %>%
  #if TotalExpense<0, assign TotalExpense=0
  mutate(TotalExpense = case_when(TotalExpense >= 0 ~ TotalExpense ,
                                  TotalExpense < 0  ~ 0))%>%
  #if TotalAssests<0, assign TotalAssests=0
  mutate(TotalAssests = case_when(TotalAssests >= 0 ~ TotalAssests ,
                                  TotalAssests < 0  ~ 0))%>%
  #Adjust for inflation , *1.09
  mutate(CEOCompensation = CEOCompensation * rate.2020.to.2022) %>%
  #Reorder columns
  relocate(FormYr, FormType, Name, EIN, State, MajorGroup, NTEE, NTEE.CC, UNIV, HOSP, 
           TotalExpense, TotalEmployee, GrossReceipts,TotalAssests,
           CEOCompensation, Gender,
           TransitionYr) 

#### Match with FIPS CODES #### 

#Our EIN's 
ein.ours  <- dat.sec$EIN

#get Fips #'s for geolocations

#only looking at the latest release from each year
links <- c("https://nccs-data.urban.org/dl.php?f=bmf/2019/bmf.bm1908.csv", 
           "https://nccs-data.urban.org/dl.php?f=bmf/2018/bmf.bm1812.csv", 
           "https://nccs-data.urban.org/dl.php?f=bmf/2017/bmf.bm1712.csv",
           "https://nccs-data.urban.org/dl.php?f=bmf/2016/bmf.bm1608.csv", 
           "https://nccs-data.urban.org/dl.php?f=bmf/2015/bmf.bm1512.csv",
           "https://nccs-data.urban.org/dl.php?f=bmf/2014/bmf.bm1412.csv", 
           "https://nccs-data.urban.org/dl.php?f=bmf/2013/bmf.bm1312.csv",
           "https://nccs-data.urban.org/dl.php?f=bmf/2012/bmf.bm1212.csv",
           "https://nccs-data.urban.org/dl.php?f=bmf/2011/bmf.bm1112.csv", 
           "https://nccs-data.urban.org/dl.php?f=bmf/2010/bmf.bm1011.csv", 
           "https://nccs-data.urban.org/dl.php?f=bmf/2009/bmf.bm0910.csv"
           )

#get all the data, but only keep the data with unique EIN numbers 

#start with the first link 
bmf.all <- read_csv(links[1])
bmf.all <- bmf.all[ , c("EIN", "ZIP5", "FIPS")]

#list of total EIN's 
ein.total <- bmf.all$EIN

#list of rows to keep 
keep.rows <- ein.total %in% ein.ours

#data set to merge 
bmf.all <- bmf.all[keep.rows, ]


#now do the other links
counter = 1
for(i in 2:length(links)){
  #upload new one 
  bmf.i <- read_csv(links[i])
  bmf.i <- bmf.i[ , c("EIN", "ZIP5", "FIPS")]
  
  #get all EIN's 
  ein.all <- bmf.all$EIN
  
  #get new EIN's 
  ein.new <- bmf.i$EIN
  
  #keep new ones 
  keep.rows <- (! (ein.new %in% ein.all)) & (ein.new %in% ein.ours)
  
  #add to data frame 
  if(sum(keep.rows >0 )){  bmf.all <- rbind(bmf.all, bmf.i[keep.rows, ])}
  
  counter <- counter +1
}
  

#merge bmf.all and dat.sec by EIN and ensure all FIPS codes have 5 digits 
dat.with.geo <- merge(dat.sec , bmf.all, by = "EIN") %>%
  #99 of them dont have FIPS #'s 
  #filter these out for now. ask Jesse how to deal with these later
  filter(!is.na(FIPS)) %>%
  #make as character without a decmil 
  mutate(FIPS = as.character(round(as.numeric(FIPS)))) %>%
  #get number of digits 
  mutate(digit =  stringr::str_length(FIPS)) %>%
  #fix the ones with 4 digits to be 5 digits
  mutate(FIPS = case_when(digit == 4 ~ paste0("0", FIPS),
                          digit == 5 ~ FIPS)) 



#### Match FIPS Codes with RUCA codes ######
#get RUCA codes from FIPS codes
dat.ruca.raw <- readxl::read_excel("data-raw/cencus-ruca-2010-revised.xlsx", 
                               skip = 1)

#format so it can be merged 
dat.ruca <- dat.ruca.raw %>%
  #select only the columns we care about
  select("State-County FIPS Code",  "Primary RUCA Code 2010" )%>%
  #rename them to something readable 
  rename( FIPS = "State-County FIPS Code", #state-county code 
          RUCA = "Primary RUCA Code 2010") %>%
  #get only the ones in our data set
  filter((FIPS %in% unique(dat.with.geo$FIPS))) %>%
  #group by FIPS.SC and take the avearge RUCA number 
  group_by(FIPS) %>%
  summarise(RUCA = mean(RUCA), .groups = "keep")


#merge 
dat.merge <- merge(dat.with.geo, dat.ruca, by = "FIPS") %>%
  #make RUCA into rual, suburban, and metropolitin 
  mutate(LocationType = case_when(RUCA <= 3 ~ "Urban", 
                                  RUCA > 3 & RUCA < 8 ~ "Suburban", 
                                  RUCA >= 8 ~ "Rural")) %>%
  #get rid of intermediate steps 
  select(-c(RUCA, digit)) %>%
  relocate(FIPS, .after = LocationType)


#### Impute EZ Total Employees #### 

#make 2 data sets for 990 and 990EZ
dat.990 <- dat.merge %>%
  filter(FormType == "990")

dat.EZ <- dat.merge %>%
  filter(FormType == "990EZ")


#filter 990 data to match criteria for 990EZ filers 
dat.model <- dat.990 %>%
  filter(GrossReceipts < 200000) %>%
  filter(TotalAssests < 500000) 


dat.model$FormYr <- as.factor(dat.model$FormYr )
dat.model$MajorGroup <- as.factor(dat.model$MajorGroup)
dat.model$NTEE <- as.factor(dat.model$NTEE )
dat.model$UNIV <- as.factor(dat.model$UNIV )
dat.model$HOSP <- as.factor(dat.model$HOSP )
dat.model$State <- as.factor(dat.model$State)


dat.model <- na.omit(dat.model)




mod <- glm(TotalEmployee ~ FormYr + MajorGroup + UNIV +  HOSP + TotalAssests + GrossReceipts  + TotalExpense , 
           family="poisson", data = dat.model )
#checking that model is okay
# dat.plot <- data.frame(dat.model$TotalEmployee, mod$fitted.values) %>%
#   filter(dat.model$TotalEmployee < 15)
# 
# plot(dat.plot[, 1], dat.plot[, 2],
#      xlab = "observed",
#      ylab = "fitted")
# abline(0,1, col = 2)
# 
# plot(mod)

## Get the imputed data
#format EZ data
dat.EZ$FormYr <- as.factor(dat.EZ$FormYr )
dat.EZ$MajorGroup <- as.factor(dat.EZ$MajorGroup)
dat.EZ$NTEE <- as.factor(dat.EZ$NTEE )
dat.EZ$UNIV <- as.factor(dat.EZ$UNIV )
dat.EZ$HOSP <- as.factor(dat.EZ$HOSP )
dat.EZ$State <- as.factor(dat.EZ$State)

#impute
dat.EZ$TotalEmployee <- stats::predict.glm(mod, dat.EZ)

#if anything is less than 0 just make it 0
dat.EZ$TotalEmployee <- ifelse(dat.EZ$TotalEmployee > 0 , dat.EZ$TotalEmployee, 0)


## Combine the two data sets 
dat.final <- rbind(dat.990, dat.EZ)


#### Save ####
write_csv(dat.final, file = "data-rodeo/dat-shinyapp.csv")





