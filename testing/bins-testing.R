#############################
### Filtering By Sector
###############################
### Libraries
library(dplyr)
library(ggplot2)

##
#### Read in Data
dat <- read_csv("data-raw/step-04-ceo-final.csv")

#### Aggrigate by health and education with gender 

dat.wrang <- dat %>%
  select(HEALTH, EDU, HOSP, UNIV, TOTALEXPCURRENT, TOTEMPLOYEE, transitions) %>%
  #make a sector column
  mutate(sector = case_when(HEALTH==T & EDU == F & HOSP==F & UNIV == F ~ "health", 
                            HEALTH==T & EDU == F & HOSP==T & UNIV == F ~ "hosp",
                            HEALTH==F & EDU == T & HOSP==F & UNIV == F ~ "edu",
                            HEALTH==F & EDU == T & HOSP==F & UNIV == T ~ "univ") ) %>%
  #remove transitional years
  filter(!transitions )%>% 
  #remove NA sector %>%
  filter(!is.na(sector))%>%
  select(TOTALEXPCURRENT, TOTEMPLOYEE, sector)

#get total revanue range by sector
dat.range <- dat.wrang$TOTALEXPCURRENT %>%
  aggregate(list(sec = factor(dat.wrang$sector)), range) %>%
  rename(comp.mean = x )


### Box plot of each sector
ggplot(dat.wrang) +
  geom_boxplot(aes(TOTALEXPCURRENT)) +
  facet_wrap(vars(sector),  scales = "free_x") +
  xlab("Total Expenses ")+
  ggtitle("Total Expenses by Sector")

### Get rid of anything over 100Million

dat.small <- dat.wrang %>%
  filter(TOTALEXPCURRENT < 1e8)


ggplot(dat.small) +
  geom_histogram(aes(TOTALEXPCURRENT), bins = 50) +
  facet_wrap(vars(sector),  scales = "free") +
  xlab("revanue ")

#you really need to be looking at the sepreatly 




