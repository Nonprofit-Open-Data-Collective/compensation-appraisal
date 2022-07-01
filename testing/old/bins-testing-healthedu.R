#############################
### Filtering By Sector
###############################
### Libraries
library(dplyr)
library(readr)
library(ggplot2)

##
#### Read in Data
dat <- read_csv("data-raw/step-04-ceo-final.csv")

#### Aggregate by health and education with gender 

dat.wrang <- dat %>%
  select(HEALTH, EDU, HOSP, UNIV, TOTALEXPCURRENT, TOTEMPLOYEE, transitions) %>%
  #make a sector
  mutate( sector= case_when(HEALTH == T ~ "health",
                               EDU == T ~ "edu")) %>%
  #make a subsector column
  mutate(subsector = case_when(HEALTH==T & EDU == F & HOSP==T & UNIV == F ~ "health-hosp",
                            HEALTH==T & EDU == F & HOSP==F & UNIV == F ~ "health-not-hosp",
                            HEALTH==F & EDU == T & HOSP==F & UNIV == T ~ "edu-univ",
                            HEALTH==F & EDU == T & HOSP==F & UNIV == F ~ "edu-not-univ") ) %>%
 
  #remove transitional years
  filter(!transitions )%>% 
  #remove NA sector %>%
  filter(!is.na(sector))%>%
  select(TOTALEXPCURRENT, TOTEMPLOYEE, sector, subsector)

#get range of total expenses range by sector
dat.range <- dat.wrang$TOTALEXPCURRENT %>%
  aggregate(list(sec = factor(dat.wrang$subsector)), range) %>%
  rename(comp.mean = x )
dat.range

### Box plot of each subsector
ggplot(dat.wrang) +
  geom_boxplot(aes(TOTALEXPCURRENT)) +
  facet_wrap(vars(subsector),  scales = "free_x") +
  xlab("Total Expenses ")+
  ggtitle("Total Expenses by Sector")

### Get rid of anything over 100Million

dat.small <- dat.wrang %>%
  filter(TOTALEXPCURRENT < 1e8)


ggplot(dat.small) +
  geom_histogram(aes(TOTALEXPCURRENT), bins = 50) +
  facet_wrap(vars(subsector),  scales = "free") +
  xlab("Expenses ")

#you really need to be looking at the sepreatly 


####################################
### Boxes for Expenses by Sector with no subsector 
####################################

### Education 
dat.edu <- dat.wrang %>%
  filter(sector == "edu")

quants.edu <- unname(quantile(dat.edu$TOTALEXPCURRENT))


dat.edu.bins <- dat.edu %>% 
  mutate(bin = case_when(TOTALEXPCURRENT > quants.edu[4] ~ "XL",
                         TOTALEXPCURRENT < quants.edu[4]  &  TOTALEXPCURRENT > quants.edu[3]  ~ "L", 
                         TOTALEXPCURRENT < quants.edu[3]  &  TOTALEXPCURRENT > quants.edu[2]  ~ "M",
                         TOTALEXPCURRENT < quants.edu[2] ~ "S" )) 

# Histogram with the XL;s in it 
dat.edu.bins %>% 
  ggplot() +
  geom_histogram(aes(x = TOTALEXPCURRENT, color = bin), bins = 50) +
  xlab("Total Expenses") %>%
  ggtitle("All quartiles of sector = edu") 

# Histogram of lower 3 quartiles
dat.edu.bins %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot() +
    geom_histogram(aes(x = TOTALEXPCURRENT, color = bin),bins = 50) +
    xlab("Total Expenses") %>%
    ggtitle("Lower 3 quartiles of sector = edu") 

# Histogram of upper quaritile
dat.edu.bins %>%
  filter(bin == "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot() +
  geom_histogram(aes(x = TOTALEXPCURRENT, color = bin),bins = 50) +
  xlab("Total Expenses") %>%
  ggtitle("Upper quartile of sector = edu") 




### Health 
dat.health <- dat.wrang %>%
  filter(sector == "health")

quants.health <- unname(quantile(dat.health$TOTALEXPCURRENT))


dat.health.bins <- dat.health %>% 
  mutate(bin = case_when(TOTALEXPCURRENT > quants.health[4] ~ "XL",
                         TOTALEXPCURRENT < quants.health[4]  &  TOTALEXPCURRENT > quants.health[3]  ~ "L", 
                         TOTALEXPCURRENT < quants.health[3]  &  TOTALEXPCURRENT > quants.health[2]  ~ "M",
                         TOTALEXPCURRENT < quants.health[2] ~ "S" )) 

# Histogram with the XL;s in it 
dat.health.bins %>% 
  ggplot() +
  geom_histogram(aes(x = TOTALEXPCURRENT, color = bin), bins = 50) +
  xlab("Total Expenses") %>%
  ggtitle("All quartiles of sector = health") 

# Histogram of lower 3 quartiles
dat.health.bins %>%
  filter(bin != "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot() +
  geom_histogram(aes(x = TOTALEXPCURRENT, color = bin),bins = 50) +
  xlab("Total Expenses") %>%
  ggtitle("Lower 3 quartiles of sector = health") 

# Histogram of upper quaritile
dat.health.bins %>%
  filter(bin == "XL") %>% #take out XL orgs because it makes the plot look messy 
  ggplot() +
  geom_histogram(aes(x = TOTALEXPCURRENT, color = bin),bins = 50) +
  xlab("Total Expenses") %>%
  ggtitle("Upper quartile of sector = health") 




####################################
### Sector with subsector 
####################################

### EDU SubSectors

dat.edu.univ <- dat.edu %>%
  filter(subsector == "edu-univ")

dat.edu.notuniv <- dat.edu %>%
  filter(subsector == "edu-not-univ")










