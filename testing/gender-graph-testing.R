#######################
### Gender Graph Testing
#######################

### Libraries
library(dplyr)
library(ggplot2)

#### Read in Data
dat <- read_csv("data-raw/step-04-ceo-final.csv")

#### Aggrigate by health and education with gender 

dat.wrang <- dat %>%
  select(HEALTH, EDU, HOSP, UNIV, totalComp, gender, transitions) %>%
  #make a sector column
  mutate(sector = case_when(HEALTH==T & EDU == F & HOSP==F & UNIV == F ~ "health", 
                            HEALTH==T & EDU == F & HOSP==T & UNIV == F ~ "hosp",
                            HEALTH==F & EDU == T & HOSP==F & UNIV == F ~ "edu",
                            HEALTH==F & EDU == T & HOSP==F & UNIV == T ~ "univ") ) %>%
  #remove transitional years
  filter(!transitions )%>%
  #remove unknown gender
  filter(gender != "U")%>%
  select(totalComp, sector, gender)

dat.agg <- dat.wrang$totalComp %>%
  aggregate(list(sec = factor(dat.wrang$sector), gen = factor(dat.wrang$gender)), median) %>%
  rename(comp.mean = x )

ggplot(dat.agg)+ 
  geom_point(aes(x = comp.mean, y = sec, color = gen)) +
  ylab("Sector") +
  xlab("Median Compensation") +
  ggtitle("Median Compensations by Sector")

