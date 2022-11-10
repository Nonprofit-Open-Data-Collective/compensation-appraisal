library(tidyverse)

#get original data 
dat.orig <- read_csv("data-rodeo/dat-shinyapp.csv")


## wrangle into needed format 
dat1 <- 
  dat.orig %>%
  #select what we need
  select(EIN, State, MajorGroup, NTEE, NTEE.CC, UNIV, HOSP, TotalEmployee, TotalExpense, ZIP5, FIPS, LocationType) %>%
  #dissect NTEE.CC to get mission levels 
  mutate(two.digit = substr(NTEE.CC, 2, 3))%>%
  mutate(tens = substr(two.digit, 1, 1)) %>%
  mutate(ones = substr(two.digit, 2, 2)) %>%
  # m# = mission level number 
  mutate(type.org = ifelse(two.digit < 20, "S", "R")) %>%
  mutate(broad.category = case_when(HOSP ~ 12, 
                        UNIV ~ 11, 
                        TRUE ~ MajorGroup)) %>%
  mutate(letters = NTEE)%>%
  mutate(decile.code = case_when(two.digit == "R" ~  paste0(letters, tens),
                        two.digit == "S" &  nchar(NTEE.CC) > 3 ~ paste0(letters, substr(NTEE.CC, 4, 4)), 
                        two.digit == "S" &  nchar(NTEE.CC) == 3 ~ paste0(letters, 0))) %>%
  #full.code requires a little more care for specialty orgs 
  mutate(full.code = case_when(two.digit == "R" ~  paste0(decile.code, ones), 
                        two.digit == "S" & nchar(NTEE.CC) > 3 ~  paste0(letters, substr(NTEE.CC, 4, 5)),
                        two.digit == "S" & nchar(NTEE.CC) == 3 ~ paste0(letters, two.digit ))) %>%
  
  ### Geography
  mutate(us.state = State != "PR" ) %>%
  mutate(city.type = LocationType) %>%
  
  #get rid of unneeded things 
  select(EIN, two.digit, broad.category, letters, decile.code, full.code, us.state, city.type) %>%
  distinct()



View(dat1)

### Checks -----------------------------


# #Checking the regular orgs worked
# # They are good 
# # ask Jesse about 3rd digit in regular orgs
dat1 %>%
  filter(two.digit =="R") %>%
  select(two.digit, tens, ones, two.digit, broad.category, letters, decile.code, full.code) %>%
  mutate(check = (NTEE.CC != full.code)) %>%
  filter(check) %>%
  View
# 
# #Checking Specialty Orgs worked
dat1 %>%
  filter(two.digit =="S") %>%
  select(two.digit, tens, ones, two.digit, broad.category, letters, decile.code, full.code) %>%
  View()


## Saving -------------------------
write_csv(dat1, "data-wrangle/mission-info.csv")
write_csv(dat1, "data-wrangle/state-network.csv")
