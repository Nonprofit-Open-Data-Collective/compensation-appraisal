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
  mutate(letter = NTEE)%>%
  mutate(letter1 = case_when(type.org == "R" ~  paste0(letter, tens),
                             type.org == "S" ~ paste0(letter, 0))) %>%
                        # two.digit == "S" &  nchar(NTEE.CC) > 3 ~ paste0(letter, substr(NTEE.CC, 4, 4)), 
                        # two.digit == "S" &  nchar(NTEE.CC) == 3 ~ paste0(letter, 0))) %>%
  mutate(letter2 = case_when(type.org == "R" ~  paste0(letter1, ones), 
                             type.org == "S" ~ paste0(letter, two.digit) ) ) %>%
                        # two.digit == "S" & nchar(NTEE.CC) > 3 ~  paste0(letter, substr(NTEE.CC, 4, 5)),
                        # two.digit == "S" & nchar(NTEE.CC) == 3 ~ paste0(letter, two.digit ))) %>%
  mutate(s.further = ifelse(type.org == "S" & nchar(NTEE.CC) > 3,paste0(letter, substr(NTEE.CC, 4, 5)), NA )) %>%
  
  ### Geography
  mutate(us.state = State != "PR" ) %>%
  mutate(city.type = LocationType) %>%
  
  #get rid of unneeded things 
  select(EIN, type.org, two.digit, broad.category, letter, letter1, letter2, s.further, us.state, city.type) %>%
  distinct()



View(dat1)

### Checks -----------------------------


# #Checking the regular orgs worked
# # They are good 
# # ask Jesse about 3rd digit in regular orgs
dat1 %>%
  filter(two.digit =="R") %>%
  select(two.digit, tens, ones, two.digit, broad.category, letter, letter1, letter2) %>%
  mutate(check = (NTEE.CC != letter2)) %>%
  filter(check) %>%
  View
# 
# #Checking Specialty Orgs worked
dat1 %>%
  filter(two.digit =="S") %>%
  select(two.digit, tens, ones, two.digit, broad.category, letter, letter1, letter2) %>%
  View()


## Saving -------------------------
write_csv(dat1, "data-wrangle/mission-info-v02.csv")
