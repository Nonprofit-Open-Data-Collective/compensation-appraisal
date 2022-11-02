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
  mutate(m1 = ifelse(two.digit < 20, "S", "R")) %>%
  mutate(m2 = case_when(HOSP ~ 12, 
                        UNIV ~ 11, 
                        TRUE ~ MajorGroup)) %>%
  mutate(m3 = NTEE) %>%
  mutate(m4 = case_when(m1 == "R" ~  paste0(m3, tens),
                        m1 == "S" &  nchar(NTEE.CC) > 3 ~ paste0(m3, substr(NTEE.CC, 4, 4)), 
                        m1 == "S" &  nchar(NTEE.CC) == 3 ~ paste0(m3, 0))) %>%
  #m5 requires a little more care for specialty orgs 
  mutate(m5 = case_when(m1 == "R" ~  paste0(m4, ones), 
                        m1 == "S" & nchar(NTEE.CC) > 3 ~  paste0(m3, substr(NTEE.CC, 4, 5)),
                        m1 == "S" & nchar(NTEE.CC) == 3 ~ paste0(m3, two.digit ))) %>%
  
  ### Geography
  mutate(us.state = State != "PR" ) %>%
  mutate(city.type = LocationType) %>%

  #get rid of unneeded things 
  select(EIN, m1, m2, m3, m4, m5, us.state, city.type)



View(dat1)

### Checks -----------------------------


# #Checking the regular orgs worked
# # They are good 
# # ask Jesse about 3rd digit in regular orgs
# dat1 %>%
#   filter(m1 =="R") %>%
#   select(NTEE.CC, two.digit, tens, ones, m1, m2, m3, m4, m5) %>%
#   mutate(check = (NTEE.CC != m5)) %>%
#   filter(check) %>%
#   View 
# 
# #Checking Specialty Orgs worked
# dat1 %>%
#   filter(m1 =="S") %>%
#   select(NTEE.CC, two.digit, tens, ones, m1, m2, m3, m4, m5) %>%
#   View()


## Saving -------------------------
write_csv(dat1, "data-wrangle/mission-info.csv")
