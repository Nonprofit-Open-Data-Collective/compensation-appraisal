#########################
### Creating data-by-sector
#########################

### Libraries
library(dplyr)
library(readr)

### Read in Data
dat <- read_csv("data-raw/step-04-ceo-final.csv")

### Explinations for columns in data-by-sector

# FormYr = year filed with the IRS. Options: 2009, 2010, ... , 2019
# State = 2 letter abbreviation for state. Options: AK, AL, ... , WY
# MajorGroup = Major group from NTEE codes from #https://nccs.urban.org/project/national-taxonomy-exempt-entities-ntee-codes
      # I. Arts, Culture, and Humanities - A
      # II. Education - B
      # III. Environment and Animals - C, D
      # IV. Health - E, F, G, H
      # V. Human Services - I, J, K, L, M, N, O, P
      # VI. International, Foreign Affairs - Q
      # VII. Public, Societal Benefit - R, S, T, U, V, W
      # VIII. Religion Related - X
      # IX. Mutual/Membership Benefit - Y
      # X. Unknown, Unclassified - Z
# NTEE = NTEE code. Options: A, B, ... , Z
# NTEE.CC = NTEE-CC code. Options A01, A02, ... , Z99
# UNIV = T or F for if organization is a university
# HOSP = T or F for if organization is a hospital
# TotalExpense = Total Expenses for that organization with the FileYr
# TotalEmployee = Total employees for that organization with the FileYr
# CEOCompensation= Total CEOC compensation for that organization with the FileYr
# Gender = Gender of the CEO. Options: M, F, U (unknown)



### Filtering Data

dat.sec <- dat %>%
  #Select only the columns we care about 
  select(NTEE1, NTEEFINAL, TOTALEXPCURRENT, TOTEMPLOYEE, UNIV, HOSP, transitions, TOTCOMP, gender, FormYr, STATE, FORMTYPE) %>%
  #Creating Major Group
  rename(NTEE = NTEE1) %>%
  rename(NTEE.CC = NTEEFINAL)%>%
  mutate(MajorGroup = case_when(NTEE %in% LETTERS[1] ~ "I", 
                                NTEE %in% LETTERS[2] ~ "II",
                                NTEE %in% LETTERS[3:4] ~ "III",
                                NTEE %in% LETTERS[5:8] ~ "IV",
                                NTEE %in% LETTERS[9:16] ~ "V", 
                                NTEE %in% LETTERS[17] ~ "VI", 
                                NTEE %in% LETTERS[18:23] ~ "VII", 
                                NTEE %in% LETTERS[24] ~ "VIII",
                                NTEE %in% LETTERS[25] ~ "IX",
                                NTEE %in% LETTERS[26] ~ "X")) %>%
  #Renaming columns
  rename(TotalExpense = TOTALEXPCURRENT) %>%
  rename(TotalEmployee = TOTEMPLOYEE) %>%
  rename(TransitionYear = transitions) %>%
  rename(CEOCompensation = TOTCOMP) %>%
  rename(Gender = gender ) %>%
  rename(State = STATE) %>%
  rename(FormType = FORMTYPE) %>%
  #Reorder columns
  relocate(FormYr, State, MajorGroup, NTEE, NTEE.CC, UNIV, HOSP, TotalExpense, TotalEmployee, CEOCompensation, Gender)


### Save File 
write_csv(x = dat.sec, path = "data-wrangle/data-by-sector.csv")




