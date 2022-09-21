#########################
### Libraries and data 
#########################
library(tidyverse)

dat.final <- read_csv("data-rodeo/dat-shinyapp.csv")


###########################
### Get only the orgs we care about
###########################
ein.list <- c( 383663314, #edu
               350868101, #univ
               160743979, #hosp
               208969896, #health 
               161538584, #arts
               410993136, #S03
               222681317 #S05
              )

dat.testing <- dat.final %>%
  filter(EIN %in% ein.list)

write.csv(dat.testing, "unit-testing/test-orgs.csv")

