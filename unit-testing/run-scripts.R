#######################
### Load Libraries and data
#######################

library(tidyverse)

source("funcs/applying-filters-func.R")
source("funcs/dat-filtering-hard-unit-testing.R") #removes test org EIN from comparison set
source("funcs/distance-metric-unit-testing.R")
source("funcs/dollarize.R")
source("funcs/state-distance.R")


dat.testing <- read_csv("unit-testing/test-orgs.csv")


#######################
### Form the `orgs` input argument 
#########################

orgs.list <- list()
for(i in 1:nrow(dat.testing)){
  temp.dat <- dat.testing[i, ]
  temp.list <- list(State = temp.dat$State, 
                    Loc = temp.dat$LocationType,
                    MajorGroup = temp.dat$MajorGroup,
                    NTEE = temp.dat$NTEE, 
                    NTEE.CC = temp.dat$NTEE.CC,
                    UNIV = temp.dat$UNIV,
                    HOSP = temp.dat$HOSP,
                    TotalExpense = temp.dat$TotalExpense,
                    TotalEmployee = temp.dat$TotalEmployee
  )
  orgs.list[[i]] <- temp.list
}


#######################
### Making the Search Input
#########################3

search.list <- list()
for(i in 1:nrow(dat.testing)){
  search.list[[i]] <- list(State = orgs.list[[i]]$State,
                           MajorGroup = orgs.list[[i]]$MajorGroup,
                           Loc = orgs.list[[i]]$Loc,
                           NTEE = orgs.list[[i]]$NTEE,
                           NTEE.CC = NA, 
                           UNIV = 1,
                           HOSP = 1,
                           TotalExpense = c(1000000, 6000000),
                           TotalEmployee = c(0, 100))
}


#######################
### Making the Hard Input
#########################

hard.list <- list()
for(i in 1:nrow(dat.testing)){
  hard.list[[i]] <- list(State = FALSE,
                         Loc = FALSE,
                         MajorGroup = TRUE,
                         NTEE = TRUE, 
                         NTEE.CC = FALSE,
                         TotalExpense = FALSE,
                         TotalEmployee = FALSE)

}

###########################
### Generating Reports 
##########################

#Create folder you want inside compensation-appraisal/unit-testing/test-results
folder.name <- "testB"
#dir.create(paste0("unit-testing/test-results/", folder.name))


#Generate reports (this should take <5 minutes)

for(i in 1:nrow(dat.testing)){
  #path to folder.name 
  path.result <-  paste0("test-results/", folder.name, "/report-testing-org-", i, ".pdf")
  
  #render document
  rmarkdown::render( input='unit-testing/report-testing.Rmd', 
                     output_file = path.result,
                     params = list( org = orgs.list[[i]],
                                    search = search.list[[i]],
                                    hard = hard.list[[i]],
                                    testorg = dat.testing[i, ]))
  
}
