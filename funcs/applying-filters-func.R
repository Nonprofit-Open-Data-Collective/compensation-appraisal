
### Libraries
library(dplyr)
library(readr)
library(ggplot2)

##
#### Read in Data
dat <- read_csv("data-wrangle/data-by-sector.csv")


##################################
### Function testing
##################################

# func.result <- dat_filtering(form.year = c(2018, 2019), 
#                              state = c("CA", "NY", "CO", "PA", "NJ", "AL"),
#                              #major.group = c("I", "X", "IV", "VII"),
#                              #ntee = c("E", "F", "G", "H"),
#                              #ntee.cc = c("E11", "H43"), 
#                              #hosp = 1,
#                              #univ = 1,
#                              #form.type = "990EZ",
#                              tot.expense = c(0, 10000))

#function to retireve data set with specified filters 

######################
### dat_filtering function
#######################3

dat_filtering <- function(form.year = NA, 
                          state = NA,
                          major.group = NA,
                          ntee =NA,
                          ntee.cc = NA,
                          univ = NA,
                          hosp = NA,
                          tot.expense = c(-Inf, Inf),
                          tot.employee = c(-Inf, Inf),
                          form.type = NA){
  
  ### Inputs
  # if any input is NA, there will be no filtering on that criteria
  # form.year = a vector of form years you want to compare to. Options are 2009 - 2019
  # state = a vector of form years you want to compare to. Options are 2009 - 2019
  # major.group = a vector of groups you want to compare to. Options are I, II, ... , X
  # ntee = a vector of NTEE codes you want to compare to . Options are A, B, C, ... Z
  # ntee.cc = a vector of NTEE-CC codes you want to compare to. Options are A01 - Z99
  # univ = NA if you want to include universities in your search, 1 if you only want to consider univerisites, 2 if you want to exclude universities
  # hosp = NA if you want to include hospitals in your search, 1 if you only want to consider hospitals, 2 if you want to exclude hospitals
  # tot.expense = range of organization yearly expenses you want to compare to 
  # tot.employee = range of organization employees you want to compare to. NOTE: not an option if form.type = "990EZ"
  # form.type = NA if all form types, "990" for only IRS-990 filers, "990EZ" for only IRS-990EZ filers

  ### Output
  # A Tibble of all organizations that fit the specified criteria
  
  dat.filtered <- read_csv("data-wrangle/data-by-sector.csv")
  
  # Filter By form.year
  if(!any(is.na(form.year))){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(FormYr %in% form.year)
  }

  # Filter By state
  if(!any(is.na(state))){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(State %in% state)
  }

  # Filter By Major Group
  if(!any(is.na(major.group))){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(MajorGroup %in% major.group)
  }

  # Filter by NTEE Code
  if(!any(is.na(ntee))){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(NTEE %in% ntee)
  }

  # Filter by NTEE-CC Code
  if(!any(is.na(ntee.cc))){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(NTEE.CC %in% ntee.cc)
  }

  # Filter by univ Code
  if(!any(is.na(univ))){
    if(univ == 1){
      dat.filtered <- dat.filtered %>%
        dplyr::filter(UNIV == TRUE)
    }else if(univ == 2){
      dat.filtered <- dat.filtered %>%
        dplyr::filter(UNIV == FALSE)
    }
  }

  # Filter by hosp Code
  if(!any(is.na(hosp))){
    if(hosp == 1){
      dat.filtered <- dat.filtered %>%
        dplyr::filter(HOSP == TRUE)
    }else if(hosp == 2){
      dat.filtered <- dat.filtered %>%
        dplyr::filter(HOSP == FALSE)
    }
  }

  # Filter by form.type Code
  if(!any(is.na(form.type))){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(FormType %in% form.type)
  }
  
  # Filter by total expenses and employees
  dat.filtered <- dat.filtered %>%
    dplyr::filter(TotalExpense > tot.expense[1] & TotalExpense < tot.expense[2])
  
  #need to still write the total employees filter
  #running into issues where there are NA employees
    
  return(dat.filtered)
}



