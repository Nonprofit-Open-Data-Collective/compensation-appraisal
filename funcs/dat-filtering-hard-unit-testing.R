
### Libraries
library(dplyr)
library(readr)


######################
### dat_filtering function
#######################

dat_filtering_hard <- function(search.criteria,
                               hard.criteria, 
                               test.org.ein) {
  #testing
  #return(data.frame(search.criteria$State))
  
  ### Inputs
  # search.criteria - a list of the search criteria
  # hard.criteria - a list of TRUE or FALSE on if each search criteria should be hard or soft, respectively 
  # test.org.ein - EIN of test orginization
  
  ## See either bottom of this file or the pretend-shiny vignette for format of search.criteria and hard.criteria
  
  ### Output
  # A Tibble of all organizations that fit the specified hard criteria
  # also, if there are repeat organizations (i.e. we have data for an orginization for multiple years, we only report the most recent value)
  # also, filter out any orgs during their transition years
  
  
  ### Get data
  dat.filtered <- read_csv("data-rodeo/dat-shinyapp.csv")
  
  #remove EIN of test org
  dat.filtered <- dat.filtered %>%
    filter(EIN != test.org.ein)


  ### Filtering
  #for each search criteria we will say if(!any(is.na(search.criteria$criteria)) & hard.criteria$criteria ) then filter, otherwise do nothing


  # Filter By state
  if(!any(is.na(search.criteria$State)) & hard.criteria$State ){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(State %in% search.criteria$State)
  }

  # Filter By Location
  if(!any(is.na(search.criteria$Loc)) & hard.criteria$Loc ){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(LocationType %in% search.criteria$Loc)
  }

  # Filter By Major Group
  if(!any(is.na(search.criteria$MajorGroup)) & hard.criteria$MajorGroup){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(MajorGroup %in% search.criteria$MajorGroup)
  }

  # Filter by NTEE Code
  if(!any(is.na(search.criteria$NTEE)) & hard.criteria$NTEE){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(NTEE %in% search.criteria$NTEE)
  }

  # Filter by NTEE-CC Code
  if(!any(is.na(search.criteria$NTEE.CC)) & hard.criteria$NTEE.CC){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(NTEE.CC %in% search.criteria$NTEE.CC)
  }

#   # For EZQual, HOSP, and UNIV,
#   # 1 - no
#   # 2 - yes, any
#   # 3 - yes, only
#   
#   # EZ Qualification 
#   # https://www.irs.gov/pub/irs-pdf/i990ez.pdf
#   # dat.filtered <- dat.filtered %>%
#   #   mutate(EZQual = ifelse(TotalAssests <= 500000 & GrossReceipts <= 200000 , TRUE, FALSE))
#   # if(!any(is.na(search.criteria$EZQual)) & hard.criteria$EZQual){
#   #   if(search.criteria$EZQual == 1){ #do not include orgs that qualify for an EZ
#   #     dat.filtered <- dat.filtered %>%
#   #       dplyr::filter(EZQual == FALSE )
#   #   }else if(search.criteria$EZQual == 2){ #only include orgs that qualify for an EZ
#   #     dat.filtered <- dat.filtered %>%
#   #       dplyr::filter(EZQual = TRUE )
#   #   }
#   # }
#   # 
  # Filter by univ Code
  if(!any(is.na(search.criteria$UNIV)) ){
    if(search.criteria$UNIV == 1){ #do not include universities
      dat.filtered <- dat.filtered %>%
        dplyr::filter(UNIV == FALSE)
    }else if(search.criteria$UNIV == 3){ #only include universities
      dat.filtered <- dat.filtered %>%
        dplyr::filter(UNIV == TRUE)
    }
  }

  # Filter by hosp Code
  if(!any(is.na(search.criteria$HOSP)) ){
    if(search.criteria$HOSP == 1){ #do not include hospitals
      dat.filtered <- dat.filtered %>%
        dplyr::filter(HOSP == FALSE)
    }else if(search.criteria$HOSP == 3){ #only include hospitals
      dat.filtered <- dat.filtered %>%
        dplyr::filter(HOSP == TRUE)
    }
  }


  # Filter by total  employees
  if(!any(is.na(search.criteria$TotalEmployee)) & hard.criteria$TotalEmployee){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(TotalEmployee >= search.criteria$TotalEmployee[1] & TotalEmployee <= search.criteria$TotalEmployee[2])
  }

  # Filter by total expense
  if(!any(is.na(search.criteria$TotalExpense)) & hard.criteria$TotalExpense){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(TotalExpense >= search.criteria$TotalExpense[1] & TotalExpense <= search.criteria$TotalExpense[2])
  }


  ### Clean Up data
  dat.filtered <- dat.filtered %>%
    #get rid of any transition years
    dplyr::filter(TransitionYr == FALSE) %>%
    #remove cols TrnasitionYr, and TRANS.D
    dplyr::select(-c(TransitionYr, TRANS.D))
# 
  return(dat.filtered)
}
# 
# 
# ########## Testing 
# org <- list(State = "DC",
#             MajorGroup = 2,
#             NTEE = "B",
#             NTEE.CC = "B92",
#             UNIV = FALSE,
#             HOSP = FALSE,
#             TotalExpense = 300000,
#             TotalEmployee = 3,
#             Loc = "Metropolitan"
# )
# #search criteria
# search <- list(State = c("VA", "DC", "MD"),
#                MajorGroup = 2,
#                NTEE = NA, 
#                NTEE.CC = NA, #no hard matching on this criteria aaaa
#                UNIV = 1,
#                HOSP = 1,
#                TotalExpense = c(0, 600000),
#                TotalEmployee = c(0, 10),
#                Loc =NA
# )
# 
# #True is hard match, FALSE is soft match
# #no hard/soft option for univ or hosp
# hard <- list(State = FALSE,
#              MajorGroup = FALSE,
#              NTEE = FALSE,
#              NTEE.CC = FALSE,
#              TotalExpense = TRUE,
#              TotalEmployee = TRUE,
#              Loc = TRUE
# )
# 
# 
# 
# dat.filtered <- dat_filtering_hard(search, hard)
# 
