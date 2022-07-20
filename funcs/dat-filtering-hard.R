
### Libraries
library(dplyr)
library(readr)


######################
### dat_filtering function
#######################

dat_filtering_hard <- function(search.criteria,
                               hard.criteria) {
  
  ### Inputs
  # search.criteria - a list of the search criteria
  # hard.criteria - a list of TRUE or FALSE on if each search criteria should be hard or soft, respectivly 
 
  ### Output
  # A Tibble of all organizations that fit the specified hard criteria
  # also, if there are repeat organizations (i.e. we have data for an orginization for multiple years, we only report the most recent value)
  # also, filter out any orgs during their transition years
  
  
  ### Get data
  dat.filtered <- read_csv("data-rodeo/dat-shinyapp.csv")
  
  
  ### Filtering
  #for each search criteria we will say if(!any(is.na(search.criteria$criteria)) & hard.criteria$criteria ) then filter, otherwise do nothing
  
  # Filter By form.year
  if(!any(is.na(search.criteria$FormYr)) & hard.criteria$FormYr ){
    dat.filtered <- dat.filtered %>%
      dplyr::filter(FormYr %in% search.criteria$FormYr)
  }
  
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
  
  # For EZQual, HOSP, and UNIV,
  # 1 - no
  # 2 - yes, any
  # 3 - yes, only
  
  # EZ Qualification 
  # https://www.irs.gov/pub/irs-pdf/i990ez.pdf
  dat.filtered <- dat.filtered %>%
    mutate(EZQual = ifelse(TotalAssests <= 500000 & GrossReceipts <= 200000 , TRUE, FALSE))
  if(!any(is.na(search.criteria$EZQual)) & hard.criteria$EZQual){
    if(search.criteria$EZQual == 1){ #do not include orgs that qualify for an EZ
      dat.filtered <- dat.filtered %>%
        dplyr::filter(EZQual == FALSE )
    }else if(search.criteria$EZQual == 2){ #only include orgs that qualify for an EZ
      dat.filtered <- dat.filtered %>%
        dplyr::filter(EZQual = TRUE )
    }
  }
  
  # Filter by univ Code
  if(!any(is.na(search.criteria$UNIV)) & hard.criteria$UNIV){
    if(search.criteria$UNIV == 1){ #do not include universities 
      dat.filtered <- dat.filtered %>%
        dplyr::filter(UNIV == FALSE)
    }else if(search.criteria$UNIV == 2){ #only include universites
      dat.filtered <- dat.filtered %>%
        dplyr::filter(UNIV == TRUE)
    }
  }
  
  # Filter by univ Code
  if(!any(is.na(search.criteria$HOSP)) & hard.criteria$HOSP){
    if(search.criteria$HOSP == 1){ #do not include universities 
      dat.filtered <- dat.filtered %>%
        dplyr::filter(HOSP == FALSE)
    }else if(search.criteria$HOSP){ #only include universities
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

  
  ### Clean Up data and only report most recent year from each org
  dat.filtered <- dat.filtered %>%
    #get rid of any transition years 
    filter(TransitionYr == FALSE) %>%
    #remove cols TrnasitionYr, and TRANS.D
    select(-c(TransitionYr, TRANS.D))  %>%
    #only report most recent year for each orginization 
    group_by(EIN) %>%
    filter(FormYr==max(FormYr)) %>%
    ungroup()
  
  
  

  return(dat.filtered)
}



