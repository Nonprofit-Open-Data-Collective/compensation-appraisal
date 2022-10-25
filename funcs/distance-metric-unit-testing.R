HEOM_with_weights <- function(org, search, dat.filtered){
  
  ### Inputs 
  # org = the list of organization characteristics
  # search = the list of search criteria
  # dat.filtered = the data frame to compare org to, output of dat-filtering-hard function 
  #   n = # of other orgs to compare to = number of rows in dat.filtered
  
  ## See either bottom of this file or the pretend-shiny vignette for format of org and search
  
  ### Output 
  # list of (max) top 100 closest organizations with their weights
  
  ### High weighted match on matching on MajorGroup, UNIV, HOSP
  ### Low weighted match on NTEE, NTEE.CC, STATE
  ### Euclidian distance on log(Total Expense) and Total Employee
  
  #set up distance column and transform Total Expenses to a log_10 scale
  dat.filtered <- dat.filtered%>%
    dplyr::mutate(dist = NA) %>%
    dplyr::mutate(log.expense = log(TotalExpense+1, base = 10)) %>%
    dplyr::mutate(log.employee = log(TotalEmployee+1, base = 10))
  
  #needed values
  A <- 9 #number of attributes to compare 
  n <- dim(dat.filtered)[1] #number of points in the filtered data set
  
  #initialize storage
  D <- as.data.frame(matrix(0, nrow = n, ncol = A) )
  colnames(D) <- c("logTotalExpense", "TotalEmployee", "State", "MajorGroup", "NTEE", "NTEE.CC", "UNIV", "HOSP", "Loc" )
  
  #get categorical comparisons tables for probabilities 
  # State, Major Group, NTEE, NTEE.CC, UNIV, HOSP, and Location type
  # d <- 0 if org i attribute == org attribute 
  # d <- p matching else if org i attribute %in% search criteria
  # d <- 1 o.w.
  
  #tab.state <- base::table(dat.filtered$State) / n
  tab.majorgroup <- base::table(dat.filtered$MajorGroup) / n
  tab.ntee <- base::table(dat.filtered$NTEE) / n
  tab.ntee.cc <- base::table(dat.filtered$NTEE.CC) / n
  tab.univ <- base::table(dat.filtered$UNIV) / n
  tab.hosp <- base::table(dat.filtered$HOSP) / n
  tab.loc <- base::table(dat.filtered$LocationType) / n
  
  
  #Get weights for categorical variables, 
  #weight = 0 if there was a hard matching on only one option
  #weight = 0 if exact match with multiple hard matching options, = 1 if no match with multiple hard mathcing criteria
  
  #weight.state <- sum(tab.state[which(names(tab.state) %in% search$State)])
  weight.majorgroup <- sum(tab.majorgroup[which(names(tab.majorgroup) %in% search$MajorGroup)])
  weight.ntee <- sum(tab.ntee[which(names(tab.ntee) %in% search$NTEE)])
  #weight.ntee.cc <- sum(tab.ntee.cc[which(names(tab.ntee.cc) %in% search$NTEE.CC)])
  weight.loc <- sum(tab.loc[which(names(tab.loc) %in% search$Loc)])
  
  #if anything is NA assign that distance 1 
  
  #for loop
  for(i in 1:n){

    #first do numerical comparisons
    ## distance for log.expense
    if(is.na(dat.filtered$TotalExpense[i])){
      D[i, "logTotalExpense"] <- 1
    }else{
      r.log.expense <- max(dat.filtered$log.expense) -  min(dat.filtered$log.expense) #range
      D[i, "logTotalExpense"] <- abs(dat.filtered$log.expense[i] - log(org$TotalExpense, 10)) / r.log.expense
    }
   
    ## distance for total employee
    if(is.na(dat.filtered$TotalEmployee[i])){
      D[i, "TotalEmployee"] <- 1
    }else{
      r.log.employee <- max(dat.filtered$log.employee) -  min(dat.filtered$log.employee) #range
      D[i, "TotalEmployee"] <- abs(dat.filtered$log.employee[i] - log(org$TotalEmployee+1, 10)) / r.log.employee
    }
    

    # Next do categorical comparisons
    #State has its own function
    if(is.na(dat.filtered$State[i])){
      D[i, "State"] <- 1
    }else{
      D[i, "State"] <- state_distance(org$State, dat.filtered$State[i])
    }
    
    # Every Categorical but state uses proportions as weights
    if(is.na(dat.filtered$MajorGroup[i])){
      D[i, "MajorGroup"] <- 1
    }else{
      D[i, "MajorGroup"] <- ifelse(dat.filtered$MajorGroup[i] == org$MajorGroup, 0,
                                 ifelse(dat.filtered$MajorGroup[i] %in% search$MajorGroup, weight.majorgroup, 1 ))
    }
    
    if(is.na(dat.filtered$NTEE[i])){
      D[i, "NTEE"] <- 1
    }else{
      D[i, "NTEE"] <- ifelse(dat.filtered$NTEE[i] == org$NTEE, 0,
                             ifelse(dat.filtered$NTEE[i] %in% search$NTEE, weight.ntee, 1 ))
    }
    
    if(is.na(dat.filtered$LocationType[i] )){
      D[i, "Loc"] <- 1
    }else{
      D[i, "Loc"] <-  ifelse(dat.filtered$LocationType[i] == org$Loc, 0,
                             ifelse(dat.filtered$LocationType[i] %in% search$Loc, weight.loc, 1 ))
    }
    
    if(is.na(dat.filtered$UNIV[i] )){
      D[i, "UNIV"] <- 1
    }else{
      D[i, "UNIV"] <- ifelse(dat.filtered$UNIV[i] == org$UNIV, 0 , 1)
    }
    
    if(is.na(dat.filtered$HOSP[i] )){
      D[i, "HOSP"] <- 1
    }else{
      D[i, "HOSP"] <- ifelse(dat.filtered$HOSP[i] == org$HOSP, 0 , 1)
    }
    
    


  }

  # Get distance
  # this number will be between 0 and 1
  # closer to 0 = filtered org is closer to input org, closer to 1 = filtered or is farther away from input org
  dat.filtered$dist <- rowSums(D) / A #normalize by number of matched attributes.

  dat.ret <- dat.filtered %>%
    dplyr::arrange(dist) %>%
    dplyr::select(-c(log.expense, log.employee))%>%
    #slice(1:100) %>%
    dplyr::mutate(Rank = row_number()) %>%
    dplyr::relocate(Rank)

  
  ret <- list(dat.filtered, D)
  names(ret) <- c("TableWithDistance", "DistanceMatrix")
  return(ret)
}

### testing 

# View(HEOM_with_weights(org, dat.filtered))


########### Example for mid summer presentation ##########3

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
#                NTEE = c("B"),
#                NTEE.CC = NA, #no hard matching on this criteria aaaa
#                UNIV = 1,
#                HOSP = 1,
#                TotalExpense = c(0, 600000),
#                TotalEmployee = c(0, 10),
#                Loc = c("Metropolitan")
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
# source("funcs/dat-filtering-hard.R")
# 
# dat.filtered <- dat_filtering_hard(search, hard)
# 
# results <- HEOM_with_weights(org, search, dat.filtered)
# 
# 
# 
