HEOM_with_weights <- function(org, dat.filtered){
  
  ### Inputs 
  # org = the organization characteristics
  # dat.filtered = the data frame to compare org to 
  #   A = # of attributes to compare on
  #   n = # of other orgs to compare to
  #   dim(dat) = n - by - A
  
  ### Output 
  # list of top 100 closest organizations
  
  ### High weighted match on matching on MajorGroup, UNIV, HOSP
  ### Low weighted match on NTEE, NTEE.CC, STATE
  ### Euclidian distance on log(Total Expense) and Total Employee
  
  #get rid of that transitioned that year
  dat.filtered <- dat.filtered%>%
    mutate(dist = NA) %>%
    mutate(log.expense = log(TotalExpense+1, base = 10)) %>%
    filter(TransitionYr == FALSE)
  
  
  #needed values
  A <- 8 #number of attributes to compare 
  n <- dim(dat.filtered)[1] #number of points in the data set
  
  #initialize storage
  D <- as.data.frame(matrix(0, nrow = n, ncol = A) )
  colnames(D) <- c("MajorGroup", "UNIV", "HOSP", "NTEE", "NTEE.CC", "State", "logTotalExpense", "TotalEmployee")
  
  ## distance for Major Group
  for(i in 1:n){
    D[i, "MajorGroup"] <- ifelse(dat.filtered$MajorGroup[i] != org$MajorGroup, 1 ,0) 
  }
  
  ## distance for UNIV
  for(i in 1:n){
    D[i, "UNIV"] <- ifelse(dat.filtered$UNIV[i] != org$UNIV, 1 ,0) 
  }
  
  ## distance for HOSP
  for(i in 1:n){
    D[i, "HOSP"] <- ifelse(dat.filtered$HOSP[i] != org$HOSP, 1 ,0) 
  }
  
  ## distance for NTEE
  for(i in 1:n){
    D[i, "NTEE"] <- ifelse(dat.filtered$NTEE[i] != org$NTEE, 0.5 ,0) 
  }
  
  ## distance for NTEE.CC
  for(i in 1:n){
    D[i, "NTEE.CC"] <- ifelse(dat.filtered$NTEE.CC[i] != org$NTEE.CC, 0.5 ,0) 
  } 
  
  ## distance for STATE
  for(i in 1:n){
    D[i, "STATE"] <- ifelse(dat.filtered$State[i] != org$State, 0.5 ,0) 
  }
  
  ## distance for log.expense
  r.log.expense <- max(dat.filtered$log.expense) -  min(dat.filtered$log.expense)
  for(i in 1:n){
    D[i, "logTotalExpense"] <- abs(dat.filtered$log.expense[i] - log(org$TotalExpense, 10)) / r.log.expense
  }
  
  ## distance for total employee
  r.employee <- max(dat.filtered$TotalEmployee) -  min(dat.filtered$TotalEmployee)
  for(i in 1:n){
    D[i, "TotalEmployee"] <- abs(dat.filtered$TotalEmployee[i] - org$TotalEmployee) / r.employee
  }
  
  
  dat.filtered$dist <- rowSums(D)
  
  dat.ret <- dat.filtered %>%
    arrange(dist) %>%
    select(-c(Gender, TransitionYr, TRANS.D, dist, log.expense))%>%
    slice(1:100) %>%
    mutate(Rank = row_number()) %>%
    relocate(Rank)
  
  return(dat.ret)
}

### testing 
# 
# org <- list(MajorGroup = 1,
#             FormYr = 2019,
#             State = "GA",
#             NTEE= "A",
#             NTEE.CC = "A23",
#             UNIV = F,
#             HOSP = F,
#             TotalExpense = 129323,
#             TotalEmployee = 23)
# dat.filtered <-  dat_filtering(form.year = c( 2019),
#                                              state = c("GA", "CA", "LA"),
#                                              major.group = c(1),
#                                              #ntee = c("E", "F", "G", "H"),
#                                              #ntee.cc = c("E11", "H43"),
#                                              #hosp = 1,
#                                              #univ = 1,
#                                              #form.type = "990EZ",
#                                              #tot.expense = c(0, 10000)
# )
# 
# View(HEOM_with_weights(org, dat.filtered))
