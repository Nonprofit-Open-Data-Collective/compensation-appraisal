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
  A <- 4 #number of attributes to compare 
  n <- dim(dat.filtered)[1] #number of points in the filtered data set
  
  #initialize storage
  D <- as.data.frame(matrix(0, nrow = n, ncol = A) )
  colnames(D) <- c("logTotalExpense", "TotalEmployee", "Geography", "Mission")
  
  #needed constants
  org.state <- org$State
  state.row.num <- which(colnames(state_dist) == org.state)

  org.us.state <- org$State == "PR"
  org.city.type <- org$Loc
  
  #get m1-m5 for org (should be put into its own function eventually)
  org.mission <- as.data.frame(org) %>%
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
                          m1 == "S" & nchar(NTEE.CC) == 3 ~ paste0(m3, two.digit )))  %>%
    select( m1, m2, m3, m4, m5)
  
    

  geo.weights <- data.frame(level = c(1, 2, 3),
                            weight = c(0.25, 0.25, 0.5/max(state_dist)))
 
  mission.weights <- data.frame(level = c(1, 2, 3, 4, 5), 
                        weight = c(5, 2, 1, 0.5, 0.25) / 8.75)
  
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
    
    ## Geographic distance 
    search.us.state <-  dat.filtered$State[i] == "PR"
    search.us.state <- ifelse(is.na(search.us.state), "NONE", search.us.state)
    g1 <- geo.weights$weight[1] * (org.us.state != search.us.state)
    
    g2 <- geo.weights$weight[2] * 
          ifelse(is.na( dat.filtered$LocationType[i]), 
                 1, 
                 org.city.type != dat.filtered$LocationType[i])
      
    
    state.col.num <- ifelse(is.na(dat.filtered$State[i]), -1,
                        which(colnames(state_dist) == dat.filtered$State[i]))
    g3 <- geo.weights$weight[3] * 
              ifelse(state.col.num < 0, 
                 max(state_dist), 
                 unlist(state_dist[state.row.num, state.col.num]))
    

    D[i, "Geography"] <- g1 + g2 + g3
    
    
    ## Mission Distance 
    search.mission <- mission_info[mission_info$EIN ==  dat.filtered$EIN[i], c("m1", "m2", "m3", "m4", "m5")]
    D[i, "Mission"] <- sum( mission.weights$weight * (search.mission == org.mission))
    
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
  
  ret <- list(dat = dat.ret,
              distance.matrix = D)
  
  return(ret)
}

##### Testing ----------------------------------------------------------
# org <- list(State = "DC", 
#             Loc = "Metropolitan",
#             MajorGroup = 2,
#             NTEE = "B", 
#             NTEE.CC = "B25",
#             UNIV = FALSE,
#             HOSP = FALSE,
#             TotalExpense = 3000000,
#             TotalEmployee = 9
# )
# search <- list(State = c("VA", "DC", "MD"),
#                MajorGroup = 2,
#                Loc = c("Metropolitan"),
#                NTEE = c("B"),
#                NTEE.CC = NA, 
#                UNIV = 1,
#                HOSP = 1,
#                TotalExpense = c(1000000, 6000000),
#                TotalEmployee = c(0, 100)
# )
# hard <- list(State = FALSE,
#              Loc = TRUE,
#              MajorGroup = TRUE,
#              NTEE = TRUE, 
#              NTEE.CC = FALSE,
#              TotalExpense = TRUE,
#              TotalEmployee = TRUE
# )
# 
# dat.filtered <- dat_filtering_hard(search, hard)
# 
# ### Do the distances 
# state_dist <- read_csv("data-wrangle/state-distance-matrix.csv")
# mission_info <- read_csv("data-wrangle/mission-info.csv")
# 
# test1 <- HEOM_with_weights(org, search, dat.filtered)
# test1$dat$dist
# 
# ggplot(test1$dat, aes(x = dist)) + geom_density()
# 
# test1$distance.matrix %>%
#   mutate(rownum = row_number()) %>%
#   pivot_longer(!rownum, names_to = "attribute", values_to = "vals") %>%
#   ggplot(aes(x = vals))+
#   geom_histogram()+
#   facet_wrap( ~ attribute)
# 
# 
# ### If any are greater than 1 or NA?
# test1$dat$EIN[which(test1$dat$dist > 1) ]
# test1$dat$EIN[which(is.na(test1$dat$dist ))]
# 
