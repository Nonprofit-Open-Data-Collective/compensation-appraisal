HEOM_with_weights <- function(org, search, dat.filtered){
  
  ### Inputs 
  # org = the list of organization characteristics
  # search = the list of search criteria
  # dat.filtered = the data frame to compare org to, output of dat-filtering-hard function 
  #   n = # of other orgs to compare to = number of rows in dat.filtered 
  
  ### Output 
  # list of (max) top 100 closest organizations with their weights
  
  ### High weighted match on matching on MajorGroup, UNIV, HOSP
  ### Low weighted match on NTEE, NTEE.CC, STATE
  ### Euclidian distance on log(Total Expense) and Total Employee
  
  #set up distance column and transform Total Expenses to a log_10 scale
  dat.filtered <- dat.filtered%>%
    mutate(dist = NA) %>%
    mutate(log.expense = log(TotalExpense+1, base = 10))
  
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
  
  tab.state <- base::table(dat.filtered$State) / n
  tab.majorgroup <- base::table(dat.filtered$MajorGroup) / n
  tab.ntee <- base::table(dat.filtered$NTEE) / n
  tab.ntee.cc <- base::table(dat.filtered$NTEE.CC) / n
  tab.univ <- base::table(dat.filtered$UNIV) / n
  tab.hosp <- base::table(dat.filtered$HOSP) / n
  tab.loc <- base::table(dat.filtered$LocationType) / n
  
  
  #get regions for state match # https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
  # 1 = New England 
  # 2 = Mid Atlantic 
  # 3 = East North Central
  # 4 = West North Central
  # 5 = South Atlantic 
  # 6 = East south central 
  # 7 = West South Central
  # 8 = Mountain 
  # 9 = Pacific
  
  #get regions with DC and PR
  region <- data.table::data.table(state = sort(c(state.abb, "PR", "DC")),
                                   region = c(datasets::state.division[1:7], 
                                               5, 
                                               datasets::state.division[8: 38], 
                                               5, 
                                               datasets::state.division[39:50]))

  #Get weights for categorical variables, 
  #weight = 0 if there was a hard matching on only one option
  #weight = 0 if exact match with multiple hard matching options, = 1 if no match with multiple hard mathcing criteria
  
  weight.state <- sum(tab.state[which(names(tab.state) %in% search$State)])
  weight.majorgroup <- sum(tab.majorgroup[which(names(tab.majorgroup) %in% search$MajorGroup)])
  weight.ntee <- sum(tab.ntee[which(names(tab.ntee) %in% search$NTEE)])
  #weight.ntee.cc <- sum(tab.ntee.cc[which(names(tab.ntee.cc) %in% search$NTEE.CC)])
  weight.loc <- sum(tab.loc[which(names(tab.loc) %in% search$Loc)])
  
  
  #for loop
  for(i in 1:n){
    
    #first do numerical comparisons 
    ## distance for log.expense
    r.log.expense <- max(dat.filtered$log.expense) -  min(dat.filtered$log.expense) #range
    D[i, "logTotalExpense"] <- abs(dat.filtered$log.expense[i] - log(org$TotalExpense, 10)) / r.log.expense
    
    ## distance for total employee
    r.employee <- max(dat.filtered$TotalEmployee) -  min(dat.filtered$TotalEmployee) #range
    D[i, "TotalEmployee"] <- abs(dat.filtered$TotalEmployee[i] - org$TotalEmployee) / r.employee
    
    
    # Next do categorical comparisons
    D[i, "State"] <- ifelse(dat.filtered$State[i] == org$State, 0, #if they match assign 
                            ifelse(dat.filtered$State[i] %in% search$State, weight.state, 1 ))
    
    # Every Categorical but state uses proportions as weights
    D[i, "MajorGroup"] <- ifelse(dat.filtered$MajorGroup[i] == org$MajorGroup, 0,
                                 ifelse(dat.filtered$MajorGroup[i] %in% search$MajorGroup, weight.majorgroup, 1 ))
    
    D[i, "NTEE"] <- ifelse(dat.filtered$NTEE[i] == org$NTEE, 0,
                                 ifelse(dat.filtered$NTEE[i] %in% search$NTEE, weight.ntee, 1 ))
    
    # D[i, "NTEE.CC"] <- ifelse(dat.filtered$NTEE.CC[i] == org$NTEE.CC, 0,
    #                        ifelse(dat.filtered$NTEE.CC[i] %in% search$NTEE.CC, weight.ntee.cc, 1 ))
    
    D[i, "Loc"] <-  ifelse(dat.filtered$LocationType[i] == org$Loc, 0,
                           ifelse(dat.filtered$LocationType[i] %in% search$Loc, weight.loc, 1 ))
    
    D[i, "UNIV"] <- ifelse(dat.filtered$UNIV[i] == org$UNIV, 0 , 1)
    D[i, "HOSP"] <- ifelse(dat.filtered$HOSP[i] == org$HOSP, 0 , 1)
    
    
  }
  
  # Get distance
  #this number will be between 0 and 1
  # closer to 0 = filtered org is closer to input org, closer to 1 = filtered or is farther away from input org
  dat.filtered$dist <- rowSums(D) / A #normalize by number of matched attributes. 
  
  dat.ret <- dat.filtered %>%
    arrange(dist) %>%
    select(-c(log.expense))%>%
    #slice(1:100) %>%
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


########### Example for mid summer presentation ##########3
# 
# org <- list(FormYr = 2019,
#             State = "GA",
#             MajorGroup = 3,
#             NTEE = "C", #need to add input for this
#             NTEE.CC = "B01", #need to add input for this
#             UNIV = FALSE,
#             HOSP = FALSE,
#             TotalExpense = 1000000,
#             TotalEmployee = 12,
#             FormType = "990"
# )
# 
# dat.filtered <- dat_filtering(form.year = 2019,
#                               state = c("GA", "SC", "AL", "NC", "TN", "MS"),
#                               major.group = 3,
#                               ntee = NA, #need to add input for this
#                               ntee.cc = NA, #need to add input for this
#                               hosp = 2,
#                               univ = 2,
#                               tot.expense = c(500000, 10000000),
#                               tot.employee = c(0, Inf),
#                               form.type = "990")
# 
# ret <- HEOM_with_weights(org, dat.filtered)
# 
# View(ret)
# 
# library(knitr)
# 
# dat.tab <- ret %>% 
#   select(-c(Rank, EIN, FormYr, FormType, GrossReceipts, ZIP5, FIPS))
# dat.tab <- dat.tab[c(1, 3,5, 7,9,11, 13, 15, 17, 19, 21), ]
# 
# View(dat.tab)
# 
# write_csv(dat.tab, path = "/Users/oliviabeck/Desktop/Presentation.csv")
