HEOM_with_weights <- function(org, search, dat.filtered, weights){
  
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
    dplyr::mutate(log.expense = log(TotalExpense+1, base = 10)) 
  
  #needed values
  A <- 3 #number of attributes to compare 
  n <- dim(dat.filtered)[1] #number of points in the filtered data set
  
  #initialize storage
  D <- as.data.frame(matrix(0, nrow = n, ncol = A) )
  colnames(D) <- c("logTotalExpense", "Geography", "Mission")
  
  #needed constants
  org.state <- org$State
  state.row.num <- which(colnames(state_dist) == org.state)
  
  org.us.state <- org$State == "PR"
  org.city.type <- org$Loc
  
  #get m1-m5 for org (should be put into its own function eventually)
  org.mission <- as.data.frame(org) %>%
    #select what we need
    select(State, MajorGroup, NTEE, NTEE.CC, UNIV, HOSP, TotalExpense) %>%
    #dissect NTEE.CC to get mission levels 
    mutate(two.digit = substr(NTEE.CC, 2, 3))%>%
    mutate(tens = substr(two.digit, 1, 1)) %>%
    mutate(ones = substr(two.digit, 2, 2)) %>%
    # m# = mission level number 
    mutate(type.org = ifelse(two.digit < 20, "S", "R")) %>%
    mutate(broad.category = case_when(HOSP ~ 12, 
                                      UNIV ~ 11, 
                                      TRUE ~ MajorGroup)) %>%
    mutate(letter = NTEE)%>%
    mutate(letter1 = case_when(type.org == "R" ~  paste0(letter, tens),
                               type.org == "S" ~ paste0(letter, 0))) %>%
    # two.digit == "S" &  nchar(NTEE.CC) > 3 ~ paste0(letter, substr(NTEE.CC, 4, 4)), 
    # two.digit == "S" &  nchar(NTEE.CC) == 3 ~ paste0(letter, 0))) %>%
    mutate(letter2 = case_when(type.org == "R" ~  paste0(letter1, ones), 
                               type.org == "S" ~ paste0(letter1, ones) ) ) %>%
    # two.digit == "S" & nchar(NTEE.CC) > 3 ~  paste0(letter, substr(NTEE.CC, 4, 5)),
    # two.digit == "S" & nchar(NTEE.CC) == 3 ~ paste0(letter, two.digit ))) %>%
    mutate(s.further = ifelse(type.org == "S" & nchar(NTEE.CC) > 3,paste0(letter, substr(NTEE.CC, 4, 5)), NA ))%>%
    mutate(s.further.tens = ifelse(type.org == "S" & nchar(NTEE.CC) > 3,paste0(letter, substr(NTEE.CC, 4, 4)), NA ))
  
  
  
  geo.weights <- weights$geo
  r.mission.weights <- weights$r.mission.weight
  s.mission.weights <- weights$s.mission.weight
  
  # unnormalized 
  # geo.weights$weight <- geo.weights$weight / sum(geo.weights$weight )
  # r.mission.weights$weight <- r.mission.weights$weight / sum(r.mission.weights$weight )
  # s.mission.weights$weight <- s.mission.weights$weight / sum(s.mission.weights$weight )
  
  if(is.na(org.mission$s.further.tens )){
    s.further.tens.needed <- FALSE
  }else{
    s.further.tens.needed <- TRUE
    }
    
  #needed constat
  r.log.expense <- max(dat.filtered$log.expense) -  min(dat.filtered$log.expense) #range

  #for loop
  for(i in 1:n){
    
    #first do numerical comparisons
    ## distance for log.expense
    if(is.na(dat.filtered$TotalExpense[i])){
      D[i, "logTotalExpense"] <- 1
    }else{
      D[i, "logTotalExpense"] <- log(abs(dat.filtered$TotalExpense[i] - org$TotalExpense + 1), base = 10)  # / r.log.expense
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
    
    
    D[i, "Geography"] <-  log(g1 + g2 + g3 + 1, base=10)
    
    
    ## Mission Distance 
    temp.org <- mission_info[mission_info$EIN ==  dat.filtered$EIN[i], ] 
    if(org.mission$type.org != temp.org$type.org){
      D[i, "Mission"] <- 1 #max distance 
    }else if(org.mission$type.org == "R"){
      cols <- c("type.org", "letter", "letter1", "letter2")
      D[i, "Mission"] <- log(sum( r.mission.weights$weight * (temp.org[, cols] != org.mission[, cols])), base = 10)
    }else if(org.mission$type.org == "S"){
      temp.comp <- data.frame(
        two.digit = ifelse(as.numeric(temp.org$two.digit)>10, 10, as.numeric(temp.org$two.digit)),
        broad.category = temp.org$broad.category,
        letter = temp.org$letter, 
        s.further = ifelse(is.na(temp.org$s.further), "None.Temp", temp.org$s.further ),
        s.further.tens = ifelse(is.na(temp.org$s.further), "None.Temp", temp.org$s.further.tens ))
      org.comp <- data.frame(
        two.digit = ifelse(as.numeric(org.mission$two.digit)>10, 10, as.numeric(org.mission$two.digit)),
        broad.category = org.mission$broad.category,
        letter = org.mission$letter,
        s.further = ifelse(is.na(org.mission$s.further), "None.Org", org.mission$s.further ),
        s.further.tens = ifelse(is.na(org.mission$s.further), "None.Org", org.mission$s.further.tens ))      
      
      if(s.further.tens.needed){
         to.sum <- s.mission.weights$weight * (temp.comp != org.comp )
      }else{
        norm.constant <- sum(s.mission.weights$weight[1:4])
        to.sum <- (s.mission.weights$weight/norm.constant * (temp.comp != org.comp ))[-5] 
      }
      D[i, "Mission"] <- log(sum( s.mission.weights$weight * (temp.comp != org.comp )) + 1, base = 10)
      
                
    }
    
    
    
  }
  
  # Get distance
  # this number will be between 0 and 1
  # closer to 0 = filtered org is closer to input org, closer to 1 = filtered or is farther away from input org
  dat.filtered$dist <- rowSums(D) / A #normalize by number of matched attributes.
  colnames(D) <-  c("dist.log.expense", "dist.geo", "dist.mission")
  
  #get normalized weights 
  D.norm <- D
  colnames(D.norm) <-  c("dist.log.expense.norm", "dist.geo.norm", "dist.mission.norm")
  for(i in 1:A){
    mi <- min(D[, i])
    ma <- max(D[, i])
    for(j in 1:n){
      D.norm[j, i] <- (D[j, i] - mi) / (ma - mi)
    }
  }
  
  dat.filtered$dist.norm <- rowSums(D.norm) / A 
  

  dat.ret <- dat.filtered %>%
    dplyr::bind_cols(D, D.norm)  %>%
    dplyr::mutate(row.ID = row_number()) %>%
    dplyr::arrange(dist) %>%
    dplyr::select(-c(log.expense))%>%
    #slice(1:100) %>%
    dplyr::mutate(Rank = row_number()) %>%
    dplyr::relocate(Rank)%>%
    dplyr::arrange(row.ID) %>%
    dplyr::select(-row.ID) 

  
  ret <- list(dat = dat.ret,
              distance.matrix = D,
              distance.matrix.norm = D.norm)
  
  return(ret)
}

##### Testing ----------------------------------------------------------

org <- orgs.list[[7]]
search <- search.list[[7]]
hard <- hard.list[[7]]
dat.filtered <- dat_filtering_hard(search, hard, 410993136)

weights <- list(geo.weight = data.frame(level = c(1, 2, 3),
                                        weight = c(6, 6, 1) / 24 ), #max(state_dist) = 12)
                r.mission.weights = data.frame(level = c(1, 2, 3, 4),
                                               weight = c(5, 2, 1, 0.5) / 8.50),
                s.mission.weights = data.frame(level = c(1, 2, 3, 4, 5),
                                                weight = c(5, 2, 1, 0.5, 0.25) / 8.75))

# # # ### Do the distances 
# # state_dist <- read_csv("data-wrangle/state-distance-matrix.csv")
# # mission_info <- read_csv("data-wrangle/mission-info.csv")
# # 
# test1 <- HEOM_with_weights(org, search, dat.filtered, weights)
# test1$dat$dist
# # 
# # ggplot(test1$dat, aes(x = dist)) + geom_density()
# # 
# # test1$distance.matrix %>%
# #   mutate(rownum = row_number()) %>%
# #   pivot_longer(!rownum, names_to = "attribute", values_to = "vals") %>%
# #   ggplot(aes(x = vals))+
# #   geom_histogram()+
# #   facet_wrap( ~ attribute)
# # 
# # 
# # ### If any are greater than 1 or NA?
# # test1$dat$EIN[which(test1$dat$dist > 1) ]
# # test1$dat$EIN[which(is.na(test1$dat$dist ))]
# # 
