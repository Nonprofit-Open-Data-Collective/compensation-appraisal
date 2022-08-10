######################################
### Function to find list of similar orginizations using the distane metric
######################################


find_comparisons <- function(org , search){
  
  ### Inputs 
  # org = list of orgnization characteristics, attributes must have same column name values as dat-wrangle/data-by-sector.csv
  # seach = list of criteria to search on, attributes must have same names as inputs in dat_filtering()
  
  ### Outputs 
  # a list of the top 10 organizations in your search criteria that are closest to you.
  
  #make sure search$major.group is roman numerals 
  search$major.group <- as.roman(search$major.group)
  
  
  ### First filter the data 
  dat.filtered <- dat_filtering(form.year = search$form.year,
                                state = search$state,
                                major.group = search$major.group,
                                ntee = search$ntee,
                                ntee.cc = search$ntee.cc,
                                hosp = search$hosp,
                                univ = search$univ,
                                tot.expense = search$tot.expense,
                                tot.employee = search$tot.employee,
                                form.type = search$form.type
                                )


  ### prep data for for loop
  dat.filtered <- dat.filtered%>%
    mutate(dist = NA) %>%
    filter(TotalExpense>0)%>% #might need to handle this differently ask Jesse
    mutate(log.expense = log(TotalExpense, base = 10)) %>%
    filter(!is.na(TotalEmployee))#might need to handle this differently ask Jesse


  org.dist <- data.frame(org) %>%
    select( TotalExpense, TotalEmployee) %>%
    mutate(log.expense = log(TotalExpense, base = 10)) %>%
    select(-TotalExpense)

  ##### PROBLEM IS IN THIS FOR LOOP###########
  ### Find distance measures in for loop
  for(i in 1:dim(dat.filtered)[1] ){
    compare <- rbind(org.dist, dat.filtered[i, c("TotalEmployee", "log.expense")])
    dat.filtered$dist[i] <- philentropy::distance(compare, method = "euclidean")
  }

  ### list the top 10 most similar
  #Get rid of the NA's - they are EZ filers - will have to figure out how to deal with that later
  dat.result <- dat.filtered %>%
    filter(!is.na(dist)) %>%
    arrange(dist ) %>%
    slice(1:10) %>%
    select(FormYr, FormType, State, MajorGroup, NTEE, NTEE.CC, UNIV, HOSP, TotalExpense, TotalEmployee, CEOCompensation)

   return(dat.result)

}


### Testing

search <- list(form.year = 2019,
               state = c("GA", "SC", "AL", "NC", "TN", "MS"),
               major.group = 3,
               ntee = NA, #need to add input for this
               ntee.cc = NA, #need to add input for this
               hosp = NA,
               univ = NA,
               tot.expense = c(-Inf, Inf),
               tot.employee = c(0, Inf),
               form.type = "990")


org <- list(FormYr = 2019,
            State = "GA",
            MajorGroup = 2,
            NTEE = "B", #need to add input for this
            NTEE.CC = "B01", #need to add input for this
            UNIV = FALSE,
            HOSP = FALSE,
            TotalExpense = 300000,
            TotalEmployee = 4,
            FormType = "990"
)


# find_comparisons(org, search)

