generate_comparisons <- function(geo.weighs, r.mission.weights, s.mission.weights, folder.name){
  
  #If weight lengths dont match return error---------------------------------------
  if(length(unique(nrow(geo.weights), nrow(r.mission.weights), nrow(s.mission.weights))) > 1){
    return("ERROR: Number of rows in `geo.weights`, `r.mission.weights` and `s.mission.weights` do not match.")
  }
  
  # Create directory to save in ----------------------------------------
  path <- paste0("unit-testing/test-results/", folder.name)
  if(file.exists(path)){
    return("ERROR: Directory already exits, choose a diffent folder name.")
  }else{
    dir.create(path)
  }
  
  #load needed library/functions/datasets -----------------------------------------
  library(tidyverse)
  
  source("funcs/applying-filters-func.R")
  source("funcs-testing/dat-filtering-hard-unit-testing-v03.R") #removes test org EIN from comparison set
  source("funcs-testing/distance-metric-v03.R")
  source("funcs/dollarize.R")
  
  
  dat.testing <- read_csv("unit-testing/test-orgs.csv")
  state_dist <- read_csv("data-wrangle/state-distance-matrix.csv")
  mission_info <- read_csv("data-wrangle/mission-info-v03.csv")
  
  #load the test orgs and search criteria ----------------------------------------
  orgs.list <- list()
  for(i in 1:nrow(dat.testing)){
    temp.dat <- dat.testing[i, ]
    orgs.list[[i]] <- list(State = temp.dat$State, 
                           Loc = temp.dat$LocationType,
                           MajorGroup = temp.dat$MajorGroup,
                           NTEE = temp.dat$NTEE, 
                           NTEE.CC = temp.dat$NTEE.CC,
                           UNIV = temp.dat$UNIV,
                           HOSP = temp.dat$HOSP,
                           TotalExpense = temp.dat$TotalExpense,
                           TotalEmployee = temp.dat$TotalEmployee,
                           CEOCompensation = temp.dat$CEOCompensation) }
  
  search.list <- list()
  for(i in 1:nrow(dat.testing)){
    search.list[[i]] <- list(State = orgs.list[[i]]$State,
                             MajorGroup = orgs.list[[i]]$MajorGroup,
                             Loc = orgs.list[[i]]$Loc,
                             NTEE = orgs.list[[i]]$NTEE,
                             NTEE.CC = NA, 
                             UNIV = 1,
                             HOSP = 1,
                             TotalExpense = orgs.list[[i]]$TotalExpense* c(0.5 , 1.5),
                             TotalEmployee = NA)}
  
  hard.list <- list()
  for(i in 1:nrow(dat.testing)){
    hard.list[[i]] <- list(State = FALSE,
                           Loc = FALSE,
                           MajorGroup = FALSE,
                           NTEE = FALSE, 
                           NTEE.CC = FALSE,
                           TotalExpense = FALSE,
                           TotalEmployee = FALSE) }
  
  
  
  # Generate each data storage in 2 for loops ----------------------------------
  dat.storage <-  vector(mode = "list", length = nrow(dat.testing))
  for(n in 1:nrow(dat.testing)){
    dat.storage[[n]] <- vector(mode = "list", length = nrow(geo.weights))
  }
  
  for(i in 1:nrow(dat.testing)){
    #running the method
    dat.filtered <- dat_filtering_hard(search.list[[i]], hard.list[[i]], dat.testing$EIN[i])
    for(j in 1:nrow(geo.weights)){
      weights.temp <- list(geo.weights = data.frame(level = c(1,2,3),
                                                    weight = unlist(geo.weights[j, ])), 
                           r.mission.weights = data.frame(level = c(1,2,3,4),
                                                          weight = unlist(r.mission.weights[j, ])), 
                           s.mission.weights = data.frame(level = c(1,2,3,4,5),
                                                          weight = unlist(s.mission.weights[j, ])))
      
      
      out.temp <-  HEOM_with_weights(orgs.list[[i]], search.list[[i]], dat.filtered, weights.temp)
      
      #formatting for storage
      colnames(out.temp[[2]]) <- c("dist.log.expense", "dist.geo", "dist.mission")
      dat.storage[[i]][[j]] <- out.temp[[1]] %>% 
        mutate(Weights.Set = j) %>% 
        mutate(Test.Org = dat.testing$EIN[i]) %>%
        bind_cols(as.data.frame(out.temp[[2]]))           
    } 
  }
  
  
  # Merge data storage into one dataset ------------------------
  dat.ret <- data.frame()
  for(i in 1:nrow(dat.testing)){
    for(j in 1:nrow(geo.weights)){
      dat.ret <- bind_rows(dat.ret, dat.storage[[i]][[j]])
    }
  }
  
  
  # Save results  -------------------------------------------
  saveRDS(dat.ret, file = paste0(path, "/full-data.rds"))
  
  # Save inital parameters --------------------------------------
  #test orgs
  saveRDS(dat.testing, file = paste0(path, "/test-orgs.rds"))
  
  #weights
  dat.weights <-  cbind(geo.weights, r.mission.weights, s.mission.weights) 
  colnames(dat.weights) <- 
    c( paste0("geo.", colnames(geo.weights)),
       paste0("r.mission.", colnames(r.mission.weights)),
       paste0("s.mission.", colnames(s.mission.weights))
    )
  dat.weights <- dat.weights %>%
    mutate(Weight.Set = row_number())%>%
    relocate(Weight.Set)
  
  saveRDS(dat.weights, file =  paste0(path, "/weights.rds"))
  
  
  # Make and save plots 
  rmarkdown::render( input='unit-testing/make-comparison-plots-v03.Rmd', 
                     output_file = paste0("test-results/", folder.name, "/graphs"),
                     params = list( dat = dat.ret,
                                    weights = dat.weights,
                                    test.orgs = dat.testing))
}





