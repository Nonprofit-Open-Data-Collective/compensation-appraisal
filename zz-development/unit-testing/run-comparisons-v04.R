### Generate Comparison set 

## Name the folder you want to save the final data set in
folder.name <- "v04-test3"

## Setting up the weights ------------------------------------------------
#Create 3 data frames geo.weights, r.weights, and s.weights
#each row is a new combination weights you want to run 


## initialize - don't change this, just run it ---------------------------------
geo.weights <- 
  data.frame(level1 = numeric(),    
             level2 = numeric(),
             level3 = numeric())

r.mission.weights <- 
  data.frame(level1 = numeric(),    
             level2 = numeric(),
             level3 = numeric(),
             level4 = numeric())

s.mission.weights <- 
  data.frame(level1 = numeric(),    
             level2 = numeric(),
             level3 = numeric(),
             level4 = numeric(),
             level5 = numeric())


## Enter in the combination of weights you want to use as rows-----------------------
#set 1
weight.set.ID <- 1
geo.weights[weight.set.ID, ] <- c(1,1,1)
r.mission.weights[weight.set.ID, ] <- c(1,1,1,1)
s.mission.weights[weight.set.ID, ] <- c(1,1,1,1,1)

#set 2 
weight.set.ID <- 2
geo.weights[weight.set.ID, ] <- c(2,2,1)
r.mission.weights[weight.set.ID, ] <- c(3,2,1,1)
s.mission.weights[weight.set.ID, ] <- c(3,2,1,1,0.5)

#set 3
weight.set.ID <- 3
geo.weights[weight.set.ID, ] <- c(5,2,1)
r.mission.weights[weight.set.ID, ] <- c(5,2,1,1)
s.mission.weights[weight.set.ID, ] <- c(5,2,1,1,0.5)


## Run the Scripts-- don't change anything below this line, just run it -----------
source("funcs-testing/generate-comparison-dataset-v04.R")
generate_comparisons(geo.weighs, r.mission.weights, s.mission.weights, folder.name)


## Clean Up 
#clean up plots 
directories <- paste0(getwd(), "/unit-testing")

plots <- list.files(directories, pattern="*.jpg")
plots <- paste0( directories, "/", plots)
unlink(plots)
