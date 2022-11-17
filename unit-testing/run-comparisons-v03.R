### Generate Comparison set 

## Name the folder you want to save the final data set in
folder.name <- "v03-test4"

## Setting up the weights ------------------------------------------------
#Create 3 data frames geo.weights, r.weights, and s.weights
#each row is a new combination weights you want to run 


## initialize - don't touch this ---------------------------------
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
geo.weights[1, ] <- c(1,1,1)
r.mission.weights[1, ] <- c(1,1,1,1)
s.mission.weights[1, ] <- c(1,1,1,1,1)

#set 2 
geo.weights[2, ] <- c(2,2,1)
r.mission.weights[2, ] <- c(3,2,1,1)
s.mission.weights[2, ] <- c(3,2,1,1,0.5)


## Run the function-- don't change anything below this line, just run it -----------
source("funcs-testing/generate-comparison-dataset-v03.R")
generate_comparisons(geo.weighs, r.mission.weights, s.mission.weights, folder.name)

