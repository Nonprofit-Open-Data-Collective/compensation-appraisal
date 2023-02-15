library(tidyverse)

## NEed region data 
region <- data.table::data.table(state = sort(c(state.abb, "PR", "DC")),
                                 region = c(9, 6, 7, 8, 9, 8, 1, 5, 1, 5, 
                                            5, 9, 5, 8, 3, 3, 4, 6, 7, 1, 
                                            1, 1, 6, 4, 4, 6, 8, 5, 4, 4, 
                                            1, 1, 8, 8, 1, 3, 7, 9, 1, 5, 
                                            1, 5, 4, 6, 7, 8, 5, 1, 8, 3, 
                                            5, 8))

### Get Testing Dat
set.seed(100)
dat.test <- read.csv("data-rodeo/dat-shinyapp.csv") %>%
  filter(!is.na(State) & !is.na(LocationType))%>%
  sample_n(1000)

weights <- data.frame(level = c(1, 2, 3, 4, 5), 
                      weight = c(5, 2, 1, 0.5, 0.25))
## Levels
#1 = US State or territory 
#2 = Rural/Urban
#3 = Census Region
#4 = State
#5 = Zip


#Example Test Org 
test.org <- dat.test[1, ]
levels.org <- c("S",
                test.org$LocationType, 
                region$region[which(region$state == test.org$State)],
                test.org$State, 
                test.org$ZIP5)

#### Getting Metrics for Comparison set 

levels.compare <- as.data.frame(matrix(NA, ncol = 5, nrow = 1000))
colnames(levels.compare) <- c("StateTerr", "RuralUrban", "Region", "State", "Zip")
for(i in 1:1000){
  levels.compare[i, 1] <- ifelse(dat.test$State[i] == "PR", "T", "S")
  levels.compare[i, 2] <- dat.test$LocationType[i]
  levels.compare[i, 3] <- region$region[which(region$state == dat.test$State[i])]
  levels.compare[i, 4] <- dat.test$State[i]
  levels.compare[i, 5] <- as.character(dat.test$ZIP5[i])
    
}




### Fixing 4 digit issue 
for(i in 1:1000){
  split <- strsplit(levels.compare$Zip[i], split = "")[[1]]
  if(length(split) < 5){ 
    levels.compare$Zip[i] <- paste0("0", levels.compare$Zip[i])
      }
}


#### Getting Distances 
dist <- rep(NA, 1000)
for(i in 1:1000){ #sum-product 
  match <- levels.org == levels.compare[i, ]
  dist.each <- as.numeric(!match) * weights$weight 
  dist[i] <- sum(dist.each)
}


# Inspecting Results
closest.rows <- which(dist == min(dist))
closest.orgs <- dat.test[closest.rows, ]

table(closest.orgs$NTEE.CC)
test.org

dist[closest.rows]

