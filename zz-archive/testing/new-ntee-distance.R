library(tidyverse)

weights <- data.frame(level = c(1, 2, 3, 4, 5), 
                      weight = c(5, 2, 1, 0.5, 0.25))

set.seed(100)
dat.test <- read.csv("data-rodeo/dat-shinyapp.csv") %>%
  sample_n(1000)

#Example Test Org 
test.org <- "B21"
levels.org <- c("R", "2", "B", "B2", "B21")

#Getting the level codes for each org in comparison set
levels.compare <- as.data.frame(matrix(NA, ncol = 5, nrow = 1000))
colnames(levels.compare) <- c("l1", "l2", "l3", "l4", "l5")
for(i in 1:1000){
  ntee.code <- dat.test$NTEE.CC[i]
  ntee.sperate <- strsplit(ntee.code, "")[[1]]
  
  #Level 1: Regular Vs Specality 
  if(ntee.sperate[2] < 2){
    levels.compare$l1[i] <- "S"
  }else{
    levels.compare$l1[i] <- "R"
  }
  
  #Level 2 
  levels.compare$l2[i] <- dat.test$MajorGroup[i]
  
  ###test if hospital or university
  if(dat.test$HOSP[i]){
    levels.compare$l2[i] <- 12
  }
  if(dat.test$UNIV[i]){
    levels.compare$l2[i] <- 11
  }
  
  #Level 3
  levels.compare$l3[i] <- ntee.sperate[1]
  
  #Level 4
  levels.compare$l4[i] <- paste0(ntee.sperate[1], ntee.sperate[2])
  
  #Level 5
  levels.compare$l5[i] <- paste0(ntee.sperate[1], ntee.sperate[2], ntee.sperate[3])
  
}


#Calculating Distances for all 5 critera

dist <- rep(NA, 1000)
for(i in 1:1000){ #sum-product 
  match <- levels.org == levels.compare[i, ]
  dist.each <- as.numeric(!match) * weights$weight 
  dist[i] <- sum(dist.each)
}

#inspecting results 
closest.rows <- which(dist == min(dist))
closest.orgs <- dat.test[closest.rows, ]

table(closest.orgs$NTEE.CC)
test.org

dist[closest.rows]


# Calculating distances stopping at level 3

dist3 <- rep(NA, 1000)
for(i in 1:1000){ #sum-product 
  match <- levels.org == levels.compare[i, ]
  dist.each <- as.numeric(!match) * weights$weight
  dist3[i] <- sum(dist.each[1:3])
}

#inspecting results 
closest.rows3 <- which(dist3 == min(dist3))
closest.orgs3 <- dat.test[closest.rows3, ]

table(closest.orgs3$NTEE.CC)
test.org

dist3[closest.rows3]

