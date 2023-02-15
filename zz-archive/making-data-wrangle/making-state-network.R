library(tidyverse)
library(rvest)
library(igraph)

### Get data and standardize -----------------------------------

state_list <- 
  "https://thefactfile.org/u-s-states-and-their-border-states/" %>%
  read_html() %>%
  html_nodes(css = "table") %>%
  html_table(fill = TRUE)

state_list <- state_list[[1]]
state_list$`Serial number` <- as.character(state_list$`Serial number`)

#change Alaksa
state_list[2, 3:4] <- matrix(c("Washington", "1"), nrow = 1)
#change Hawaii
state_list[11, 3:4] <- matrix(c("California", "1"), nrow = 1)
#Add PR
state_list[51, ] <- matrix(c("51", "Puerto Rico", "Florida", "1"), nrow = 1)
#Add DC
state_list[52, ] <- matrix(c("52", "District of Columbia", "Virginia, Maryland", "2"), nrow = 1)

#Standardize State Name
state_list$`State name`[19] <- "Maine"
state_list$`State name`[25] <- "Missouri"
state_list$`State name`[42] <- "Tennessee"


### Standardize table

state_list <- 
  state_list %>%
  arrange(`State name`) %>%
  mutate(order = row_number()) %>%
  rename(state = `State name`,
         border = `Bordering State`, 
         num.border = `Number of bordering states`) %>%
  #add census regions if we want them later
  mutate(region  = c(6, 9, 8, 7, 9, 8, 1, 1, 5, 5, 
                     5, 9, 8, 3, 3, 5, 4, 6, 7, 1,  #start with georgia ends with maine
                     1, 1, 6, 4, 6, 4, 8, 4, 8, 1,   #starts with maryland ends with new hampshire
                     1, 8, 1, 5, 4, 3, 7, 9, 1, 5,  #starts with new jersy
                     1, 5, 4, 6, 7, 8, 1, 5, 8, 5, 
                     3, 8  )) %>%
  mutate(abb = c(state.abb[1:8], "DC", state.abb[9:38], "PR", state.abb[39:50])) %>%
  select(-1) 
  


### Make graph distances  ---------------------------------------------

AdjMat <- matrix(0, ncol = 52, nrow = 52)
for(i in 1:52){
  border <- strsplit(state_list$border[i], split = ", ")[[1]]
  AdjMat[i, ] <- as.numeric(state_list$state %in% border)
}

g <- graph_from_adjacency_matrix(AdjMat, mode = "undirected" )

state_dist <- as.data.frame(distances(g)) / max(distances(g))
colnames(state_dist) <- rownames(state_dist) <- state_list$abb

### Save Tables -----------------------------------
write_csv(state_dist, "data-wrangle/state-distance-matrix.csv")
