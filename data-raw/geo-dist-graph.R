## Creating State Centers graphic 

## Needed libraries -------------------------------
library(dplyr)
library(compensator)
library(igraph)


## Makings the data --------------------------------
full.dist.mat <- state.dist.mat * 12

state.centers <- data.frame(name = datasets::state.name, 
                            abb = datasets::state.abb)

state.centers[51, ] <- c("District of Columbia", "DC")
state.centers[52, ] <- c("Puerto Rico", "PR")

state.centers <- state.centers %>%
  dplyr::arrange(name) 

state.centers$x <- c(14, 1 ,5 ,11, 3, 7 , 21, 20, 18, 16, #ends with flordia 
                     16, 1, 3, 11, 13, 9, 9, 12, 10, 23, #end with Maine
                     16, 20, 14, 8, 12, 10, 4, 8, 4, 21, #end with new hampshire 
                     19, 6, 18, 17, 6, 15, 8, 2, 17, 18, #ends with pureto rico,
                     22, 18, 7, 13, 7, 6, 19, 15, 2, 14, 10, 5) 

state.centers$y <- c(3, 8, 4, 4, 4, 4, 6, 5, 5, 2, #FL
                    3, 2, 6, 6, 6, 6, 4, 5, 3, 8, #ME
                    5, 7, 7, 7, 3, 5, 7, 5, 5, 8, #NH
                    6, 3, 7, 4, 7, 6, 3, 5, 6, 2, #PR
                    7, 3, 6, 4, 2, 5, 8, 4, 7, 5, 7, 6)



## Creating igraph object ---------------------------

# Getting Adjacency Matrix 
adj.mat <- full.dist.mat == 1
state.igraph <- igraph::graph_from_adjacency_matrix(adj.mat, mode = "undirected")

# Adding locations 
lonlat <- 2*as.matrix(state.centers[ , 3:4], ncol =2)

## Plot -----------------------------------------
plot(state.igraph, 
     layout = lonlat,
     vertex.size = 15,
     vertex.label.size = 3,
     vertex.color = "black",
     vertex.label.color = "white")

