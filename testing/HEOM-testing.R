################################
### Heterogeneous Euclidean-Overlap Metric 
################################

library(dplyr)

#### Make a fake data st 

#data frame
dat.fake <- data.frame(A = rnorm(10),
                       B = rpois(10, lambda = 3),
                       C = sample(letters[1:3], 10, replace = T),
                       D = sample(state.abb[1:2], 10, replace = T))

#vector of classes that each column is 
dat.col.indicator <- c("numeric", "numeric", "character", "character")
apply(dat.fake, 2, range)

#### The Function 

HEOM <- function(dat, dat.type){
  
  #dat <- dat.fake
  #dat.type <-  c("numeric", "numeric", "character", "character")

  
  #needed values
  A <- dim(dat)[2] #number of attributes to comapare
  n <- dim(dat)[1] #number of points in the data set 

  #initialize storage
  D <- array(0, c(n,n,A)) #first 2 dim ension are the corrlation matrix for the attribute number of the 3rd dimension

  #make a 3d cuble 
  #only need to search through upper triangluar that is not on the diagonal then can transpose 
  for(a in 1:A){
    
    #get the data column we care about 
    dat.a <- dat[, a]
    type <- dat.type[a]
    #get range if numberic 
    if(type == "numeric"){ r <- max(dat.a) - min(dat.a) }
   
    
    # fill in the distance matrix
    # only need to fill in the upper triangular thats not the diagonal then we can transpose it
    for(i in 1:(n-1)){
      for(j in (i+1):n){
        #if its type is numeric
        if(type == "numeric"){
          D[i, j, a] <- abs(dat.a[i] - dat.a[j]) / r
        }else if( type == "character"){
          #D[i, j, a] <- as.numeric(dat.a[i] != dat.a[j]) # 0 if they match, 1 if they do not # want them to be far away if they do not match 
          D[i, j, a] <- ifelse(dat.a[i] != dat.a[j], 1 ,0) # 0 if they match, 1 if they do not # want them to be far away if they do not match 
        }
      }
    }
    
    #make it a true distance matrix 
    #dont need this to search for smallest ones 
    #D[ , , a] <- D[ , , a] + t(D[ , , a])
    
  }

  D <- apply(D,c(1,2),sum)
  
  return(D)

}

test <- HEOM(dat.fake, dat.col.indicator)

#only search in the upper triangular for smallest ones 

test.frame <- as.data.frame(as.table(test))  
colnames(test.frame) <- c("rows", "columns", "values")

#recode levels
n <- dim(dat.fake)[1]
levels(test.frame$rows) <- 1:n
levels(test.frame$columns) <- 1:n

#make numeric 
test.frame$rows <- as.numeric(test.frame$rows)
test.frame$columns <- as.numeric(test.frame$columns)

#make keep indicator 
#only keep upper triangular that is not on the diagonal 
test.frame$keep <- FALSE
test.frame %>%
  mutate(keep = columns > rows) %>%
  filter( keep == T) %>%
  select(- keep) %>%
  arrange(values) %>% 
  rename("Point1" = rows, "Point2" = columns)

#lets see if this is true 
dat.fake[c(4,9) , ]


### This method compare all points to all other points, 
#We dont need to compare every point to every other point
#We only need to compare one point to every other point
#so we dont need to loop over i 

#we can also change some of the weights 
# like we want to do hard match on major group
# but instead of doing 0 = matching and 1 = no match we could do 
# for ntee 0 = match, 0.5 = no match
# for ntee.cc 0=match, 0.25 = no match 
# becuase its less important to match on ntee and ntee.cc but when it does its beneficial










