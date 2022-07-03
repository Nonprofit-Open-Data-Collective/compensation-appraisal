###############################
### Using HEOM metric on our data 
################################

library(dplyr)

dat <- read_csv("data-wrangle/data-by-sector.csv")




### High weighted match on matching on MajorGroup, UNIV, HOSP
### Low weighted match on NTEE, NTEE.CC, STATE
### Euclidian distance on log(Total Expense) and Total Employee

#only 990 data and transform to log (haven't imputed 990EZ data yet)
dat.990 <- dat %>%
  filter(FormType == "990") %>%
  filter(TotalExpense > 0) %>%
  mutate(logTotalExpense = log(TotalExpense+1, 10)) %>%
  mutate(row.num = row_number()) #%>%
  #filter(MajorGroup %in% c(4,5)) #my memory runs out if we try to do all of them

#data to match on 
dat.func.input <- dat.990 %>%
  select(-c(FormYr, TotalExpense, CEOCompensation, Gender, TransitionYear, FormType))

dat.type <- c("categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "numeric", "numeric")

#use first row as test org 
org <- dat.func.input[1, ]
dat.func.input <- dat.func.input[ -1, ]



###############################
### The function 
##############################


func.result <- HEOM_no_weights(org, dat.func.input, dat.type )

HEOM_no_weights <- function(org, dat, dat.type){
  
  ### Inputs 
  # org = the organization characteristics
  # dat = the data frame to compare org to 
  #   A = # of attributes to compare on
  #   n = # of other orgs to compare to
  #   dim(dat) = n - by - (A+1)
  #   last column in dat is row number 
  # dat.type = vector of length A, "numeric" and "categorical" entires for each type that the column in dat is
  
  ### Output 
  # distances of the rows of dat
  
  
  #needed values
  A <- dim(dat)[2]  - 1 #number of attributes to comapare (last column is row number)
  n <- dim(dat)[1] #number of points in the data set
  
  #initialize storage
  D <- matrix(0, nrow = n, ncol = A) 
  
  # Get the distances
  for(a in 1:A){
    #get the data column we care about 
    dat.a <- dplyr::pull(dat, a)
    org.a <- dplyr::pull(org, a)
    type <- dat.type[a]
    #get range if numberic 
    if(type == "numeric"){ r <- max(dat.a) - min(dat.a) }
    
    #fill in that column of the distance matrix 
    for(i in 1:n){
      #if its type is numeric
      if(type == "numeric"){
        D[i, a] <- abs(dat.a[i] - org.a) / r
      }else if( type == "categorical"){
        #D[i, j, a] <- as.numeric(dat.a[i] != dat.a[j]) # 0 if they match, 1 if they do not # want them to be far away if they do not match 
        D[i, a] <- ifelse(dat.a[i] != org.a, 1 ,0) # 0 if they match, 1 if they do not # want them to be far away if they do not match 
      }#end if
      
    }#end for i
    
  } #end for a
  
  
  dist <- rowSums(D)
  # 
  # ret <- cbind(dat, dist) #%>%
  #   #arrange(dist) #%>% 
  #   # select(row.num)
    
  
  return(dist)
}


##########################
### Using the function result
##########################

dat.990.post <- cbind(dat.990[-1, ], func.result)

#compare top 10 closest to org
dat.990.post %>% 
  arrange(func.result) %>%
  head(10)

c(dat.990[1, ])


## It works!! 
## now to make this work with weights 

################################
### Function with variable weights
#   make this in the format we need for the shiny app 
################################

func.result <- HEOM_with_weights(org, dat.func.input, dat.type )

HEOM_with_weights <- function(org, dat.filtered){
  
  ### Inputs 
  # org = the organization characteristics
  # dat.filtered = the data frame to compare org to 
  #   A = # of attributes to compare on
  #   n = # of other orgs to compare to
  #   dim(dat) = n - by - A

  ### Output 
  # list of top 10 closest orginizaitions
  
  ### High weighted match on matching on MajorGroup, UNIV, HOSP
  ### Low weighted match on NTEE, NTEE.CC, STATE
  ### Euclidian distance on log(Total Expense) and Total Employee
  
  #get rid of orgs with negative expenses 
  dat.filtered <- dat.filtered%>%
                  mutate(dist = NA) %>%
                  filter(TotalExpense>0)%>% #might need to handle this differently ask Jesse
                  mutate(log.expense = log(TotalExpense, base = 10))
    
  
  #needed values
  A <- 8 #number of attributes to compare 
  n <- dim(dat.filtered)[1] #number of points in the data set
  
  #initialize storage
  D <- as.data.frame(matrix(0, nrow = n, ncol = A) )
  colnames(D) <- c("MajorGroup", "UNIV", "HOSP", "NTEE", "NTEE.CC", "State", "logTotalExpense", "Totalmployee")
  
  ## distance for Major Group
  for(i in 1:n){
    D[i, "MajorGroup"] <- ifelse(dat.filtered$MajorGroup[i] != org$MajorGroup, 1 ,0) 
  }
  
  ## distance for UNIV
  for(i in 1:n){
    D[i, "UNIV"] <- ifelse(dat.filtered$UNIV[i] != org$UNIV, 1 ,0) 
  }
  
  ## distance for HOSP
  for(i in 1:n){
    D[i, "HOSP"] <- ifelse(dat.filtered$HOSP[i] != org$HOSP, 1 ,0) 
  }
  
  ## distance for NTEE
  for(i in 1:n){
    D[i, "NTEE"] <- ifelse(dat.filtered$NTEE[i] != org$NTEE, 0.5 ,0) 
  }
  
  ## distance for NTEE.CC
  for(i in 1:n){
    D[i, "NTEE.CC"] <- ifelse(dat.filtered$NTEE.CC[i] != org$NTEE.CC, 0.5 ,0) 
  } 
  
  ## distance for STATE
  for(i in 1:n){
    D[i, "STATE"] <- ifelse(dat.filtered$State[i] != org$State, 0.5 ,0) 
  }
  
  ## distance for log.expense
  r.log.expense <- max(dat.filtered$log.expense) -  min(dat.filtered$log.expense)
  for(i in 1:n){
    D[i, "logTotalExpense"] <- abs(dat.filtered$log.expense[i] - log(org$TotalExpense, 10)) / r.log.expense
  }
  
  ## distance for total employee
  r.employee <- max(dat.filtered$TotalEmployee) -  min(dat.filtered$TotalEmployee)
  for(i in 1:n){
    D[i, "TotalEmployee"] <- abs(dat.filtered$TotalEmployee[i] - org$TotalEmployee) / r.employee
  }
  
  
  dat.filtered$dist <- rowSums(D)
  
  dat.ret <- dat.filtered %>%
    arrange(dist) %>%
    select(-c(Gender, TransitionYear, FormType, dist, log.expense))%>%
    slice(1:10)

  return(dat.ret)
}

## Testing 
source("funcs/applying-filters-func.R")

org <- list(FormYr = 2019,
            State = "CA",
            MajorGroup = 3,
            NTEE = "G", #need to add input for this
            NTEE.CC = "G9B", #need to add input for this
            UNIV = FALSE,
            HOSP = FALSE,
            TotalExpense = 300000,
            TotalEmployee = 10,
            FormType = "990"
)

search <- list(form.year = c(2018,2019),
               state = c("CA", "TX", "WA", "OR", "NV"),
               major.group = c(3, 4),
               ntee = NA, #need to add input for this
               ntee.cc = NA, #need to add input for this
               hosp = NA,
               univ = NA,
               tot.expense = c(-Inf, Inf),
               tot.employee = c(0, Inf),
               form.type = "990")


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

test.func.output <- HEOM_with_weights(org, dat.filtered)
test.func.output
unlist(org)
