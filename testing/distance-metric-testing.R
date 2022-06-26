### Read in Data
dat <- read_csv("data-wrangle/data-by-sector.csv")

### Load dat_filtering function 
source("testing/applying-filters-func.R")


#make first org in I the test case 

dat.test <- dat.I%>%
  filter(FormYr > 2017) %>%
  filter(TotalExpense>0)%>%
  mutate(log.expense = log(TotalExpense+1, base = 10))

org <- dat.test[ 1, ]
dat.test <- dat.test[-1, ]

dist <- rep(NA, dim(dat.test)[1])

dat.test.dist <- dat.test[, c("log.expense", "TotalEmployee")]
org.dist <- org[, c("log.expense", "TotalEmployee") ]

for(i in 1:dim(dat.test)[1] ){
  compare <- rbind(org.dist, dat.test.dist[i, ])
  dist[i] <- distance(compare, method = "euclidean")
}


#####################################
### Testing Distance Metric 
######################################

#enter in your organizations credentials 
org <- data.frame(FormYr =  2019, 
                  State = "CO", #State.region make a state region by state,
                  MajorGroup = "I",
                  NTEE = "A",
                  NEEE.CC = "A01",
                  UNIV = F,
                  HOSP = F,
                  TotalExpense = 500000,
                  TotalEmployee = 150,
                  FormType = "990"
              )
                  
#enter what you want to search on
search.criteria <- list(FormYr = c(2018, 2019),  #default to form year of org
                  State = NA, #default to NA
                  #State.region make a state region by state,
                  MajorGroup = "I", #default to major group of org 
                  NTEE = NA,#default to NA
                  NEEE.CC = NA,#default to NA
                  UNIV = NA, #default to input of org
                  HOSP = NA,#default to input of org
                  TotalExpense = c(100000,1000000),#default to NA
                  TotalEmployee = NA,#default to NA
                  FormType = NA#default to NA
)


## Filter data-by-sector.csv by the inputed search criteria 

dat.filtered <- dat_filtering(form.year = search.criteria$FormYr,
                              state = search.criteria$State,
                              major.group = search.criteria$MajorGroup,
                              tot.expense = search.criteria$TotalExpense)


## Find closest orgs based on total expenses and total employees 

dat.filtered <- dat.filtered%>%
  mutate(dist = NA) %>%
  mutate(log.expense = log(TotalExpense, base = 10))

org.dist <- org %>%
  select( TotalExpense, TotalEmployee) %>%
  mutate(log.expense = log(TotalExpense, base = 10)) %>%
  select(-TotalExpense)


for(i in 1:dim(dat.filtered)[1] ){
  compare <- rbind(org.dist, dat.filtered[i, c("TotalEmployee", "log.expense")])
  dat.filtered$dist[i] <- distance(compare, method = "euclidean")
}

#Get rid of the NA's - they are EZ filers - will have to figure out how to deal with that later 
dat.result <- dat.filtered %>%
  filter(!is.na(dist)) %>%
  arrange(dist ) %>%
  slice(1:10) %>%
  select(FormYr, State, MajorGroup, NTEE, NTEE.CC, UNIV, HOSP, TotalExpense, TotalEmployee, CEOCompensation, FormType)

org

suggested.pay <- median(dat.result$CEOCompensation)

