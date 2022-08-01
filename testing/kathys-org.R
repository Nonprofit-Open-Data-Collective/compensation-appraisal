#######################
### Test case of Kathy's non profit
########################

#Input info from Jesse's email
# Budget of $150K - $500K. Education services.  
# Washington DC Metro  (or just DC, VA, MD if we need to pick a state).

# It's a tutoring-mentoring program - so maybe these?
# O30 Adult & Child Matching Programs
# B92 Remedial Reading & Encouragement
# The EIN is 82-1656738 if that helps.


#setting up parameters 
org <- list(State = "DC",
            MajorGroup = 2,
            NTEE = "B", 
            NTEE.CC = "B92",
            UNIV = FALSE,
            HOSP = FALSE,
            TotalExpense = 300000,
            TotalEmployee = 3,
            EZQual = TRUE,
            Loc = "Metropolitan"
)


search <- list(FormYr = c(2018, 2019),
               State = c("VA", "DC", "MD"),
               MajorGroup = 2,
               NTEE = c("B"),
               NTEE.CC = NA, #no hard matching on this criteria aaaa
               UNIV = 1,
               HOSP = 1,
               TotalExpense = c(0, 600000),
               TotalEmployee = c(0, 10),
               EZQual = 1,
               Loc = c("Metropolitan")
)

#True is hard match, FALSE is soft match
hard <- list(FormYr = FALSE,
             State = FALSE,
             MajorGroup = TRUE,
             NTEE = TRUE, 
             NTEE.CC = FALSE,
             UNIV = TRUE,
             HOSP = TRUE,
             TotalExpense = TRUE,
             TotalEmployee = TRUE,
             EZQual = TRUE,
             Loc = TRUE
)


#Running the function
source("funcs/dat-filtering-hard.R")
dat.filtered <- dat_filtering_hard(search, hard)

results <- HEOM_with_weights(org, search, dat.filtered)


hist(results$CEOCompensation, breaks = 50)

plot(results$TotalExpense, results$CEOCompensation)
plot(results$TotalEmployee, results$CEOCompensation)


boxplot(results$CEOCompensation)
quantile(results$CEOCompensation, c(0, 10, 25, 50, 75, 90, 100)/ 100)
mean(results$CEOCompensation)


results1 <- results[results$TotalEmployee > 0, ]
hist(results1$CEOCompensation, breaks = 50)

plot(results1$TotalExpense, results1$CEOCompensation)
plot(results1$TotalEmployee, results1$CEOCompensation)


boxplot(results1$CEOCompensation)
quantile(results1$CEOCompensation, c(0, 10, 25, 50, 75, 90, 100)/ 100)
mean(results1$CEOCompensation)

weighed.average <- sum(results$CEOCompensation * (1- results$dist) ) / dim(results)[1]
weighed.average1 <- sum(results1$CEOCompensation * (1- results1$dist) ) / dim(results1)[1]

