#org characteristics 
org <- list(FormYr = 2019,
            State = "GA",
            MajorGroup = 3,
            NTEE = "C", #need to add input for this
            NTEE.CC = "B01", #need to add input for this
            UNIV = FALSE,
            HOSP = FALSE,
            TotalExpense = 1000000,
            TotalEmployee = 12,
            EZQual = FALSE,
            Loc = "Urban"
)

#search criteria
search <- list(FormYr = 2019,
            State = c("GA", "SC", "NC"),
            MajorGroup = 3,
            NTEE = c("C", "D"), #need to add input for this
            NTEE.CC = NA, #need to add input for this
            UNIV = 1,
            HOSP = 1,
            TotalExpense = c(500000, 10000000),
            TotalEmployee = c(5, 30),
            EZQual = 2,
            Loc = c("Urban", "Suburan")
)

#True is hard match, FALSE is soft match
hard <- list(FormYr = FALSE,
             State = TRUE,
             MajorGroup = TRUE,
             NTEE = TRUE, #need to add input for this
             NTEE.CC = FALSE, #need to add input for this
             UNIV = FALSE,
             HOSP = FALSE,
             TotalExpense = TRUE,
             TotalEmployee = TRUE,
             EZQual = FALSE,
             Loc = TRUE
)
