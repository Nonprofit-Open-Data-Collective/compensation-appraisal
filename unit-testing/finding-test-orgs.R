###########################
### Load Libraries and data
#########################
library(tidyverse)
dat.final <- read_csv("data-rodeo/dat-shinyapp.csv")




########################
### Data Summary
########################
summary(dat.final)


########################
###  Educational Org - non univ
##########################

dat.edu <- dat.final %>%
  filter(MajorGroup == 2 & !UNIV) 

#summary(dat.edu)

dat.edu.att <- dat.edu %>%
  select(TotalExpense, TotalEmployee, TotalAssests, GrossReceipts, CEOCompensation)

#plot(dat.edu.att)

med.edu <- data.frame(Vals = apply(dat.edu.att,2,median),
                       Vars = c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))


dat.edu.att$dist.2 <- rep(0, dim(dat.edu.att)[1])
for(i in 1:(dim(dat.edu.att)[1])){
  dat.edu.att$dist.2[i] <- norm(as.matrix(abs(dat.edu.att[i, ] - med.edu[, 1])), "2")
}


row.num <- which(dat.edu.att$dist.2  == min(dat.edu.att$dist.2 ))
View(dat.edu[row.num, ])
EIN.edu <- dat.edu$EIN[row.num]

dat.edu$med.dist <- dat.edu.att$dist.2
dat.edu%>% 
  arrange(med.dist) %>%
  slice(1:5) %>%
  View()

edu.org <- data.frame(Vals =  unlist(dat.edu.att[row.num, c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation")]),
                       Vars =  c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))


cbind(dat.edu$EIN, dat.edu.att) %>%
  filter(between(TotalExpense, quantile(TotalExpense, 0.1), quantile(TotalExpense, 0.9))) %>%
  filter(between(TotalEmployee, quantile(TotalEmployee, 0.1), quantile(TotalEmployee, 0.9))) %>%
  filter(between(TotalAssests, quantile(TotalAssests, 0.1), quantile(TotalAssests, 0.9))) %>%
  filter(between(GrossReceipts, quantile(GrossReceipts, 0.1), quantile(GrossReceipts, 0.9))) %>%
  pivot_longer(!"dat.edu$EIN", names_to = "Vars", values_to = "Vals") %>%
  ggplot(aes(x = Vals)) + 
  geom_density()+
  geom_vline(aes(xintercept = Vals), data=edu.org, col = "red") +
  facet_wrap( ~Vars, scales = "free")+
  ggtitle(" Education - Nonuniversity ")


########################
###  Univ Org 
##########################
dat.univ <- dat.final %>%
  filter(UNIV) 

#summary(dat.edu)

dat.univ.att <- dat.univ %>%
  select(TotalExpense, TotalEmployee, TotalAssests, GrossReceipts, CEOCompensation)

#plot(dat.univ.att)

med.univ <- data.frame(Vals = apply(dat.univ.att,2,median),
                      Vars = c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))


dat.univ.att$dist.2 <- rep(0, dim(dat.univ.att)[1])
for(i in 1:(dim(dat.univ.att)[1])){
  dat.univ.att$dist.2[i] <- norm(as.matrix(abs(dat.univ.att[i, ] - med.univ[, 1])), "2")
}


row.num <- which(dat.univ.att$dist.2  == min(dat.univ.att$dist.2 ))
View(dat.univ[row.num, ])
#https://www.newarktrust.org
EIN.univ <- dat.univ$EIN[row.num]

dat.univ$med.dist <- dat.univ.att$dist.2
dat.univ %>% 
  arrange(med.dist) %>%
  slice(1:5) %>%
  View()

univ.org <- data.frame(Vals =  unlist(dat.univ.att[row.num, c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation")]),
                      Vars =  c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))



cbind(dat.univ$EIN, dat.univ.att) %>%
  filter(between(TotalExpense, quantile(TotalExpense, 0.1), quantile(TotalExpense, 0.9))) %>%
  filter(between(TotalEmployee, quantile(TotalEmployee, 0.1), quantile(TotalEmployee, 0.9))) %>%
  filter(between(TotalAssests, quantile(TotalAssests, 0.1), quantile(TotalAssests, 0.9))) %>%
  filter(between(GrossReceipts, quantile(GrossReceipts, 0.1), quantile(GrossReceipts, 0.9))) %>%
  pivot_longer(!"dat.univ$EIN", names_to = "Vars", values_to = "Vals") %>%
  ggplot(aes(x = Vals)) + 
  geom_density()+
  geom_vline(aes(xintercept = Vals), data=univ.org, col = "red") +
  facet_wrap( ~Vars, scales = "free")+
  ggtitle(" University ")


##########################
### Hospital
############################
dat.hosp <- dat.final %>%
  filter(HOSP) 

#summary(dat.hosp)

dat.hosp.att <- dat.hosp %>%
  select(TotalExpense, TotalEmployee, TotalAssests, GrossReceipts, CEOCompensation)

#plot(dat.hosp.att)

med.hosp <- data.frame(Vals = apply(dat.hosp.att,2,median),
                       Vars = c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))


dat.hosp.att$dist.2 <- rep(0, dim(dat.hosp.att)[1])
for(i in 1:(dim(dat.hosp.att)[1])){
  dat.hosp.att$dist.2[i] <- norm(as.matrix(abs(dat.hosp.att[i, ] - med.hosp[, 1])), "2")
}


row.num <- which(dat.hosp.att$dist.2  == min(dat.hosp.att$dist.2 ))
View(dat.hosp[row.num, ])
#https://www.newarktrust.org
EIN.hosp <- dat.hosp$EIN[row.num]

dat.hosp$med.dist <- dat.hosp.att$dist.2
dat.hosp%>% 
  arrange(med.dist) %>%
  slice(1:5) %>%
  View()


hosp.org <- data.frame(Vals =  unlist(dat.hosp.att[row.num, c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation")]),
                       Vars =  c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))



cbind(dat.hosp$EIN, dat.hosp.att) %>%
  filter(between(TotalExpense, quantile(TotalExpense, 0.1), quantile(TotalExpense, 0.9))) %>%
  filter(between(TotalEmployee, quantile(TotalEmployee, 0.1), quantile(TotalEmployee, 0.9))) %>%
  filter(between(TotalAssests, quantile(TotalAssests, 0.1), quantile(TotalAssests, 0.9))) %>%
  filter(between(GrossReceipts, quantile(GrossReceipts, 0.1), quantile(GrossReceipts, 0.9))) %>%
  pivot_longer(!"dat.hosp$EIN", names_to = "Vars", values_to = "Vals") %>%
  ggplot(aes(x = Vals)) + 
  geom_density()+
  geom_vline(aes(xintercept = Vals), data=hosp.org, col = "red") +
  facet_wrap( ~Vars, scales = "free") +
  ggtitle(" Hospital ")


#########################
### Health clinic - non hospital
# this is just the "most average" clinic, not the "most average" large clinic, but can easily be changed
############################
dat.health <- dat.final %>%
  filter(MajorGroup == 4 & !HOSP) 

#summary(dat.health)

dat.health.att <- dat.health %>%
  select(TotalExpense, TotalEmployee, TotalAssests, GrossReceipts, CEOCompensation)

#plot(dat.health.att)

med.health <- data.frame(Vals = apply(dat.health.att,2,median),
                       Vars = c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))


dat.health.att$dist.2 <- rep(0, dim(dat.health.att)[1])
for(i in 1:(dim(dat.health.att)[1])){
  dat.health.att$dist.2[i] <- norm(as.matrix(abs(dat.health.att[i, ] - med.health[, 1])), "2")
}


row.num <- which(dat.health.att$dist.2  == min(dat.health.att$dist.2 ))
View(dat.health[row.num, ])
#https://www.newarktrust.org
EIN.health <- dat.health$EIN[row.num]

dat.health$med.dist <- dat.health.att$dist.2
dat.health%>% 
  arrange(med.dist) %>%
  slice(1:5) %>%
  View()


health.org <- data.frame(Vals =  unlist(dat.health.att[row.num, c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation")]),
                       Vars =  c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))



cbind(dat.health$EIN, dat.health.att) %>%
  filter(between(TotalExpense, quantile(TotalExpense, 0.1), quantile(TotalExpense, 0.9))) %>%
  filter(between(TotalEmployee, quantile(TotalEmployee, 0.1), quantile(TotalEmployee, 0.9))) %>%
  filter(between(TotalAssests, quantile(TotalAssests, 0.1), quantile(TotalAssests, 0.9))) %>%
  filter(between(GrossReceipts, quantile(GrossReceipts, 0.1), quantile(GrossReceipts, 0.9))) %>%
  pivot_longer(!"dat.health$EIN", names_to = "Vars", values_to = "Vals") %>%
  ggplot(aes(x = Vals)) + 
  geom_density()+
  geom_vline(aes(xintercept = Vals), data=health.org, col = "red") +
  facet_wrap( ~Vars, scales = "free") +
  ggtitle(" Health - Nonhospital ")


#########################
### Arts Administrator 
############################
dat.arts <- dat.final %>%
  filter(MajorGroup == 1)  %>%
  filter( sub('.', '', NTEE.CC) %in% c(23:27, 34, 40, 50:80) )  

summary(dat.arts)

dat.arts.att <- dat.arts %>%
  select(TotalExpense, TotalEmployee, TotalAssests, GrossReceipts, CEOCompensation)

plot(dat.arts.att)



med.arts <- data.frame(Vals = apply(dat.arts.att,2,median),
                       Vars = c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))


dat.arts.att$dist.2 <- rep(0, dim(dat.arts.att)[1])
for(i in 1:(dim(dat.arts.att)[1])){
  dat.arts.att$dist.2[i] <- norm(as.matrix(abs(dat.arts.att[i, ] - med.arts[, 1])), "2")
}


row.num <- which(dat.arts.att$dist.2  == min(dat.arts.att$dist.2 ))
View(dat.arts[row.num, ])
#https://www.newarktrust.org
EIN.arts <- dat.arts$EIN[row.num]

dat.arts$med.dist <- dat.arts.att$dist.2
dat.arts%>% 
  arrange(med.dist) %>%
  slice(1:5) %>%
  View()


arts.org <- data.frame(Vals =  unlist(dat.arts.att[row.num, c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation")]),
                       Vars =  c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))




cbind(dat.arts$EIN, dat.arts.att) %>%
  filter(between(TotalExpense, quantile(TotalExpense, 0.1), quantile(TotalExpense, 0.9))) %>%
  filter(between(TotalEmployee, quantile(TotalEmployee, 0.1), quantile(TotalEmployee, 0.9))) %>%
  filter(between(TotalAssests, quantile(TotalAssests, 0.1), quantile(TotalAssests, 0.9))) %>%
  filter(between(GrossReceipts, quantile(GrossReceipts, 0.1), quantile(GrossReceipts, 0.9))) %>%
  pivot_longer(!"dat.arts$EIN", names_to = "Vars", values_to = "Vals") %>%
  ggplot(aes(x = Vals)) + 
  geom_density()+
  geom_vline(aes(xintercept = Vals), data=arts.org, col = "red") +
  facet_wrap( ~Vars, scales = "free") +
  ggtitle(" Arts ")


#########################
### Specialty Orgs 03 - Professional Societies/Associations
##########################
dat.s03 <- dat.final %>%
  filter( sub('.', '', NTEE.CC) == "03" )  

summary(dat.s03)

dat.s03.att <- dat.s03 %>%
  select(TotalExpense, TotalEmployee, TotalAssests, GrossReceipts, CEOCompensation)

plot(dat.s03.att)



med.s03 <- data.frame(Vals = apply(dat.s03.att,2,median),
                       Vars = c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))


dat.s03.att$dist.2 <- rep(0, dim(dat.s03.att)[1])
for(i in 1:(dim(dat.s03.att)[1])){
  dat.s03.att$dist.2[i] <- norm(as.matrix(abs(dat.s03.att[i, ] - med.s03[, 1])), "2")
}


row.num <- which(dat.s03.att$dist.2  == min(dat.s03.att$dist.2 ))
View(dat.s03[row.num, ])
#https://www.newarktrust.org
EIN.s03 <- dat.s03$EIN[row.num]

dat.s03$med.dist <- dat.s03.att$dist.2
dat.s03%>% 
  arrange(med.dist) %>%
  slice(1:5) %>%
  View()


s03.org <- data.frame(Vals =  unlist(dat.s03.att[row.num, c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation")]),
                       Vars =  c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))




cbind(dat.s03$EIN, dat.s03.att) %>%
  filter(between(TotalExpense, quantile(TotalExpense, 0.1), quantile(TotalExpense, 0.9))) %>%
  filter(between(TotalEmployee, quantile(TotalEmployee, 0.1), quantile(TotalEmployee, 0.9))) %>%
  filter(between(TotalAssests, quantile(TotalAssests, 0.1), quantile(TotalAssests, 0.9))) %>%
  filter(between(GrossReceipts, quantile(GrossReceipts, 0.1), quantile(GrossReceipts, 0.9))) %>%
  pivot_longer(!"dat.s03$EIN", names_to = "Vars", values_to = "Vals") %>%
  ggplot(aes(x = Vals)) + 
  geom_density()+
  geom_vline(aes(xintercept = Vals), data=s03.org, col = "red") +
  facet_wrap( ~Vars, scales = "free") +
  ggtitle(" Specality 03 ")


#########################
### Specialty Orgs 05 - Research Institutes and/or Public Policy Analysis
##########################
dat.s05 <- dat.final %>%
  filter( sub('.', '', NTEE.CC) == "05" ) 

summary(dat.s05)

dat.s05.att <- dat.s05 %>%
  select(TotalExpense, TotalEmployee, TotalAssests, GrossReceipts, CEOCompensation)

plot(dat.s05.att)



med.s05 <- data.frame(Vals = apply(dat.s05.att,2,median),
                      Vars = c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))


dat.s05.att$dist.2 <- rep(0, dim(dat.s05.att)[1])
for(i in 1:(dim(dat.s05.att)[1])){
  dat.s05.att$dist.2[i] <- norm(as.matrix(abs(dat.s05.att[i, ] - med.s05[, 1])), "2")
}


row.num <- which(dat.s05.att$dist.2  == min(dat.s05.att$dist.2 ))
View(dat.s05[row.num, ])
#https://www.newarktrust.org
EIN.s05 <- dat.s05$EIN[row.num]

dat.s05$med.dist <- dat.s05.att$dist.2
dat.s05%>% 
  arrange(med.dist) %>%
  slice(1:5) %>%
  View()

s05.org <- data.frame(Vals =  unlist(dat.s05.att[row.num, c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation")]),
                      Vars =  c("TotalExpense", "TotalEmployee", "TotalAssests", "GrossReceipts", "CEOCompensation"))




cbind(dat.s05$EIN, dat.s05.att) %>%
  filter(between(TotalExpense, quantile(TotalExpense, 0.1), quantile(TotalExpense, 0.9))) %>%
  filter(between(TotalEmployee, quantile(TotalEmployee, 0.1), quantile(TotalEmployee, 0.9))) %>%
  filter(between(TotalAssests, quantile(TotalAssests, 0.1), quantile(TotalAssests, 0.9))) %>%
  filter(between(GrossReceipts, quantile(GrossReceipts, 0.1), quantile(GrossReceipts, 0.9))) %>%
  pivot_longer(!"dat.s05$EIN", names_to = "Vars", values_to = "Vals") %>%
  ggplot(aes(x = Vals)) + 
  geom_density()+
  geom_vline(aes(xintercept = Vals), data=s05.org, col = "red") +
  facet_wrap( ~Vars, scales = "free") +
  ggtitle(" Specality 05 ")





############################################################
### Orgs to pick 
############################################################

### EDU
EIN.edu.final <- 237392118
edu.select <- dat.edu[ dat.edu$EIN == EIN.edu.final, ]
edu.select$url <- "https://pipershill.org"

### Univ
EIN.univ.final <- 350868101
univ.select <- dat.univ[ dat.univ$EIN == EIN.univ.final, ]
univ.select$url <- "https://www.huntington.edu"

### Hosp
EIN.hosp.final <- 160743979
hosp.select <- dat.hosp[ dat.hosp$EIN == EIN.hosp.final, ]
hosp.select$url <- "https://www.urmc.rochester.edu/noyes.aspx" #affiliated with the university of Rodchester

#this is more of a "stand alone" hospital
# 390808442
#https://www.healthpartners.com/care/hospitals/westfields/

### Health
EIN.health.final <- 208969896
health.select <- dat.health[ dat.health$EIN == EIN.health.final, ]
health.select$url <- "https://smilegb.org"

### Arts
EIN.arts.final <- 260317963
arts.select <- dat.arts[ dat.arts$EIN == EIN.arts.final, ]
arts.select$url <- "https://www.redlineart.org"

### s03
EIN.s03.final <- 943019570
s03.select <- dat.s03[ dat.s03$EIN == EIN.s03.final, ]
s03.select$url <- "https://www.proteinsociety.org/page/about-us"

### s05
#Research Institute 
EIN.s05.final <- 205935069
s05.select <- dat.s05[ dat.s05$EIN == EIN.s05.final, ]
s05.select$url <- "https://www.cerf.science"

#This one is more "Public Policy Analysis"
#NAME = THE WASHINGTON STATE BUDGET AND POLICY *CENTER*
#https://budgetandpolicy.org
#721612982


############################################################
#### Make final data set of test orgs 
############################################################
orgs.test <- rbind(edu.select, 
                   univ.select,
                   health.select,
                   hosp.select,
                   arts.select,
                   s03.select, 
                   s05.select)

View(orgs.test)

write.csv(dat.testing, "unit-testing/test-orgs.csv")

