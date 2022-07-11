#######################
### Gender Graph Testing
#######################

### Libraries
library(dplyr)
library(ggplot2)
library(readr)

#### Read in Data
dat <- read_csv("data-wrangle/step-04-ceo-final.csv")

#### Aggrigate by health and education with gender 

dat.wrang <- dat %>%
  select(HEALTH, EDU, HOSP, UNIV, totalComp, gender, transitions) %>%
  #make a sector column
  mutate(sector = case_when(HEALTH==T & EDU == F & HOSP==F & UNIV == F ~ "health", 
                            HEALTH==T & EDU == F & HOSP==T & UNIV == F ~ "hosp",
                            HEALTH==F & EDU == T & HOSP==F & UNIV == F ~ "edu",
                            HEALTH==F & EDU == T & HOSP==F & UNIV == T ~ "univ") ) %>%
  #remove transitional years
  filter(!transitions )%>%
  #remove unknown gender
  filter(gender != "U")%>%
  select(totalComp, sector, gender)

dat.agg <- dat.wrang$totalComp %>%
  aggregate(list(sec = factor(dat.wrang$sector), gen = factor(dat.wrang$gender)), median) %>%
  rename(comp.mean = x )

ggplot(dat.agg)+ 
  geom_point(aes(x = comp.mean, y = sec, color = gen)) +
  ylab("Sector") +
  xlab("Median Compensation") +
  ggtitle("Median Compensations by Sector")


#################################
### Data - by - sectors testing 
#################################


data_by_sector <- read_csv("data-wrangle/data-by-sector.csv")
source("funcs/applying-filters-func.R")

dat.filtered <- data_by_sector%>%
  filter(FormYr == 2019)

dat.agg <- dat.filtered$CEOCompensation %>%
  aggregate(list(sec = factor(dat.filtered$MajorGroup), gen = factor(dat.filtered$Gender)), median) %>%
  rename(comp.med = x ) %>%
  filter(gen != "U" )

ggplot(dat.agg)+ 
  geom_point(aes(x = comp.med, y = sec, color = gen)) +
  ylab("Sector") +
  xlab("Median Compensation") +
  scale_y_discrete(labels=as.roman(1:10))


#############################
### 
############################
source("funcs/applying-filters-func.R")
library(ggplot2)
library(plotly)
library(dplyr)


dat.filterd <- dat_filtering(form.year = NA,
                             state = NA,
                             #major.group = c(3,4,  6, 7),
                             #ntee = c("E", "F", "G", "H", "I", "J", "K"),
                             #ntee.cc = c("E11", "H43", "J12"  ,  "K26", "I73" ),
                             hosp = NA,
                             univ = NA,
                             form.type = NA ,
                             tot.expense = c(-Inf, Inf),
                             tot.employee = c(0, Inf)
                )


dat.filterd$MajorGroup[which(dat.filterd$HOSP) == T] <- 11
dat.filterd$MajorGroup[which(dat.filterd$UNIV) == T] <- 12


#yaxis options: "MajorGroup", "NTEE", "NTEE.CC"
y.axis <- c("MajorGroup")
#stat options: Median, Mean
s <- c("Mean")



dat.plot <- dat.filterd %>%
  filter(Gender != "U") %>%
  select(CEOCompensation, Gender, paste(y.axis)) %>%
  rename(Yaxis = paste(y.axis)) %>%
  group_by_at(vars(-CEOCompensation)) %>%
  summarise(Value =  ifelse(s == "Median", median(CEOCompensation), mean(CEOCompensation)), .groups = "keep") %>%
  ungroup()

if(y.axis == "MajorGroup"){
  dat.plot <- dat.plot %>%
    mutate(Yaxis = case_when(Yaxis == 1 ~ "Arts, Culture, and Humanities", 
                             Yaxis == 2 ~ "Education",
                             Yaxis == 3 ~ "Environment and Animals",
                             Yaxis == 4 ~ "Health",
                             Yaxis == 5 ~ "Human Services",
                             Yaxis == 6 ~ "International, Foreign Affairs",
                             Yaxis == 7 ~ "Public, Societal Benefit",
                             Yaxis == 8 ~ "Religion Related",
                             Yaxis == 9 ~ "Mutual/Membership Benefit",
                             Yaxis == 10 ~ "Unknown/Unclassified",
                             Yaxis == 11 ~ "Hosptial",
                             Yaxis == 12 ~ "University"))
} 



dat.plot$Gender <- as.factor(dat.plot$Gender)
dat.plot$Yaxis <- as.factor(dat.plot$Yaxis)

# colnames(dat.plot)[which(colnames(dat.plot) == "Yaxis")] <- case_when(y.axis == "MajorGroup" ~ "MajorGroup",
#                                                                       y.axis == "NTEE" ~ "NTEE",
#                                                                       y.axis == "NTEE.CC" ~ "NTEE.CC")

p <- dat.plot %>%
  ggplot(aes(x = Value, 
             y = reorder(Yaxis,Value), 
             color = Gender,
             text = paste("Classification:", Yaxis))) +
  geom_point() + 
  geom_line(aes(group = Yaxis), col = "gray" ) +
  scale_color_manual(values=c("#FFB1CB", "#01A6EA")) + 
  geom_text()
  ggtitle(paste("Gender Pay Gap by", case_when(y.axis == "MajorGroup" ~ "Major Group",
                                               y.axis == "NTEE" ~ "NTEE Code",
                                               y.axis == "NTEE.CC" ~ "NTEE-CC Code"))) +
  xlab(paste(s, "CEO Compensation")) +
  ylab(paste(case_when(y.axis == "MajorGroup" ~ "Major Group",
                       y.axis == "NTEE" ~ "NTEE Code",
                       y.axis == "NTEE.CC" ~ "NTEE-CC Code")))


ggplotly(p,
         tooltip = c( "text", "Gender", "Value"))


################################### 
### Plotting it with geom_segment###
###################################

dat.diff <- dat.plot %>%
  tidyr::pivot_wider(names_from = Gender, values_from = Value) %>%
  dplyr::rename(c( "Female" = F, "Male" = M)) %>%
  mutate(diff = abs(Female- Male)) %>%
  mutate(pois = (Female + Male) / 2) %>%
  select(c(Yaxis, pois, diff)) %>%
  merge(dat.plot)

dollarize <- function(x)
{ paste0("$", format( round( x, 0 ), big.mark="," ) ) }


p2 <- dat.diff %>%
  ggplot(aes(x = Value, 
             y = reorder(Yaxis,Value), 
             color = Gender,
             text = paste("Classification:", Yaxis))) +
  geom_point() + 
  geom_line(aes(group = Yaxis), col = "gray" ) +
  geom_text(aes(x = pois , y = reorder(Yaxis,Value) , 
                label = dollarize (round(diff))),
            nudge_y = 0.3, 
            color = "grey")+
  scale_color_manual(values=c("#FFB1CB", "#01A6EA")) + 
  ggtitle(paste("Gender Pay Gap by", case_when(y.axis == "MajorGroup" ~ "Major Group",
                                             y.axis == "NTEE" ~ "NTEE Code",
                                             y.axis == "NTEE.CC" ~ "NTEE-CC Code"))) +
  xlab(paste(s, "CEO Compensation")) +
  ylab(paste(case_when(y.axis == "MajorGroup" ~ "Major Group",
                       y.axis == "NTEE" ~ "NTEE Code",
                       y.axis == "NTEE.CC" ~ "NTEE-CC Code"))) 

ggplotly(p2,tooltip = c( "text", "Gender", "Value"))
