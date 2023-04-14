#### Making the Dendogram/Collapsable Tree for orginal NTEE Codes 


#devtools::install_github("AdeelK93/collapsibleTree")
library(collapsibleTree)
library(tidyverse)

ntee.codes <- read.csv("data-raw/ntee-disaggregated.csv")
ntee.values <- read.csv("data-raw/ntee-original.csv")



dat.letter1 <- 
  ntee.values%>%
  filter(substr(ntee, 3, 3) == 0) %>%
  mutate(letter1 = substr(ntee, 1, 2)) %>%
  select(-c(ntee)) 
  

dat.letter2 <- 
  ntee.values %>%
  filter(nchar(ntee) == 3) %>%
  select(-definition) %>%
  mutate(description = gsub("<*>", "", description)) %>%
  rename(letter2 = ntee)


## Spaces Structure to differentiate levels for the TreeNetwork
## broad category = 0 spaces at end
## major group = 1 spaces at end 
## letter + 1 digit = 2 spaces at end (description tens)
## letter + 2 digit = 3 spaces at end (description ones)


NTEE <- compensator::ntee.crosswalk %>%
  filter(nchar(ntee) == 3) %>%
  mutate(letter1 = paste0(major.group, tens)) %>%
  mutate(letter2 = ntee) %>%
  left_join(dat.letter1, by = "letter1") %>% 
  mutate(description.tens = 
           ifelse(is.na(description), 
                  paste("Specality - ", major.group), 
                  description)) %>% 
  left_join(dat.letter2, by = "letter2" ) %>%
  mutate(description.ones = 
           ifelse(type.org == "speciality",
                  paste(major.group, " - ", description.y),
                  description.y)) %>%
  dplyr::mutate(broad.category = case_when(
    broad.category == 1 ~ "Arts, Culture & Humanities",
    broad.category == 2 ~ "Education",
    broad.category == 3 ~ "Environment and Animals",
    broad.category == 4 ~ "Health",
    broad.category == 5 ~ "Human Services",
    broad.category == 6 ~ "International, Foreign Affairs",
    broad.category == 7 ~ "Public, Societal Benefit",
    broad.category == 8 ~ "Religion Related",
    broad.category == 9 ~ "Mutual/Membership Benefit",
    broad.category == 10 ~ "Unknown/Unclassified",
    broad.category == 11 ~ "Education",
    broad.category == 12 ~ "Health")) %>%
  dplyr::mutate(major.group = case_when(
    major.group == "A" ~ "Arts, Culture & Humanities",
    major.group == "B" ~ "Education", 
    major.group == "C" ~ "Environment",
    major.group == "D" ~ "Animal-Related",
    major.group == "E" ~ "Health Care",
    major.group == "F" ~ "Mental Health & Crisis Intervention",
    major.group == "G" ~ "Voluntary Health Associations & Medical Disciplines",
    major.group == "H" ~ "Medical Research",
    major.group == "I" ~ "Crime & Legal-Related",
    major.group == "J" ~ "Employment",
    major.group == "K" ~ "Food, Agriculture & Nutrition",
    major.group == "L" ~ "Housing & Shelter",
    major.group == "M" ~ "Public Safety, Disaster Preparedness & Relief",
    major.group == "N" ~ "Recreation & Sports",
    major.group == "O" ~ "Youth Development",
    major.group == "P" ~ "Human Services",
    major.group == "Q" ~ "International, Foreign Affairs & National Security",
    major.group == "R" ~ "Civil Rights, Social Action & Advocacy",
    major.group == "S" ~ "Community Improvement & Capacity Building",
    major.group == "T" ~ "Philanthropy, Voluntarism & Grantmaking Foundations",
    major.group == "U" ~ "Science & Technology",
    major.group == "V" ~ "Social Science",
    major.group == "W" ~ "Public & Societal Benefit",
    major.group == "X" ~ "Religion-Related",
    major.group == "Y" ~ "Mutual & Membership Benefit",
    major.group == "Z" ~ "Unknown ")) %>%
  select(broad.category, major.group, description.tens, description.ones, ntee, definition)

NTEE <- as.data.frame(lapply(NTEE, function(y) gsub("/", "-", y)))

NTEE$tooltip = "YES"
  
  
collapsibleTree(
  NTEE,
  hierarchy = c("broad.category", "major.group", "description.tens", "description.ones", "ntee"),
  width = 3000,
  fill = "red",
  fillByLevel = TRUE,
  tooltipHtml = "Yes"
  ) 


#### NTEE TREE _----------------------------------------

NTEE <- NTEE %>%
  dplyr::mutate(major.group = paste0(major.group, " ")) %>%
  dplyr::mutate(description.tens = paste0(description.tens, "  ")) %>%
  dplyr::mutate(description.ones = paste0(description.ones, "   ")) %>%
  mutate(pathString =
    paste0("NTEE:", 
           broad.category, ":", 
           major.group, ":",
           description.tens, ":", 
           description.ones, ":",
           ntee))

ntee.join <- 
  ntee.values %>%
  select(ntee, definition)%>%
  rename(to = ntee)

ntee.tree <-  data.tree::as.Node(NTEE, pathDelimiter = ":")


NTEE2 <-
  data.tree::ToDataFrameNetwork(
    ntee.tree, 
    "level", "count", # add level for tooltip later
    direction = "climb", 
    format = TRUE, 
    inheritFromAncestors = TRUE) %>% 
  mutate(times = lengths(regmatches(to, gregexpr("/", to))))%>%
  mutate(from = str_split_i(from, "/", -1)) %>%
  mutate(to =   str_split_i(to, "/", -1)) %>%
  mutate(to = ifelse(
    grepl( "Specality*", to),
    gsub("[[:space:]]*$","",to),
    to)) %>%
  mutate(from = ifelse(
    grepl( "Specality*", from),
    gsub("[[:space:]]*$","",from),
    from)) %>%
  mutate(remove = to == from) %>%
  filter(!remove)  %>%
  left_join(ntee.join) %>%
  mutate(tooltip = case_when(level == 0 ~ " ",
                             level == 1 ~ " ",
                             level == 2 ~ paste("Broad Category:", to, 
                                                "<br>Contains", count, "Major Groups"),
                             level == 3 ~ paste("Major Group:", to,
                                                "<br>Contains", count, "Divisions"),
                             level == 4 ~ paste("Division:", to,
                                                "<br>Contains", count, "Subdivisions"),
                             level == 5 ~ paste("Subdivision:", to,
                                                "<br>Contains", count, "NTEE Codes"),
                             level == 6 ~ paste("NTEE Code:", to,
                                                "<br>Definition:", definition))) %>%
  add_row(from = NA, to = "NTEE", level = 0, count = 0, remove = FALSE, definition = "") %>%
  select(from, to, tooltip)



#save this graph as an html document for ntee tab on dashboard 
#images/saved in ntee-dendogram.html
collapsibleTreeNetwork(NTEE2,
                       width = 3000,
                       height = 500,
                       linkLength = 110,
                       fill = "#0082ea",
                       tooltipHtml = "tooltip")




