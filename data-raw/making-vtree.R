dat.vtree <-
  ntee.values %>%
  filter(nchar(ntee) == 1) %>%
  select(-definition) %>%
  rename(mg = ntee) %>%
  mutate(bc = c("Arts, Culture, and Humanities", 
                "Education", 
                rep("Environment and Animals",2),
                rep("Health", 4), 
                rep("Human Services", 8),
                "International, Foreign Affairs", 
                rep("Public, Societal Benefit", 6),
                "Religion Related",
                "Mutual/Membership Benefit", 
                "Unknown, Unclassified")) %>%
  mutate(size = table(compensator::EIN.filtering$major.group)) %>%
  mutate(per = size / nrow(compensator::EIN.filtering))

write.csv(dat.vtree, file = "data-raw/vtree.csv")
