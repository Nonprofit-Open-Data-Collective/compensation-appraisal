#yaxis options: "MajorGroup", "NTEE", "NTEE.CC"
y.axis <- "MajorGroup"
#stat options: Median, Mean
s <- "Median"
#get filtered data
dat.filterd <- dat_filtering(form.year = 2019,
                             state = NA,
                             major.group =  NA,
                             ntee = NA,
                             ntee.cc = NA,
                             hosp = NA,
                             univ = NA,
                             tot.expense = c(0, Inf),
                             tot.employee =  c(0, Inf)
)


#if we want to include hospitals, add a major group for hospitals 
dat.filterd$MajorGroup[which(dat.filterd$HOSP == T)] <- 11

#if we want to include universities, add a major group for hospitals 
dat.filterd$MajorGroup[which(dat.filterd$UNIV == T)] <- 12

#format data for plotting
dat.plot <- dat.filterd %>%
  filter(Gender != "U") %>%
  select(CEOCompensation, Gender, paste(y.axis)) %>%
  rename(Yaxis = paste(y.axis)) %>%
  group_by_at(vars(-CEOCompensation)) %>%
  summarise(Value =  ifelse(s == "Median", median(CEOCompensation), mean(CEOCompensation)), n = n(), .groups = "keep") %>%
  ungroup()

#rename major groups to something readable 
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

#recode as factors
dat.plot$Gender <- as.factor(dat.plot$Gender)
dat.plot$Yaxis <- as.factor(dat.plot$Yaxis)

#format
x.label <- paste0(toupper(strsplit(s, "")[[1]][1]), paste0(strsplit(s, "")[[1]][-1], collapse = ""), collapse = "")

#a little more formatting
dat.diff <- dat.plot %>%
  tidyr::pivot_wider(names_from = Gender, values_from = c(Value, n)) %>%
  mutate(diff = abs(Value_F- Value_M)) %>%
  mutate(pois = (Value_F + Value_M) / 2) %>%
  select(c(Yaxis, pois, diff)) %>%
  merge(dat.plot)

#make the ggplot
p <- dat.diff %>%
  ggplot(aes(x = Value, 
             y = reorder(Yaxis,Value), 
             color = Gender,
             text = paste("Classification:", Yaxis),
             n = n)) +
  geom_point() + 
  geom_line(aes(group = Yaxis), col = "gray" ) +
  geom_text(aes(x = pois, y = reorder(Yaxis,Value) , 
                label = dollarize (round(diff))),
            nudge_y = 0.3, 
            color = "grey", 
            size = 3)+
  scale_color_manual(values=c("#9525AD", "#25AD80")) + 
  scale_x_continuous(labels=scales::dollar_format())+
  ggtitle(paste("CEO Pay by Gender by", case_when(y.axis == "MajorGroup" ~ "Major Group",
                                               y.axis == "NTEE" ~ "NTEE Code",
                                               y.axis == "NTEE.CC" ~ "NTEE-CC Code"))) +
  xlab(paste(x.label, "CEO Compensation")) +
  ylab(paste(case_when(y.axis == "MajorGroup" ~ "Major Group",
                       y.axis == "NTEE" ~ "NTEE Code",
                       y.axis == "NTEE.CC" ~ "NTEE-CC Code"))) 

#output plotly
ggplotly(p,
  tooltip = c( "text", "n", "Gender", "Value"))  %>%
  #suppress the hover over the difference line
  style(p, hoverinfo = "none", traces = c(4))




