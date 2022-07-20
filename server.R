function(input, output, session) {
    
  #########################################
  ### CEO Compensation page
  #######################################
  observe_helpers()
  ### Get the Orginizations Characteristics
  org <- reactive({
    list(FormYr = input$org.FormYr,
         State = input$org.State,
         MajorGroup = input$org.MajorGroup,
         NTEE = input$org.NTEE, #need to add input for this 
         NTEE.CC = input$org.NTEE.CC, #need to add input for this 
         UNIV = input$org.HOSP,
         HOSP = input$org.UNIV,
         TotalExpense = input$org.TotalExpense,
         TotalEmployee = input$org.TotalEmployee,
         FormType = input$org.FormType)
  })
 
  #Get the Search Criteria
  search <- reactive({
    search.1 <- list(form.year = input$search.FormYr,
                     state = input$search.State,
                     major.group = input$search.MajorGroup,
                     ntee = NA, #need to add input for this
                     ntee.cc = NA, #need to add input for this
                     hosp = input$search.HOSP,
                     univ = input$search.UNIV,
                     tot.expense = input$search.TotalExpenses,
                     tot.employee = input$search.TotalEmployees,
                     form.type = input$search.FormType)
    
    # Change null to na 
    if(is.null(search.1$form.year)){search.1$form.year <- NA}
    if(is.null(search.1$state)){search.1$state <- NA}
    if(is.null(search.1$major.group)){search.1$major.group <- NA}
    if(is.null(search.1$ntee)){search.1$ntee <- NA}
    if(is.null(search.1$ntee.cc)){search.1$ntee.cc <- NA}
    if(is.null(search.1$hosp)){search.1$hosp <- NA}
    if(is.null(search.1$univ)){search.1$univ <- NA}
    if(is.null(search.1$tot.expense)){search.1$tot.expense <- c(-Inf, Inf)}
    if(is.null(search.1$tot.employee)){search.1$tot.employee <- c(0, Inf)}
    if(is.null(search.1$form.type)){search.1$form.type <- NA}
    
    search.1
    
  })
  

  ### Get filtered data 
  dat.filtered <- reactive({
    dat_filtering(form.year = search()$form.year,
                  state = search()$state,
                  major.group = search()$major.group,
                  ntee = search()$ntee,
                  ntee.cc = search()$ntee.cc,
                  hosp = search()$hosp,
                  univ = search()$univ,
                  tot.expense = search()$tot.expense,
                  tot.employee = search()$tot.employee,
                  form.type = search()$form.type
    )
  })

  
  ### Do the search
  dat.similar <- reactive({
    HEOM_with_weights(org = org(), dat.filtered = dat.filtered())
  })
  
  output$similar <- DT::renderDataTable({
    dat.similar()
  }, rownames = FALSE)

  # output$similar <- renderTable({
  #   dat.similar()
  # })
  
  output$ceo.suggest <- renderText({
    paste(names(org()))
    #paste("Your suggested compensation is ", median(dat.similar()$CEOCompensation))
  })


    
    
  #########################################
  ### Gender Pay gap difference in diference model
  ####################################### 
  

    
    
  #########################################
  ### Gender Pay Gap Graphs Page 
  #######################################
    
  #Format the Search Criteria
  gender.graph.filters <- reactive({
    filter.gender.1 <- list(form.year = input$filter.gender.FormYr,
                     state = input$filter.gender.State,
                     major.group = input$filter.gender.MajorGroup,
                     ntee = NA, #need to add input for this
                     ntee.cc = NA, #need to add input for this
                     hosp = input$filter.gender.HOSP,
                     univ = input$filter.gender.UNIV,
                     tot.expense = input$filter.gender.TotalExpenses,
                     tot.employee = input$filter.gender.TotalExpenses)
    
    # Change null to na 
    if(is.null(filter.gender.1$form.year)){filter.gender.1$form.year <- NA}
    if(is.null(filter.gender.1$state)){filter.gender.1$state <- NA}
    if(is.null(filter.gender.1$major.group)){filter.gender.1$major.group <- NA}
    if(is.null(filter.gender.1$ntee)){filter.gender.1$ntee <- NA}
    if(is.null(filter.gender.1$ntee.cc)){filter.gender.1$ntee.cc <- NA}
    if(is.null(filter.gender.1$hosp)){filter.gender.1$hosp <- NA}
    if(is.null(filter.gender.1$univ)){filter.gender.1$univ <- NA}
    if(is.null(filter.gender.1$tot.expense)){filter.gender.1$tot.expense <- c(-Inf, Inf)}
    if(is.null(filter.gender.1$tot.employee)){filter.gender.1$tot.employee <- c(0, Inf)}

    filter.gender.1
    
  })
    
  ## Testing that the filtering criteria is what it should be 
  output$test <- renderText({
    paste(gender.graph.filters())
  })
  
  ## Output Gender Pay Graph
  output$gender.pay.graph <- renderPlotly({
    
    #yaxis options: "MajorGroup", "NTEE", "NTEE.CC"
    y.axis <- input$filter.gender.graph.yaxis
    #stat options: Median, Mean
    s <- input$filter.gender.graphs.method
    #get filtered data
    dat.filterd <- dat_filtering(form.year = gender.graph.filters()$form.year,
                                 state = gender.graph.filters()$state,
                                 major.group =  gender.graph.filters()$major.group,
                                 ntee = gender.graph.filters()$ntee,
                                 ntee.cc = gender.graph.filters()$ntee.cc,
                                 hosp = gender.graph.filters()$hosp,
                                 univ = gender.graph.filters()$univ,
                                 tot.expense =  gender.graph.filters()$tot.expense,
                                 tot.employee =  gender.graph.filters()$tot.employee
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
      summarise(Value =  ifelse(s == "Median", median(CEOCompensation), mean(CEOCompensation)), .groups = "keep") %>%
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
      tidyr::pivot_wider(names_from = Gender, values_from = Value) %>%
      dplyr::rename(c( "Female" = F, "Male" = M)) %>%
      mutate(diff = abs(Female- Male)) %>%
      mutate(pois = (Female + Male) / 2) %>%
      select(c(Yaxis, pois, diff)) %>%
      merge(dat.plot)
    
    #make the ggplot
    p <- dat.diff %>%
      ggplot(aes(x = Value, 
                 y = reorder(Yaxis,Value), 
                 color = Gender,
                 text = paste("Classification:", Yaxis))) +
      geom_point() + 
      geom_line(aes(group = Yaxis), col = "gray" ) +
      geom_text(aes(x = pois, y = reorder(Yaxis,Value) , 
                    label = dollarize (round(diff))),
                nudge_y = 0.3, 
                color = "grey", 
                size = 3)+
      scale_color_manual(values=c("#9525AD", "#25AD80")) + 
      ggtitle(paste("Gender Pay Gap by", case_when(y.axis == "MajorGroup" ~ "Major Group",
                                                   y.axis == "NTEE" ~ "NTEE Code",
                                                   y.axis == "NTEE.CC" ~ "NTEE-CC Code"))) +
      xlab(paste(x.label, "CEO Compensation")) +
      ylab(paste(case_when(y.axis == "MajorGroup" ~ "Major Group",
                           y.axis == "NTEE" ~ "NTEE Code",
                           y.axis == "NTEE.CC" ~ "NTEE-CC Code"))) 
    
    #output plotly
    ggplotly(p,
             tooltip = c( "text", "Gender", "Value"))
    
  })

  
  
  ### Helpter functions that aren't working 
   # demostrate helpers on dynamic UI
   output$dynamicUI <- renderUI({
     h4("Click the help icon for current details...") %>% 
       helper(icon = "question", 
              colour = "orange",
              size = "s",
              type = "markdown",
              title = "Current Details",
              content = "Clusters")
   })
   
} #end function



