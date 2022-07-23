server <- function(input, output, session) {
  shinyhelper::observe_helpers()


  #########################################
  ### CEO Compensation page
  #######################################
  ### Get the Organizations Characteristics
  org <- reactive({
    list(State = input$org.State,
         Loc = input$org.loc,
         MajorGroup = input$org.MajorGroup,
         NTEE = input$org.NTEE, #need to add input for this
         NTEE.CC = input$org.NTEE.CC, #need to add input for this
         UNIV = input$org.HOSP,
         HOSP = input$org.UNIV,
         TotalExpense = input$org.TotalExpense,
         TotalEmployee = input$org.TotalEmployee,
         EZQual = input$org.EZQual)
  })
  
  output$test1 <- renderText({
    paste(org())
  })

  #Get the Search Criteria
  search <- reactive({
    search.1 <- list(FormYr = input$search.FormYr,
                     State = input$search.State,
                     Loc = input$search.loc,
                     MajorGroup = input$search.MajorGroup,
                     NTEE = input$search.ntee, #need to add input for this
                     NTEE.CC = NA, #need to add input for this
                     HOSP = input$search.HOSP,
                     UNIV = input$search.UNIV,
                     TotalExpense = input$search.TotalExpenses,
                     TotalEmployee = input$search.TotalEmployees,
                     EZQual = base::ifelse(input$search.FormType =="Yes", "990EZ", "990")
                     )

    # Change null to na
    if(is.null(search.1$FormYr)){search.1$FormYr <- NA}
    if(is.null(search.1$State)){search.1$State <- NA}
    if(is.null(search.1$Loc)){search.1$Loc <- NA}
    if(is.null(search.1$MajorGroup)){search.1$MajorGroup <- NA}
    if(is.null(search.1$NTEE)){search.1$NTEE <- NA}
    if(is.null(search.1$NTEE.CC)){search.1$NTEE.CC <- NA}
    if(is.null(search.1$HOSP)){search.1$HOSP <- NA}
    if(is.null(search.1$UNIV)){search.1$UNIV <- NA}
    if(is.null(search.1$TotalExpense)){search.1$TotalExpense <- c(-Inf, Inf)}
    if(is.null(search.1$TotalEmployee)){search.1$TotalEmployee <- c(0, Inf)}
    if(is.null(search.1$EZQual)){search.1$EZQual <- NA}

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

  output$dat.filterd.table <- DT::renderDataTable({
    datatable(head(dat.filtered(), 10))
  })






  ### Do the search
  dat.similar <- reactive({
    HEOM_with_weights(org = org(), dat.filtered = dat.filtered())
  })
  #
  # # output$similar <- DT::renderDataTable({
  #    head(dat.filtered(), n=100)
  # #   #dat.similar()
  # # }, rownames = FALSE)

  # output$similar <- renderTable({
  #   head(dat.filtered(), n=100)
  # })

  output$ceo.suggest <- renderText({
    paste(search())
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



