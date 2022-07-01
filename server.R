function(input, output, session) {
    
  #########################################
  ### CEO Compensation page
  #######################################
  
  ### Get the Orginizations Characteristics
  org <- reactive({
    list(FormYr = input$org.FormYr,
         State = input$org.State,
         MajorGroup = input$org.MajorGroup,
         NTEE = NA, #need to add input for this 
         NTEE.CC = NA, #need to add input for this 
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
                     tot.employee = input$search.TotalExpenses,
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
  
  
  
  ### Do the search
  
  dat.comparison <- reactive({
    find_comparisons(org = org(), search = search())
  })
  
  output$table.compare <- renderTable({dat.comparison()})
  
  
  output$ceo.suggest <- renderText({
    paste("Your suggested compensation is ", median(dat.comparison()$CEOCompensation))
  })


    
    
  #########################################
  ### Gender Pay gap difference in diference model
  ####################################### 
  

    
    
  #########################################
  ### Gender Pay Gap Graphs Page 
  #######################################
    
   output$graph.gender <- renderPlotly({
     plot_ly(data = dat[1:500, ], x = ~TotalEmployee, y = ~CEOCompensation, type = "scatter")
   })
   
   output$message <- renderText({
     # use the `username` key from input and and return new value
     # for the `message` key in output
     return(paste("The currnt major group choice is", input$major.group.choice))
   })
   
   output$selected_var <- renderText({ 
     paste("You have selected this", input$range)
   })
    
} #end function



