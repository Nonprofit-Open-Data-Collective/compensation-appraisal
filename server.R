server <- function(input, output, session) {
  
  #Needed for Helpers
  shinyhelper::observe_helpers()
  
  #Needed for vaildators
  iv <- InputValidator$new()
  #org total expenses
  iv$add_rule(
    "OrgTotalExpense", 
    sv_required(message = "Please enter a number greater than 0.")) 
  iv$add_rule(
    "OrgTotalExpense", 
    sv_gte(0, message_fmt = "Please enter a number greater than 0.")) #total expenses >=0
  #org total Employees
  iv$add_rule(
    "OrgTotalEmployee", 
    sv_required(message = "Please enter a number greater than 0.")) 
  iv$add_rule(
    "OrgTotalEmployee", 
    sv_gte(0, message_fmt = "Please enter a number greater than 0.")) #total employees >=0
  
  # Search State 
  iv$add_rule(
    "SearchState", 
    sv_required(message = "Please select at least one state."))
  iv$add_rule(
    "SearchLoc", 
    sv_required(message = "Please select at least one city type."))
  #Search major group
  iv$add_rule(
    "SearchMajorGroup",
    sv_required(message = "Please select at least one broad category.")
  )
  #Search NTEE
  iv$add_rule(
    "SearchNTEE",
    sv_required(message = "Please select at least one major group.")
  )
  #Search NTEE2 (only if search.nteecc is selected)
  iv$add_rule(
    "SearchNTEE2",
    sv_required(message = "Please select at least one major group.")
  )
  #Search NTEE-CC
  iv$add_rule(
    "SearchNTEECC",
    sv_required(message = "Please select at least one option.")
  )
  
  
  
  #STILL NEED TO ADD SEARCH.TOTALEXPENSES AND SEARCH.TOTALEMPLOYEES BUT NEED TO WAIT UNTIL JESSE PICKS WHICH FORMAT WE WANT TO USE
  
  iv$enable()
  
  

  #########################################
  ### CEO Compensation page
  #######################################
  ####### Get the Organizations Characteristics #######

  ### Get the NTEE Code correct
  ORGNTEE <- reactive({
    org.MajorGroup <- input$OrgMajorGroup
    
    #if we in Major Group 1
    if(org.MajorGroup == 1 ){ntee <- "A"}
    
    #Major Group 2
    if(org.MajorGroup == 2 ){ntee <- "B"}
    
    #Major Group 3
    if(org.MajorGroup == 3 ){ntee <- input$OrgNTEE3}
    
    #Major Group4
    if(org.MajorGroup == 4 ){ntee <- input$OrgNTEE4}
    
    #Major Group5
    if(org.MajorGroup == 5 ){ntee <- input$OrgNTEE5}
    
    #Major Group6
    if(org.MajorGroup == 6){ntee <- "Q" }
    
    #Major Group7
    if(org.MajorGroup == 7 ){ntee <- input$OrgNTEE7}
    
    #Major Group8
    if(org.MajorGroup == 8){ntee <- "X" }
    
    #Major Group9
    if(org.MajorGroup == 9){ntee <- "Y" }
    
    #Major Group10
    if(org.MajorGroup == 10){ntee <- "Z" }
    
    ntee
  })
  
 
  #Org Inputs
  org <- reactive({
  
    org.temp <- list(State = input$OrgState,
                     Loc = input$OrgLoc,
                     MajorGroup = input$OrgMajorGroup, 
                     NTEE = ORGNTEE(), 
                     NTEE.CC = paste0(ORGNTEE(), input$OrgNTEECC),
                     UNIV = input$OrgHOSP,
                     HOSP = input$OrgUNIV,
                     TotalExpense = input$OrgTotalExpense,
                     TotalEmployee = input$OrgTotalEmployee)
    
    #State, Loc, MajorGroup, NTEE, HOSP, UNIV have no errors to worry about
    
    #if not a specialty org, then make NTEE.CC = NA
    #this will make dat-filtering-hard not filter on NTEE.CC 
    if(input$OrgNTEECC == "00"){org.temp$NTEE.CC <- NA}

    
    #Safe Guards for Errors 
    if(is.null(org.temp$TotalExpense)){org.temp$TotalExpense <- 0}
    if(is.null(org.temp$TotalEmployee)){org.temp$TotalEmployee <- 0}
    if(is.null(org.temp$NTEE.CC)){org.temp$NTEE.CC <- NA}
    

    #return
    org.temp
    
  })
  
  output$test1 <- renderText({
    paste((org()))
  })

  ####### Getting Search Criteria ###########

  search <- reactive({
    search.temp <- list(UNIV = input$SearchUniv,
                        HOSP = input$SearchHosp)
    
    ## location type choices
    if(input$LocType == 1){ #if location search == state
      search.temp$State <- input$SearchState
      search.temp$Loc <- NA
    }else if(input$LocType == 2){ #if location search == city type
      search.temp$State <- NA
      search.temp$Loc <- input$SearchLoc
    }
    
    ## MajorGroup/NTEE/NTEE-CC choices
    if(input$SearchType == 1){ #if major group selected
      search.temp$MajorGroup <- input$SearchMajorGroup
      search.temp$NTEE <- NA
      search.temp$NTEE.CC <- NA
    }else if(input$SearchType == 2){ #if NTEE selected
      search.temp$MajorGroup <- NA
      search.temp$NTEE <- input$SearchNTEE
      search.temp$NTEE.CC <- NA
    }else if(input$SearchType == 3){ #if Common Code Selected 
      search.temp$MajorGroup <- NA
      search.temp$NTEE <- NA
      
      #get all letter and NTEE CC options
      # if NTEE is selected, get those letteres, otherwise use all letters
      
      # if(input$FurtherNTEE){ 
      #   let <- input$SearchNTEE2
      # }else{
      #     let <- base::LETTERS
      #   }
      # 
      
      let <- base::LETTERS
      cc <- input$SearchNTEECC
      
      search.temp$NTEE.CC <- tidyr::expand_grid(let, cc) %>%
                              tidyr::unite("combo", c(let, cc),
                                           remove = T,
                                           sep = "") %>%
                              unlist() %>%
                              unname()
      
    } #end ntee.cc if 
    
    #total Expenses
    if(input$TotalExpenseDecide == TRUE){
      search.temp$TotalExpense <- c(input$SearchTotalExpensesMin, input$SearchTotalExpensesMax)
    }else{
      search.temp$TotalExpense <- c(0 , Inf)
    }
    
    #total Expenses
    if(input$TotalEmployeeDecide == TRUE){
      search.temp$TotalEmployee <-c(input$SearchTotalEmployeeMin, input$SearchTotalEmployeeMax)
    }else{
      search.temp$TotalEmployee <- c(0 , Inf)
    }
    
    #just as a safe guard 
    #if anything is null, fill it with NA
    if(is.null(search.temp$UNIV)){search.temp$UNIV <- NA}
    if(is.null(search.temp$HOSP)){search.temp$HOSP <- NA}
    if(is.null(search.temp$State)){search.temp$State <- NA}
    if(is.null(search.temp$Loc)){search.temp$Loc <- NA}
    if(is.null(search.temp$MajorGroup)){search.temp$MajorGroup <- NA}
    if(is.null(search.temp$NTEE)){search.temp$NTEE <- NA}
    if(is.null(search.temp$NTEE.CC)){search.temp$NTEE.CC <- NA}
    if(is.null(search.temp$TotalExpense)){search.temp$TotalExpense <- c(0, Inf)}
    if(is.null(search.temp$TotalEmployee)){search.temp$TotalEmployee <- c(0, Inf)}

    #return
    search.temp
  })
  
  
  output$test2 <- renderPrint({
    paste(c(names(search()),search()))
  })
  
  
  ##### Get Hard Criteria ######
  
  hard <- reactive({
    hard.temp <- list()
    
    ## location type choices
    if(input$LocType == 1){ #if location hard == state
      hard.temp$State <- input$HardState
      hard.temp$Loc <- FALSE
    }else if(input$LocType == 2){ #if location hard == city type
      hard.temp$State <- FALSE
      hard.temp$Loc <- input$HardLoc
    }

    ## MajorGroup/NTEE/NTEE-CC choices
    if(input$SearchType == 1){ #if major group selected
      hard.temp$MajorGroup <- input$HardMajorGroup
      hard.temp$NTEE <- FALSE
      hard.temp$NTEE.CC <- FALSE
    }else if(input$SearchType == 2){ #if NTEE selected
      hard.temp$MajorGroup <- FALSE
      hard.temp$NTEE <- input$HardNTEE
      hard.temp$NTEE.CC <- FALSE
    }else if(input$SearchType == 3){ #if Common Code Selected
      hard.temp$MajorGroup <- FALSE
      hard.temp$NTEE <- FALSE
      hard.temp$NTEE.CC <- input$HardNTEECC
      # if(input$FurtherNTEE == TRUE){
      #   hard.temp$NTEE <- input$HardNTEE2
      # }
    }

    #total Expenses
    if(input$TotalExpenseDecide == TRUE){
      hard.temp$TotalExpense <- input$HardTotalExpense
    }else{
      hard.temp$TotalExpense <- FALSE
    }

    #total Employee
    if(input$TotalEmployeeDecide == TRUE){
      hard.temp$TotalEmployee <- input$HardTotalEmployee
    }else{
      hard.temp$TotalEmployee <- FALSE
    }

    #just as a safe guard
    #if anything is null, fill it with Na
    if(is.null(hard.temp$State)){hard.temp$State <- FALSE}
    if(is.null(hard.temp$Loc)){hard.temp$Loc <- FALSE}
    if(is.null(hard.temp$MajorGroup)){hard.temp$MajorGroup <- FALSE}
    if(is.null(hard.temp$NTEE)){hard.temp$NTEE <- FALSE}
    if(is.null(hard.temp$NTEE.CC)){hard.temp$NTEE.CC <- FALSE}
    if(is.null(hard.temp$TotalExpense)){hard.temp$TotalExpense <- FALSE}
    if(is.null(hard.temp$TotalEmployee)){hard.temp$TotalEmployee <- FALSE}

    hard.temp
  })
  
  
  output$test3 <- renderText({
    paste(c(names(hard()),hard()))
  })
  


  ######  Get filtered data #####
  dat.filtered.pre <- reactive({
    sc <- search()
    h <- hard()
    
    dat_filtering_hard(search.criteria = sc, 
                       hard.criteria = h)
  })
  
  ############ Getting the selection of rows. Currently this breaks the code 
  
  # #PRE SELECTION OF ROWS
  # dat.filtered.pre.size <- reactive({
  #   dim(dat.filtered.pre())[1]
  # })
  # 
  
  #number of elements in dat filtered pre selection of rows
  # output$dat.filtered.pre.size <- renderUI({
  #   
  #   n.text <- paste("There are", "<b>",dat.filtered.pre.size(),"</b>", "unique orginizations in your comparison set.")
  #   HTML(paste(n.text))
  # })
  # 

  # 
  #PRE SELECTION OF ROWS output
  output$dat.filtered.pre.table <- DT::renderDataTable({
    dat.filtered.pre() 
    # %>%
    #   select(-c(Gender, CEOCompensation))
  }, 
  extensions = 'Buttons',
  options = list(
    paging = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))


  # #get selected rows 
  # rows.selected <- reactive({
  #   input$dat.filtered.pre.table_rows_selected
  # })
  # 
  # output$rows <- renderPrint({
  #   rows.selected()
  # })
  
  #if user wants to select rows, then only include selected rows 
  #if user does not want to select rows, use the entire pre selected data set
  # dat.filtered.post <- reactive({
  #   if(input$TablePickerDecide == TRUE){
  #     ret <- dat.filtered.pre()[rows.selected(), ]
  #   }else{
  #     ret <- dat.filtered.pre
  #   }
  #   ret
  # })
  # 
  # output$dat.filtered.post <- DT::renderDataTable({
  #   dat.filtered.post()
  # })
  # 

  
  #POST SELECTION OF ROWS
  
  # dat.filtered.pre.table
  

  # 
  # ### Do the search
  # dat.similar <- reactive({
  #   HEOM_with_weights(org = org(), dat.filtered = dat.filtered())
  # })
  # #
  # # # output$similar <- DT::renderDataTable({
  # #    head(dat.filtered(), n=100)
  # # #   #dat.similar()
  # # # }, rownames = FALSE)
  # 
  # # output$similar <- renderTable({
  # #   head(dat.filtered(), n=100)
  # # })
  # 
  # output$ceo.suggest <- renderText({
  #   paste(search())
  #   #paste("Your suggested compensation is ", median(dat.similar()$CEOCompensation))
  # })
  # 
  # 
  # 
  
  
  ##### Getting Results########
  
  #see pretend-shiny vignette for explanation
  
  #get the distance
  # 
  # dat.dist <- reactive({
  #   HEOM_with_weights(org(), search(), dat.filtered.pre())
  # 
  # })

  
  
  # ## weighted average 
  # weighted.average <- reactive({
  # 
  #   d_i <- dat.dist()$dist
  #   sum_w <- sum(1 - d_i)
  #   # w_i <- (1-d_i) / sum_w
  #   #sum(w_i) = 1
  # 
  #   # sum(w_i * dat.filtered.pre()$CEOCompensation)
  # })
  # # 
  # #print weighted average
  # output$weighted.average <- renderText({
  # 
  #   paste((weighted.average()))
  #   #t <- paste("Your suggested CEO pay is", "<b>", weighted.average()[1], "-", weighted.average()[2])
  # })
  # 
  # 
  # output$final.table <-  DT::renderDataTable({
  #   dat.dist()
  # })

  # 
  # output$dat.filtered.pre.size <- ({
  #   #   
  #   #   n.text <- paste("There are", "<b>",dat.filtered.pre.size(),"</b>", "unique orginizations in your comparison set.")
  #   #   HTML(paste(n.text))
  #   # })
  #   # 

  
  #### Download report ##### 
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(dat.final = dat.filtered.pre(),
                     org = org(),
                     search = search(),
                     hard = hard())
      
      #loading screen
      showModal(modalDialog("Downloading...", footer=NULL))
      on.exit(removeModal())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  
  
  #########################################
  ### Gender Pay gap difference in difference model
  #######################################




  #########################################
  ### Gender Pay Gap Graphs Page
  #######################################

  #Format the Search Criteria
  gender.graph.filters <- reactive({
    filter.gender.1 <- list(form.year = 2019, #input$filter.year, 
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
      dplyr::filter(Gender != "U") %>%
      dplyr::select(CEOCompensation, Gender, paste(y.axis)) %>%
      dplyr::rename(Yaxis = paste(y.axis)) %>%
      dplyr::group_by_at(vars(-CEOCompensation)) %>%
      dplyr::summarise(Value =  ifelse(s == "Median", median(CEOCompensation), mean(CEOCompensation)), n = n(), .groups = "keep") %>%
      dplyr::ungroup()

    #rename major groups to something readable
    if(y.axis == "MajorGroup"){
      dat.plot <- dat.plot %>%
        dplyr::mutate(Yaxis = case_when(Yaxis == 1 ~ "Arts, Culture, and Humanities",
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
      dplyr::mutate(diff = abs(Value_F - Value_M)) %>%
      dplyr::mutate(pois = (Value_F + Value_M) / 2) %>%
      dplyr::select(c(Yaxis, pois, diff)) %>%
      base::merge(dat.plot)

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
                                                   y.axis == "NTEE.CC" ~ "NTEE-CC Code"),
                    "in 2019")) +
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



} #end function



