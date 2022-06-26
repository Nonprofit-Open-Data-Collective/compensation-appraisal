function(input, output, session) {
 
    output$tab1 <- renderTable({
      head(dat)
    })
    
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



