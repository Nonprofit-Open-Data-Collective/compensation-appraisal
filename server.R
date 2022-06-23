function(input, output, session) {
 
    output$tab1 <- renderTable({
      head(dat)
    })

} #end function



