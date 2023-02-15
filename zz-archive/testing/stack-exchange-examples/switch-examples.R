ui <- fluidPage(
  tags$h3("Material switch examples"),
  
  materialSwitch(inputId = "switch1", label = "Night mode",
                 ),
  verbatimTextOutput("value1"),
  
  materialSwitch(inputId = "switch2", label = "Night mode", status = "danger"),
  verbatimTextOutput("value2")
)
server <- function(input, output) {
  
  output$value1 <- renderText({ input$switch1 })
  
  output$value2 <- renderText({ input$switch2 })
  
}
shinyApp(ui, server)
}
