library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$h2("Sweet Alert examples"),
  actionButton(
    inputId = "success",
    label = "Launch a success sweet alert",
    icon = icon("check")
  ),
  actionButton(
    inputId = "error",
    label = "Launch an error sweet alert",
    icon = icon("remove")
  ),
  actionButton(
    inputId = "sw_html",
    label = "Sweet alert with HTML",
    icon = icon("thumbs-up")
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$success, {
    show_alert(
      title = "Success !!",
      text = "All in order",
      type = "success"
    )
  })
  
  observeEvent(input$error, {
    show_alert(
      title = "Error !!",
      text = "It's broken...",
      type = "error"
    )
  })
  
  observeEvent(input$sw_html, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("With HTML tags",
                style = "color: steelblue;"),
        "In", tags$b("bold"), "and", tags$em("italic"),
        tags$br(),
        "and",
        tags$br(),
        "line",
        tags$br(),
        "breaks",
        tags$br(),
        "and an icon", icon("thumbs-up")
      ),
      html = TRUE
    )
  })
  
}

if (interactive())
  shinyApp(ui, server)

# Ouptut in alert ----

library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$h1("Click the button to open the alert"),
  actionButton(
    inputId = "sw_html",
    label = "Sweet alert with plot"
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$sw_html, {
    show_alert(
      title = "Yay a plot!",
      text = tags$div(
        plotOutput(outputId = "plot"),
        sliderInput(
          inputId = "clusters",
          label = "Number of clusters",
          min = 2, max = 6, value = 3, width = "100%"
        )
      ),
      html = TRUE,
      width = "80%"
    )
  })
  
  output$plot <- renderPlot({
    plot(Sepal.Width ~ Sepal.Length,
         data = iris, col = Species,
         pch = 20, cex = 2)
    points(kmeans(iris[, 1:2], input$clusters)$centers,
           pch = 4, cex = 4, lwd = 4)
  })
}


  shinyApp(ui, server)
