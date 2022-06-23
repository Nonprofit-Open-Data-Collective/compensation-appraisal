navbarPage("CEO Compensation Tool",
  
  ### 1st Page under header CEO Compensation
  tabPanel("CEO Compensation",
           fluidPage(theme = shinytheme("flatly")),
          tags$head(tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
          
          ### Side Bar on Main Page
          pageWithSidebar(
            headerPanel('Apply Orginization Filters'),
            sidebarPanel(width = 4,
              selectInput('player', 'Choose a player:',paste(data$player,"-",data$team)),
              sliderInput("overall", "Overall:",
                          min = 50, max = 100,
                          value = c(50,100)),
              sliderInput("height", "Height (cm):",
                          min = 155, max = 203,
                          value = c(155,203)),
              checkboxGroupInput(inputId = "position",
                                 label = 'Position:', choices = c("GK" = "GK", "CB" = "CB",
                                                                  "RB"="RB","LB"="LB","DMF"="DMF",
                                                                  "CMF"="CMF","AMF"="AMF",
                                                                  "RMF"="RMF","LMF"="LMF",
                                                                  "RWF"="RWF","LWF"="LWF",
                                                                  "SS"="SS","CF"="CF"), 
                                 selected = c("CF"="CF"),inline=TRUE),
              checkboxGroupInput(inputId = "foot",
                                 label = 'Foot:', choices = c("Right foot" = "Right foot",
                                                                  "Left foot" = "Left foot"), 
                                 selected = c("Right foot" = "Right foot",
                                              "Left foot" = "Left foot"),inline=TRUE),
             submitButton("Update filters")
            ), #end sidebarPanel
            
    
              mainPanel(
                column(8,
                       ### Internal tabsetPanel for suggested compensation 
                       tabsetPanel(type = "tabs",
                                      tabPanel("Suggested Compensation",tableOutput("tab1"), "This is where we will put the suggested compensation."),
                                      tabPanel("Similar Orginizations", "This is where we will put our list of similar orgs. "),
                                      tabPanel("Model", "This is where we will put more detailed information about the model specifications.")
                       ), # end internal tabsetPanel 
                       "Text here will be on every page."
                 ) #end column
               ) #end mainPanel

          ) #end pageWithSidebar
    
        ), #end CEO Compensation tabPanel
  
  ### Gender Pay gap difference in diference model tabPanel  
  tabPanel("Gender Transitional Pay Gap Model ",
            "This is where we will put the difference in difference model for gender transitional pay gap. "
  ), #end tabPanel for Gender transitional pay gap
   
  ###  Gender Pay Differences tabPanel
  tabPanel("Gender Pay Differences",
           "This is where we will put the graphs for current trends in gender pay. "
  ), #end Gender Pay Differences tabPanel
  
  ###  Data tabPanel
  tabPanel("Data",
           "This is where we will put links to our data."
  ), #end Gender Pay Differences tabPanel
  
  ###  Contact tabPanel
  tabPanel("Contact",
           "This is where we will put our contact information."
  ) #end Gender Pay Differences tabPanel
  
)#end navbarPage
  
  
  
  
