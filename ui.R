navbarPage("CEO Compensation Tool",
  
  ### 1st Page under header CEO Compensation
  tabPanel("CEO Compensation",
           fluidPage(theme = shinytheme("flatly")),
          tags$head(tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
          
          ### Side Bar on CEO Compensation page
          pageWithSidebar(
            headerPanel('Apply Orginization Filters'),
            sidebarPanel(width = 4,
                         tabsetPanel(type = "tabs",
                                     ### Input Orginziation Characteristics
                                     tabPanel("You Orginizaition", 
                                              "Input your orginizations characteristics.",
                                              submitButton("Submit"),
                                               # org.FormYr
                                               selectInput("org.FormYr", label = "IRS Filing Year", 
                                                           choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
                                                                          "2012" = 2012, "2013" = 2013, "2014" = 2014, 
                                                                          "2015" = 2015, "2016" = 2016, "2017" = "2017", 
                                                                          "2018" = 2018, "2019" = 2019),
                                                           selected = 2019),
                                              # org.FormType
                                              selectInput("org.FormType", label = "What type of IRS Form did you file?",
                                                          choices = list("990" = "990", "990EZ" = "990EZ")),
                                              # org.State
                                              selectInput("org.State", label = "State",
                                                          choices = state.abb), 
                                              # org.MajorGroup
                                              selectInput("org.MajorGroup", label = "Major Group",
                                                          choices = as.roman(1:10)),
                                              #NEED TO ADD NTEE
                                              #NEED TO ADD NTEE.CC
                                              # org.Hosp 
                                              "Are you a Hospital?",
                                              switchInput("org.HOSP", label = NA,
                                                          value = FALSE,
                                                          onLabel = "Yes",
                                                          offLabel = "No"),
                                              # org.Univ 
                                              "Are you a University?",
                                              switchInput("org.UNIV", label = NA,
                                                          value = FALSE,
                                                          onLabel = "Yes",
                                                          offLabel = "No"),
                                              # org.TotalExpense
                                              numericInput("org.TotalExpense", label = "Anual Expenses", 
                                                           value = 50000),
                                              # org.TotalEmployee
                                              numericInput("org.TotalEmployee", label = "Full Time Employees", 
                                                           value = 25,
                                                           min = 0),    
                                     ),
                                                          
                                     ### Comparison Filters
                                     tabPanel("Comparison Filters", "What types of orginizations to you want to compare to?",
                                              submitButton("Update filters"),
                                              #search.FormYr
                                              checkboxGroupInput("org.FormYr", label = h3("IRS Filing Year"),
                                                                 choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
                                                                                "2012" = 2012, "2013" = 2013, "2014" = 2014,
                                                                                "2015" = 2015, "2016" = 2016, "2017" = "2017",
                                                                                "2018" = 2018, "2019" = 2019)),
                                              # search.FormType
                                              checkboxGroupInput("org.FormType", label = "IRS Form",
                                                          choices = list("990" = "990", "990EZ" = "990EZ")),
                                              # search.MajorGroup
                                              pickerInput(
                                                inputId = "search.MajorGroup",
                                                label = "Major Group",
                                                choices = as.roman(1:10), #showing up as 1:10 and idk why 
                                                multiple = TRUE,
                                                options = list(
                                                  `actions-box` = TRUE,
                                                  `deselect-all-text` = "None",
                                                  `select-all-text` = "Yeah, all!",
                                                  `none-selected-text` = "NA"
                                                )
                                              ),
                                              #NEED TO ADD NTEE
                                              #NEED TO ADD NTEE.CC both NTEE and NTEE.CC need to be reactive to inside Major Group
                                              # select.HOSP
                                              # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
                                              radioButtons("search.HOSP", label = "Do you want to compare to Orginizations that are hospitals?",
                                                           choices = list("No, I do not want to compare with hospitals." = 1,
                                                                          "Yes, I want to compare to both hospitals and non-hospitals." = NA,
                                                                          "Yes, but I only want to compare to hospitals" = 2), 
                                                           selected = 1),
                                              # select.UNIV
                                              # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
                                              radioButtons("search.UNIV", label = "Do you want to compare to Orginizations that are universities?",
                                                           choices = list("No, I do not want to compare with universities." = 1,
                                                                          "Yes, I want to compare to both universities and non-universities." = NA,
                                                                          "Yes, but I only want to compare to universities" = 2), 
                                                           selected = 1),
                                              # search.TotalExpenses
                                              numericRangeInput("search.TotalExpenses", "Range of Total Expenses",
                                                                value = c(0, 1e7) #can we get commas on these values? 
                                                                ),
                                              # search.TotalEmployees
                                              numericRangeInput("search.TotalEmployees", "Range of Total Employees",
                                                                value = c(0, 500), #can we get commas on these values? 
                                                                min = 0)
                                              
                                     )
                          ), # end internal tabsetPanel 

             
            ), #end sidebarPanel
            
    
              mainPanel(
                column(8,
                       ### Internal tabsetPanel for suggested compensation 
                       tabsetPanel(type = "tabs",
                                      tabPanel("Suggested Compensation", 
                                               tableOutput("tab1"), 
                                               "This is where we will put the suggested compensation."),
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
           checkboxGroupInput("major.group.choice", "Choose", choices = c("I" = "I","II" = "II", "III" = "III",
                                                       "IV" = "IV", "V" = "V", "VI" = "VI",
                                                       "VII" = "VII", "VIII" = "VIII", 
                                                       "IX" = "IX", "X"= "X"), selected = NULL),
           submitButton("Update filters"),
           textOutput("message"),
           #plotlyOutput("graph.gender"), 
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
  
  
  
  
