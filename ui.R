navbarPage("CEO Compensation Tool",

           
##################################################
  ### 1st Page under header CEO Compensation
##################################################
  tabPanel("CEO Compensation",
           fluidPage(theme = shinytheme("flatly")),
          tags$head(tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
          
          ### Side Bar on CEO Compensation page
          pageWithSidebar(
            headerPanel('Apply Orginization Filters'),
            sidebarPanel(width = 4,
                         tabsetPanel(type = "tabs",
                                     ### Input Organization Characteristics
                                     tabPanel("You Organization", 
                                              "Input your orginizations characteristics.",
                                              
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
                                                          choices = c("Arts, Culture, and Humanities" = 1,
                                                                      "Education" = 2,
                                                                      "Environment and Animals" = 3,
                                                                      "Health" = 4,
                                                                      "Human Services" = 5,
                                                                      "International, Foreign Affairs" = 6,
                                                                      "Public, Societal Benefit" = 7,
                                                                      "Religion Related" = 8,
                                                                      "Mutual/Membership Benefit" = 9, 
                                                                      "Unknown/Unclassified"= 10)),
                                              #NEED TO ADD NTEE
                                              "NEED TO ADD LINK TO NTEE AND NETT.CC CODES",
                                              selectInput("org.NTEE", label = "NTEE Code",
                                                          choices = LETTERS),
                                              #NEED TO ADD NTEE.CC
                                              textInput("org.NTEE.CC", label = "NTEE-CC Code", 
                                                        value = "A01"),
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
                                                           value = 1000000),
                                              # org.TotalEmployee
                                              numericInput("org.TotalEmployee", label = "Full Time Employees", 
                                                           value = 25,
                                                           min = 0),  
                                              #submitButton("Submit")
                                     ),
                                                          
                                     ### Comparison Filters
                                     tabPanel("Comparison Filters", "What types of orginizations to you want to compare to?",
                                              
                                              #search.FormYr
                                              pickerInput(
                                                inputId = "search.FormYr",
                                                label = "IRS Filing Year",
                                                choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
                                                                         "2012" = 2012, "2013" = 2013, "2014" = 2014,
                                                                         "2015" = 2015, "2016" = 2016, "2017" = "2017",
                                                                         "2018" = 2018, "2019" = 2019),
                                                multiple = TRUE,
                                                selected = 2009:2019,
                                                options = list(
                                                  `actions-box` = TRUE,
                                                  `deselect-all-text` = "None",
                                                  `select-all-text` = "Yeah, all!",
                                                  `none-selected-text` = "NA"
                                                )
                                              ),
                                              # search.FormType
                                              checkboxGroupInput("search.FormType", label = "IRS Form",
                                                          choices = list("990" = "990", "990EZ" = "990EZ")),
                                              # search.state
                                              pickerInput(
                                                inputId = "search.State",
                                                label = "States",
                                                choices = state.abb,  
                                                multiple = TRUE,
                                                selected = state.abb,
                                                options = list(
                                                  `actions-box` = TRUE,
                                                  `deselect-all-text` = "None",
                                                  `select-all-text` = "Yeah, all!",
                                                  `none-selected-text` = "NA"
                                                )
                                              ),
                                              # search.MajorGroup
                                              pickerInput(
                                                inputId = "search.MajorGroup",
                                                label = "Major Group",
                                                choices = c("Arts, Culture, and Humanities" = 1,
                                                            "Education" = 2,
                                                            "Environment and Animals" = 3,
                                                            "Health" = 4,
                                                            "Human Services" = 5,
                                                            "International, Foreign Affairs" = 6,
                                                            "Public, Societal Benefit" = 7,
                                                            "Religion Related" = 8,
                                                            "Mutual/Membership Benefit" = 9, 
                                                            "Unknown/Unclassified"= 10), #showing up as 1:10 and idk why 
                                                multiple = TRUE,
                                                selected = 1:10, 
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
                                                           choices = list("No, I do not want to compare with hospitals." = 2,
                                                                          "Yes, I want to compare to both hospitals and non-hospitals." = NA,
                                                                          "Yes, but I only want to compare to hospitals." = 1), 
                                                           selected = 2),
                                              # select.UNIV
                                              # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
                                              radioButtons("search.UNIV", label = "Do you want to compare to Orginizations that are universities?",
                                                           choices = list("No, I do not want to compare with universities." = 2,
                                                                          "Yes, I want to compare to both universities and non-universities." = NA,
                                                                          "Yes, but I only want to compare to universities." = 1), 
                                                           selected = 2),
                                              # search.TotalExpenses
                                              numericRangeInput("search.TotalExpenses", "Range of Total Expenses",
                                                                value = c(-Inf, Inf) #can we get commas on these values? 
                                                                ),
                                              # search.TotalEmployees
                                              numericRangeInput("search.TotalEmployees", "Range of Total Employees",
                                                                value = c(0, Inf), #can we get commas on these values? 
                                                                min = 0),
                                              #submitButton("Update filters"),
                                              
                                     )
                          ), # end internal tabsetPanel 

             
            ), #end sidebarPanel
            
    
              mainPanel(
                column(12,
                       submitButton("Update"),
                       ### Internal tabsetPanel for suggested compensation 
                       tabsetPanel(type = "tabs",
                                      tabPanel("Instructions",
                                               "Here is where we will put the user instructions for this tool", 
                                               "First fill our your orginizations characteristics, make sure to add ntee code link, make sure we add something about full time vs full time equivalent employees",
                                               "Then fill our your search critera.",
                                               "We will give a suggested pay range.",
                                               "And output the top 10 orgs closest to you",
                                               "We also have more detailed information about the model.",
                                               "Add something about the inflation adjustment."),
                                      tabPanel("Suggested Compensation",  
                                               textOutput("ceo.suggest"),
                                               "This is where we will put the suggested compensation."),
                                      tabPanel("Similar Orginizations", 
                                               DT::dataTableOutput("similar"),
                                               "This is where we will put our list of similar orgs. "),
                                      tabPanel("Model", "This is where we will put more detailed information about the model specifications.")
                       ), # end internal tabsetPanel 
                       "Text here will be on every page."
                 ) #end column
               ) #end mainPanel

          ) #end pageWithSidebar
    
        ), #end CEO Compensation tabPanel

##################################################  
  ### Gender Pay gap difference in diference model tabPanel
##################################################

  tabPanel("Gender Transitional Pay Gap Model ",
            "This is where we will put the difference in difference model for gender transitional pay gap. "
  ), #end tabPanel for Gender transitional pay gap

##################################################   
  ###  Gender Pay Differences tabPanel
##################################################

  tabPanel("Gender Pay Gap", 
           pageWithSidebar(
             headerPanel('Gender Pay Gap'),
             sidebarPanel(width = 4,
                          h3("Filters"), 
                          "What types of orginizations to you want to look at?",
                          tabsetPanel(type = "tabs", 
                                      tabPanel("Filter Criteria", 
                                               #search.FormYr
                                               pickerInput(
                                                 inputId = "filter.gender.FormYr",
                                                 label = "IRS Filing Year",
                                                 choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
                                                                "2012" = 2012, "2013" = 2013, "2014" = 2014,
                                                                "2015" = 2015, "2016" = 2016, "2017" = 2017,
                                                                "2018" = 2018, "2019" = 2019),
                                                 multiple = TRUE,
                                                 selected = 2009:2019,
                                                 options = list(
                                                   `actions-box` = TRUE,
                                                   `deselect-all-text` = "None",
                                                   `select-all-text` = "Yeah, all!",
                                                   `none-selected-text` = "NA"
                                                 )
                                               )%>% 
                                                 helper(type = "inline",
                                                        title = "Inline Help",
                                                        content = c("This helpfile is defined entirely in the UI!",
                                                                    "This is on a new line.",
                                                                    "This is some <b>HTML</b>."),
                                                        size = "s"),
                                               
                                               # search.state
                                               pickerInput(
                                                 inputId = "filter.gender.State",
                                                 label = "States",
                                                 choices = state.abb,  
                                                 multiple = TRUE,
                                                 selected = state.abb, 
                                                 options = list(
                                                   `actions-box` = TRUE,
                                                   `deselect-all-text` = "None",
                                                   `select-all-text` = "Yeah, all!",
                                                   `none-selected-text` = "NA"
                                                 )
                                               ),
                                               # search.MajorGroup
                                               pickerInput(
                                                 inputId = "filter.gender.MajorGroup",
                                                 label = "Major Group",
                                                 choices = c("Arts, Culture, and Humanities" = 1,
                                                             "Education" = 2,
                                                             "Environment and Animals" = 3,
                                                             "Health" = 4,
                                                             "Human Services" = 5,
                                                             "International, Foreign Affairs" = 6,
                                                             "Public, Societal Benefit" = 7,
                                                             "Religion Related" = 8,
                                                             "Mutual/Membership Benefit" = 9, 
                                                             "Unknown/Unclassified"= 10), #showing up as 1:10 and idk why 
                                                 multiple = TRUE,
                                                 selected = 1:10,
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
                                               radioButtons("filter.gender.HOSP", label = "Do you want to compare to Orginizations that are hospitals?",
                                                            choices = list("No, I do not want to compare with hospitals." = 2,
                                                                           "Yes, I want to include both hospitals and non-hospitals." = NA,
                                                                           "Yes, but I only want to include hospitals." = 1), 
                                                            selected = NA),
                                               # select.UNIV
                                               # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
                                               radioButtons("filter.gender.UNIV", label = "Do you want to compare to Orginizations that are universities?",
                                                            choices = list("No, I do not want to compare with universities." = 2,
                                                                           "Yes, I want to include both universities and non-universities." = NA,
                                                                           "Yes, but I only want to include universities." = 1), 
                                                            selected = NA),
                                               # search.TotalExpenses
                                               numericRangeInput("filter.gender.TotalExpenses", "Range of Total Expenses",
                                                                 value = c(-Inf, Inf) #can we get commas on these values? 
                                               ),
                                               # search.TotalEmployees
                                               numericRangeInput("filter.gender.TotalEmployees", "Range of Total Employees",
                                                                 value = c(0, Inf), #can we get commas on these values? 
                                                                 min = 0),
                                               #submitButton("Update filters"),
                                            ),
                                      tabPanel("Graph Formatting Options", 
                                               #yaxis options
                                               radioButtons(
                                                 inputId = "filter.gender.graph.yaxis",
                                                 label = "Y-axis", 
                                                 choices = list( "Major Group" = "MajorGroup",
                                                                 "NTEE Code" = "NTEE", 
                                                                 "NTEE- CC Code" = "NTEE.CC")
                                               ), 
                                               #method option
                                               radioButtons(
                                                 inputId = "filter.gender.graphs.method",
                                                 label = "Statistics", 
                                                 choices = list("Mean" = "mean",
                                                                "Median"= "median")
                                                            ))
                                      )#end tabsetPanel
             ), #end sidebarPanel
             
             
             mainPanel(
               column(12,
                      ### Internal tabsetPanel for Gender pay differences
                      # end internal tabsetPanel 
                      submitButton("Update Graph"),
                      "Text here will be on every page.",
                      plotlyOutput("gender.pay.graph"),
                      textOutput("test"),
               ) #end column
             ) #end mainPanel
             
           ),
           
  ), #end Gender Pay Differences tabPanel

##################################################
  ###  Data tabPanel
##################################################
  tabPanel("Data",
           "This is where we will put links to our data."
  ), #end Gender Pay Differences tabPanel

##################################################  
  ###  Contact tabPanel
##################################################
  tabPanel("Contact",
           "This is where we will put our contact information."
  ) #end Gender Pay Differences tabPanel
  
)#end navbarPage
  
  
  
  
