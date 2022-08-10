ui <- navbarPage( "Urban Institute",
  theme = bs_theme(
    bg = "#f5f5f5",
    fg = "#0a4c6a",
    primary = "#fdbf11",
    base_font = font_google("Prompt"),
    code_font = font_google("JetBrains Mono")
  ),

  shiny::tabPanel(
    "CEO Pay Appraisal",
    shinyglide::glide(
      controls_position = "top",

      shinyglide::screen( #Directions
        h4("Welcome to Urban Institutes CEO Pay Appraisal Tool!"), 
        wellPanel(
          
        HTML('
              This tool is designed for nonprofits to receive a CEO total compensation appraisal based on their organization’s characteristics as well as their job market of their prospective CEO. We use the latest IRS information about nonprofit CEO pay to compare your nonprofits to other nonprofits in U.S. and provide to you a suggested CEO pay range.
<br><br>
This tool has 3 steps: Input the nonprofit’s characteristics, creating a set of other nonprofits to compare to, and the resulting CEO pay appraisal. 
<br><br>
How it works:
<br><br>
<u>Step 1:</u> Tell us about your nonprofit: 
<br>
We ask you a series of questions about your particular nonprofit. We want to know about the work your nonprofit does, where you are located, how large you are, and a few other questions. 
<br><br>
<u>Step 2:</u> Defining your Comparison Set
<br>
We want to allow you, the user, to define your own job market for a potential new CEO hire. To do this, you will create a comparison set of other nonprofits who are similar to your own. We allow you to choose from a series of filters to create your own comparison set of other nonprofits. We have over 12,000 unique nonprofits for you add to your comparison set.
<br><br>
For example, if your nonprofit is a large museum, a job market for a CEO candidate might be other large museums, regardless of where they are located. 
<br><br>
But, if your nonprofit is a small agricultural nonprofit in rural Kansas, the job market for a CEO might be other small agricultural nonprofits in rural Kansas, Iowa, Nebraska, and Missouri. 
<br><br>
<u>Step 3:</u> The Results
<br>
Based on your organization’s characteristics given in Step 1 and the other nonprofits who match the filtering criteria defined in Step 2, we suggest a reasonable range of pay for a CEO.  
<br><br>
We also provide detailed information about the originations in the user-defined job market, such as the organization’s names, size, location, the CEO’s total compensation worth, and the CEO’s sex.
<br><br>
At the end, you will be able to download all of the information used to create you CEO suggested pay in a PDF report. You will also be able to download more detailed information about the organizations in your comparison set as a csv or a Excel document. 
<br><br>
             ') #end html
        ) #end well panel
      ), #end directions screen
      shinyglide::screen(# Tell us about your org
        h4("Step 1: Tell us about your nonprofit"),
        HTML(" 
We are going to ask you a series of questions about your particular nonprofit. We want to know about the work your nonprofit does, where you are located, how large you are, and other basic information. 
<br><br>

To categorize the work your nonprofit does, we use the National Taxonomy of Exempt Entities (NTEE) codes. The NTEE classification system divides the universe of nonprofit organizations into 26 major groups under 10 broad categories. <a href='https://nccs.urban.org/project/national-taxonomy-exempt-entities-ntee-codes'>Click here</a> to learn more about the NTEE system 
<br><br>

For size of your nonprofit, we will be asking you about your total annual expenses and the total employees the CEO would be overseeing. Both of these items are listed on IRS Form 990. [Click here]( https://www.irs.gov/charities-nonprofits/required-filing-form-990-series 
) to learn more about IRS 990 series. If you do not have access to your origination’s tax filing information, your best guess should suffice (note the final results will be less specific the more approximating is done in this step).


")
      ), #end tell us screen
      
      
      

      ### Org input
      shinyglide::screen(#org location screen
        h4("Where is your nonprofit located?"),
        wellPanel(
          selectInput("OrgState",
                      label = "What state is your organization located in?",
                      choices = sort(c(datasets::state.abb, "PR", "DC"))),
          # #org.loc
          selectInput("OrgLoc",
                      "What type of city is your organization located in?",
                      choices = c("Rural", "Metropolitan")),

        )#end WellPannel
      ), #end #org location screen

      shinyglide::screen( #org size screen
        #next_condition = "input.OrgTotalExpense >= 0 & input.OrgTotalEmployee >=0",
        h4("How large is your nonprofit?"),
        wellPanel(
          #org.TotalExpense
          autonumericInput(
            inputId = "OrgTotalExpense",
            label = "What are your organization's total annual expenses?",
            value = 500000,
            align = "left",
            currencySymbol = "$",
            currencySymbolPlacement = "p",
            decimalCharacter = ".",
            digitGroupSeparator = ",",
            decimalPlaces = 2)%>%
            shinyhelper::helper(
              type = "markdown",
              content = "AnnualExpenses",
              size = "l",
              buttonLabel = "Close"),

          #org.TotalEmployee
          autonumericInput(
            inputId = "OrgTotalEmployee",
            label = "How many people does your organization employ?",
            value = 25,
            align = "left",
            digitGroupSeparator = ",",
            decimalPlaces = 0,
            minimumValue = 0
          )%>%
            shinyhelper::helper(
              type = "markdown",
              content = "TotalEmployee",
              size = "l",
              buttonLabel = "Close"),
        )#end Well Pannel
      ), #end org size screen

      shinyglide::screen( #org type
        h4("What type of work does your nonprofit do?"),
        wellPanel(
          selectInput(
            "OrgMajorGroup",
            label = htmltools::HTML("What broad category best describes the work your organization does?"),
            choices = c("Arts, Culture, and Humanities" = 1,
                        "Education" = 2,
                        "Environment and Animals" = 3,
                        "Health" = 4,
                        "Human Services" = 5,
                        "International, Foreign Affairs" = 6,
                        "Public, Societal Benefit" = 7,
                        "Religion Related" = 8,
                        "Mutual/Membership Benefit" = 9,
                        "Unknown/Unclassified"= 10)) %>%
            shinyhelper::helper(
              type = "markdown",
              content = "BroadCategory",
              size = "l",
              buttonLabel = "Close"),

          #specify NTEE
          #groups 1, 2, 3, 6, 8, 9, 10 do not have further NTEE Divisions
          conditionalPanel( #if we are in group 3
            "input.OrgMajorGroup == 3 ",
            selectInput(
              "OrgNTEE3",
              label = htmltools::HTML("What major group best describes the work your organization does?"),
              choices = c(
                "Animal Related" = "D",
                "Environment" = "C"),
              selected = NA) %>%
              shinyhelper::helper(
                type = "markdown",
                content = "NTEE",
                size = "l",
                buttonLabel = "Close"),
          ), #end Conditional Panel


          conditionalPanel( #if we are in group 4
            "input.OrgMajorGroup == 4 ",
            selectInput(
              "OrgNTEE4",
              label = htmltools::HTML("What major group best describes the work your organization does?"),
              choices = c("Health Care" = "E",
                          "Medical Research" = "H",
                          "Mental Health & Crisis Intervention" = "F",
                          "Voluntary Health Associations & Medical Disciplines" = "G"),
              selected = NA) %>%
              shinyhelper::helper(
                type = "markdown",
                content = "NTEE",
                size = "l",
                buttonLabel = "Close"),
          ), #end Conditional Panel

          conditionalPanel( #if we are in group 5
            "input.OrgMajorGroup == 5 ",
            selectInput(
              "OrgNTEE5",
              label = htmltools::HTML("What major group best describes the work your organization does?"),
              choices = c("Crime & Legal-Related" = "I",
                          "Employment" = "J",
                          "Food, Agriculture & Nutrition" = "K",
                          "Housing & Shelter" = "L",
                          "Human Services" = "P",
                          "Public Safety, Disaster Preparedness & Relief" = "M",
                          "Recreation & Sports" = "N",
                          "Youth Development" = "O"),
              selected = NA) %>%
              shinyhelper::helper(
                type = "markdown",
                content = "NTEE",
                size = "l",
                buttonLabel = "Close"),
          ), #end Conditional Panel

          conditionalPanel( #if we are in group 7
            "input.OrgMajorGroup == 7 ",
            selectInput(
              "OrgNTEE7",
              label = htmltools::HTML("What major group best describes the work your organization does?"),
              choices = c("Civil Rights, Social Action & Advocacy" = "R",
                          "Community Improvement & Capacity Building" = "S",
                          "Philanthropy, Voluntarism & Grantmaking Foundations" = "T",
                          "Public & Societal Benefit" = "W",
                          "Science & Technology" = "U",
                          "Social Science" = "V"),
              selected = NA) %>%
              shinyhelper::helper(
                type = "markdown",
                content = "NTEE",
                size = "l",
                buttonLabel = "Close"),
          ), #end Conditional Panel

        )#end wellPannel
      ), #end org type screen

      shinyglide::screen( #org other questions
        h4("We have a few more questions about your nonprofit."),
        wellPanel(
          #org ntee-cc
          pickerInput(
            "OrgNTEECC",
            label = htmltools::HTML("Does your organization fit any of the following specialty descriptions?"),
            choices = c("Alliance/Advocacy Organization" = "01",
                        "Management and Technical Assistance" = "02",
                        "Professional Society/Association" = "03",
                        "Research Institute and/or Public Policy Analysis" = "05",
                        "Monetary Support - Single Organization" = "11",
                        "Monetary Support - Multiple Organizations" = "12",
                        "Nonmonetary Support Not Elsewhere Classified (N.E.C.)" = "19",
                        "I am a regular nonprofit. None of these specialties describe my organization." = "00"),
            selected = "00") %>%
            shinyhelper::helper(
              type = "markdown",
              content = "NTEECC",
              size = "l",
              buttonLabel = "Close"),

          # # org.Hosp
          "Is your organization a Hospital?",
          switchInput(
            "OrgHOSP",
            label = NA,
            value = FALSE,
            onLabel = "Yes",
            offLabel = "No"),

          # org.Univ
          "Is your organization a University?",
          switchInput(
            "OrgUNIV",
            label = NA,
            value = FALSE,
            onLabel = "Yes",
            offLabel = "No")

        ) #end WellPanel
      ), # end other questions screen

      # ### Testing screen - to be deleted
      # shinyglide::screen(
      #   "this is a test screen for making sure org inputed correctly.",
      #   textOutput("test1")
      # ),

      ### Comparison set
      shinyglide::screen( # comparison directions
        h4("Step 2: Defining your Comparison Set"), 
        wellPanel(
        HTML("

We want to allow you, the user, to define your own job market for a potential new CEO hire. To do this, you create a comparison set of other nonprofits who are similar to your own. We allow you to choose from a series of filters to create your own comparison set of other nonprofits.
<br><br>
We ask you a series of questions about organizations you want to compare your nonprofit to such as the work the other nonprofits do, type of nonprofit, and organization size. 
<br><br>
In the sections we allow you to decide if you want to apply hard or soft filtering to each organizational attribute we ask about. Hard filtering only includes organizations who match the criteria you specify. Soft matching includes all organizations regardless of your filtering selections, but will prioritize organizations who satisfy to soft matching criteria when we calculated your suggested pay range. For example, say you only want to include organizations in your comparison set whose total employees are less than 200. Then you would select hard filtering on the total employees attribute. If you want to prioritize organizations in your comparison set who have less that 200 employees, but also want to include organizations who have more than 200 employees, then you would select soft filtering on the total employees attribute. 
<br><br>
We again be using the NTEE classification system to categorize the type of work nonprofits do. 
<a href='https://nccs.urban.org/project/national-taxonomy-exempt-entities-ntee-codes'>Click here</a> to learn more about the NTEE classification system.
")
        )#end well Panel
      ), # end comparison directions

      shinyglide::screen( #search locations
        h4("How do you want to consider location?"),
        wellPanel(
          
          pickerInput(
            inputId = "LocType",
            label = "I want to search location type by... ",
            choices = c("State" = 1,
                        "City Type" = 2),
            selected = NA)%>%
            shinyhelper::helper(
              type = "markdown",
              content = "SearchLocType",
              size = "l",
              buttonLabel = "Close"),
          
          conditionalPanel(
            condition = "input.LocType == 1", 
            #search.state
            pickerInput(
              inputId = "SearchState", 
              label = "I want to include organizations who are located in the following states:", 
              choices = sort(c(state.abb, "PR", "DC")),
              multiple = TRUE,
              selected = sort(c(state.abb, "PR", "DC")),
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "None",
                `select-all-text` = "Select All",
                `none-selected-text` = "NA"
              )
            ),
            HTML("Do you want to hard or soft filtering on state?"),
            switchInput("HardState",
                        label = NA,
                        value = FALSE,
                        onLabel = "Hard",
                        offLabel = "Soft") %>%
              shinyhelper::helper(
                type = "markdown",
                content = "HardSoft",
                size = "l",
                buttonLabel = "Close")
          ),
          
          
          conditionalPanel(
            condition = "input.LocType == 2",
            #search.loc
            pickerInput(
              inputId = "SearchLoc", 
              label = "I want to include organizations who are located in the following types of cities:", 
              choices = c("Rural", "Metropolitan"),
              selected = c("Rural", "Metropolitan"),
              multiple = TRUE, 
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "None",
                `select-all-text` = "Select All",
                `none-selected-text` = "NA"
              )
            ),
            HTML("Do you want to hard or soft filtering on city types?"),
            switchInput("HardLoc",
                           label = NA,
                           value = FALSE,
                           onLabel = "Hard",
                           offLabel = "Soft")%>%
              shinyhelper::helper(
                type = "markdown",
                content = "HardSoft",
                size = "l",
                buttonLabel = "Close")
          ),
          
          #How many items in dat.filtered.pre
          #h5(htmlOutput("dat.filtered.pre.size"))
        )#end Well Panel
      ), #end search locations screen
      
      shinyglide::screen( # search types
        h4("How to do you want to filter by organization type?"),
        wellPanel(
          pickerInput(
            inputId = "SearchType",
            "I want to filter by ... ",
            choices = c("Broad Category (10 options)" = 1,
                        "Major Group (26 options)" = 2,
                        "Specialty Description (only select if your organization fits a specality description)" = "3"),
            selected = 1
          )%>%
            shinyhelper::helper(
              type = "markdown",
              content = "SearchOrgType",
              size = "l",
              buttonLabel = "Close"),
          
          # if broad category is selected
          conditionalPanel(
            condition = "input.SearchType == 1",
            pickerInput(
              inputId = "SearchMajorGroup",
              label = "I want to include organizations in the following broad categories:",
              choices = c("Arts, Culture, and Humanities" = 1,
                          "Education" = 2,
                          "Environment and Animals" = 3,
                          "Health" = 4,
                          "Human Services" = 5,
                          "International, Foreign Affairs" = 6,
                          "Public, Societal Benefit" = 7,
                          "Religion Related" = 8,
                          "Mutual/Membership Benefit" = 9,
                          "Unknown/Unclassified"= 10),
              multiple = TRUE,
              selected = 1:10,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "None",
                `select-all-text` = "Select All",
                `none-selected-text` = "NA")
            ), #end picker input
            "Do you want to hard or soft filtering on broad category?",
            switchInput(
              "HardMajorGroup",
              label = NA,
              value = FALSE,
              onLabel = "Hard",
              offLabel = "Soft") %>%
              shinyhelper::helper(
                type = "markdown",
                content = "HardSoft",
                size = "l",
                buttonLabel = "Close"),
            #How many items in dat.filtered.pre
           # h5(htmlOutput("dat.filtered.pre.size"))
          ),  #end conditional panel
          
          #if major group is selected
          conditionalPanel(
            condition = "input.SearchType == 2",
            pickerInput(
              inputId = "SearchNTEE",
              label = "I want to include organizations in the following major groups:",
              choices = c("Animals" = "D",
                          "Arts, Culture, and Humanities" = "A",
                          "Civil Rights, Social Action & Advocacy" = "R",
                          "Community Improvement & Capacity Building" = "S",
                          "Crime & Legal-Related" = "I",
                          "Education" = "B",
                          "Environment" = "C",
                          "Employment" = "J",
                          "Food, Agriculture & Nutrition" = "K",
                          "Health Care" = "E",
                          "Housing & Shelter" = "L",
                          "Human Services" = "P",
                          "International, Foreign Affairs & National Security" = "Q",
                          "Medical Research" = "H",
                          "Mental Health & Crisis Intervention" = "F",
                          "Mutual & Membership Benefit" = "Y",
                          "Philanthropy, Voluntarism & Grantmaking Foundations" = "T",
                          "Public Safety, Disaster Preparedness & Relief" = "M",
                          "Public & Societal Benefit" = "W",
                          "Voluntary Health Associations & Medical Disciplines" = "G",
                          "Recreation & Sports" = "N",
                          "Religion Related" = "X",
                          "Science & Technology" = "U",
                          "Social Science" = "V",
                          "Youth Development" = "O",
                          "Unknown, Unclassified" = "Z"),
              multiple = TRUE,
              selected = LETTERS,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "None",
                `select-all-text` = "Select All",
                `none-selected-text` = "NA")
            )  %>%#end picker input
              shinyhelper::helper(
                type = "markdown",
                content = "SearchNTEE",
                size = "l",
                buttonLabel = "Close"),
            "Do you want to hard or soft filtering on major group?",
            switchInput(
              "HardNTEE",
              label = NA,
              value = FALSE,
              onLabel = "Hard",
              offLabel = "Soft") %>%
              shinyhelper::helper(
                type = "markdown",
                content = "HardSoft",
                size = "l",
                buttonLabel = "Close")
          ), #end conditional panel
          
          #if common code is selected
          conditionalPanel(
            condition = "input.SearchType == 3",
            pickerInput(
              "SearchNTEECC",
              label = htmltools::HTML("Do you want to include orgnizations that fit any of the following specialty descriptions?"),
              choices = c("Alliance/Advocacy Organization" = "01",
                          "Management and Technical Assistance" = "02",
                          "Professional Society/Association" = "03",
                          "Research Institute and/or Public Policy Analysis" = "05",
                          "Monetary Support - Single Organization" = "11",
                          "Monetary Support - Multiple Organizations" = "12",
                          "Nonmonetary Support Not Elsewhere Classified (N.E.C.)" = "19"),
              multiple = TRUE,
              selected = c("01", "02", "03", "05", "11", "12", "19"),
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "None",
                `select-all-text` = "Select All",
                `none-selected-text` = "NA"))%>%
              shinyhelper::helper(
                type = "markdown",
                content = "SearchNTEECC",
                size = "l",
                buttonLabel = "Close"),
            "Do you want to hard or soft filtering on specialty description?",
            switchInput(
              "HardNTEECC",
              label = NA,
              value = FALSE,
              onLabel = "Hard",
              offLabel = "Soft") %>%
              shinyhelper::helper(
                type = "markdown",
                content = "HardSoft",
                size = "l",
                buttonLabel = "Close"),
            
            # HTML("Do you want to further search by major group?
            #                       <br>
            #                       We HIGHLY suggest that you do not further your search by major group.
            #                       If you select yes, it is likely your comparison set will be small and your results will be extremely limited."),
            # switchInput(
            #   inputId = "FurtherNTEE",
            #   label = NA,
            #   value = FALSE,
            #   onLabel = "Yes",
            #   offLabel = "No"),
            # 
            # #if they want to further filter by NTEE
            # conditionalPanel(
            #   condition = "input.FurtherNTEE == true",
            #   pickerInput(
            #     inputId = "SearchNTEE2",
            #     label = "I want to include organizations in the following major groups:",
            #     choices = c("Animals" = "D",
            #                 "Arts, Culture, and Humanities" = "A",
            #                 "Civil Rights, Social Action & Advocacy" = "R",
            #                 "Community Improvement & Capacity Building" = "S",
            #                 "Crime & Legal-Related" = "I",
            #                 "Education" = "B",
            #                 "Environment" = "C",
            #                 "Employment" = "J",
            #                 "Food, Agriculture & Nutrition" = "K",
            #                 "Health Care" = "E",
            #                 "Housing & Shelter" = "L",
            #                 "Human Services" = "P",
            #                 "International, Foreign Affairs & National Security" = "Q",
            #                 "Medical Research" = "H",
            #                 "Mental Health & Crisis Intervention" = "F",
            #                 "Mutual & Membership Benefit" = "Y",
            #                 "Philanthropy, Voluntarism & Grantmaking Foundations" = "T",
            #                 "Public Safety, Disaster Preparedness & Relief" = "M",
            #                 "Public & Societal Benefit" = "W",
            #                 "Voluntary Health Associations & Medical Disciplines" = "G",
            #                 "Recreation & Sports" = "N",
            #                 "Religion Related" = "X",
            #                 "Science & Technology" = "U",
            #                 "Social Science" = "V",
            #                 "Youth Development" = "O",
            #                 "Unknown, Unclassified" = "Z"),
            #     multiple = TRUE,
            #     selected = LETTERS,
            #     options = list(
            #       `actions-box` = TRUE,
            #       `deselect-all-text` = "None",
            #       `select-all-text` = "Select All",
            #       `none-selected-text` = "NA")
            #   ) %>%
            #     shinyhelper::helper(
            #       type = "markdown",
            #       content = "SearchNTEE",
            #       size = "l",
            #       buttonLabel = "Close"), #end picker input
            #   "Do you want to hard or soft matching on major group?",
            #   switchInput(
            #     "HardNTEE2",
            #     label = NA,
            #     value = FALSE,
            #     onLabel = "Hard",
            #     offLabel = "Soft")
            # ) #end conditional Panel
            # 
          ), #end conditional Panel
          
        )#end WellPannel
      ), #end search types screen
      

      shinyglide::screen( #search.hosp screen
        h4("Do you want to include hospitals or universities?"),
        wellPanel(
          pickerInput(
            inputId = "SearchHosp",
            label = "Do you want to include hospitals in your comparison set?",
            choices = list("No, I do not want include with hospitals." = 1,
                           "Yes, I want to include both hospitals and non-hospitals." = 2,
                           "Yes, I exclusively want to include hospitals." = 3),
            selected = 2),

          pickerInput(
            inputId = "SearchUniv",
            label = "Do you want to include universities in your comparison set?",
            choices = list("No, I do not want to include universities." = 1,
                           "Yes, I want to include both universities and non-universities." = 2,
                           "Yes, I exclusively want to include universities." = 3),
            selected = 2)
        )#end WellPanel
      ), #end search.hosp screen

      shinyglide::screen( #search.total expenses
        h4("Do you want to further filter your comparison set by annual total expenses?"),
        wellPanel(
          #search.totalexpenses
          switchInput(
            "TotalExpenseDecide",
            label = NA,
            value = FALSE,
            onLabel = "Yes",
            offLabel = "No"),
          
          
          
          conditionalPanel(
            condition = "input.TotalExpenseDecide == true",
            
            fluidRow(
              column(
                5,
                autonumericInput(
                  inputId = "SearchTotalExpensesMin",
                  label = "Minimum Expenses",
                  value = 0,
                  align = "right",
                  currencySymbol = "$",
                  currencySymbolPlacement = "p",
                  decimalCharacter = ".",
                  digitGroupSeparator = ",",
                  decimalPlaces = 2,
                  minimumValue = 0
                ),
              ), #end column
              column( 1,
                      HTML("<br>  <font size=\"+3\">-</font>")),
              column(
                5,
                autonumericInput(
                  inputId = "SearchTotalExpensesMax",
                  label = "Maximum Expenses",
                  value = 6000000000,
                  align = "right",
                  currencySymbol = "$",
                  currencySymbolPlacement = "p",
                  decimalCharacter = ".",
                  digitGroupSeparator = ",",
                  decimalPlaces = 2,
                  maximumValue = 6000000000
                )%>%
                  shinyhelper::helper(
                    type = "markdown",
                    content = "SearchTotalExpense",
                    size = "l",
                    buttonLabel = "Close"),
              ) #end column
              
            ),#end fluid row
            
            
            "Do you want to hard or soft filtering on total expenses?",
            switchInput(
              "HardTotalExpense",
              label = NA,
              value = FALSE,
              onLabel = "Hard",
              offLabel = "Soft")%>%
              shinyhelper::helper(
                type = "markdown",
                content = "HardSoft",
                size = "l",
                buttonLabel = "Close")
          ) #end conditional panel
        )#end well panel
      ), #end total expenses screen
      
      shinyglide::screen(#search.TotalEmployees
        h4("Do you want to further filter your comparison set by total employees?"),
        wellPanel(
          #search.totalemployee
          switchInput(
            "TotalEmployeeDecide",
            label = NA,
            value = FALSE,
            onLabel = "Yes",
            offLabel = "No"),
          
          conditionalPanel(
            condition = "input.TotalEmployeeDecide == true",
            
            fluidRow(
              column(
                5,
                autonumericInput(
                  inputId = "SearchTotalEmployeeMin",
                  label = "Minimum Employees",
                  value = 0,
                  align = "right",
                  currencySymbolPlacement = "p",
                  digitGroupSeparator = ",",
                  minimumValue = 0,
                  decimalPlaces = 0
                ),
              ), #end column
              column( 1,
                      HTML("<br>  <font size=\"+3\">-</font>")),
              column(
                5,
                autonumericInput(
                  inputId = "SearchTotalEmployeeMax",
                  label = "Maximum Employees",
                  value = 50000,
                  align = "right",
                  currencySymbolPlacement = "p",
                  decimalCharacter = ".",
                  digitGroupSeparator = ",",
                  minimumValue = 1,
                  maximumValue = 50000,
                  decimalPlaces = 0
                ) %>%
                  shinyhelper::helper(
                    type = "markdown",
                    content = "SearchTotalEmployee",
                    size = "l",
                    buttonLabel = "Close"),
              ) #end column
              
            ),#end fluid row
            
            
            "Do you want to hard or soft filtering on total employees?",
            switchInput(
              "HardTotalEmployee",
              label = NA,
              value = FALSE,
              onLabel = "Hard",
               offLabel = "Soft") %>%
              shinyhelper::helper(
                type = "markdown",
                content = "HardSoft",
                size = "l",
                buttonLabel = "Close")
          ), #end conditional panel
          #How many items in dat.filtered.pre
          #h5(htmlOutput("dat.filtered.pre.size"))
        )#end well panel
      ), #end total expenses screen
      
      
      # ### Testing screen - to be deleted
      # shinyglide::screen(
      #   "this is a test screen for making sure search inputed correctly.",
      #   verbatimTextOutput("test2"),
      #   "hard test",
      #   textOutput("test3")
      # 
      # ),

      
      
      # shinyglide::screen(
      #   h4("The Resutls"),
      # 
      #     wellPanel(
      #       textOutput("weighted.average")
      # 
      #     )#well pannel
      # 
      # ), #end results screen
      

      shinyglide::screen(
        h4("The Report"),
        wellPanel(
          HTML("Click here to download your final report. <br>"),
          downloadButton("report", "Generate Report")
        ),#end well panel 
      next_label = "See Comparison Data Set"),
      
      shinyglide::screen(
        h4("Your Comparison Set"), 
        wellPanel(
          
          box(
            #title = "All organizations used to generate your CEO suggested pay:", 
            width = NULL, 
            status = "primary",
            div(style = 'overflow-x: scroll', 
                DT::DTOutput('dat.filtered.pre.table'))
          ) #end box
          
        ) #end wellPanel
        
      ) #end data table picker screen
      
    ),# end shiny glide
    
    
    
    


  ),# End Tab Panel
  
  
  # ##################################################
  #   ###  Gender Pay Differences tabPanel
  # ##################################################
  
  tabPanel("Gender Pay Gap",
           pageWithSidebar(
             headerPanel('Gender Pay Gap'),
             sidebarPanel(width = 4,
                          h3("Filters"),
                          "What types of organizations to you want to look at?",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Filter Criteria",
                                               #search.FormYr
                                               # pickerInput(
                                               #   inputId = "filter.year",
                                               #   label = "test year", 
                                               #   choices = 2013:2019,
                                               #   selected = 2019
                                               # ), #justdoing 2019 for now, will talk to Jesse about why later, this is because we only retained distinct values in the data-rodeo phase. this can be changed later but its too late in the intership at the moment to change everything
                                               # sliderInput(
                                               #   inputId = "filter.year",
                                               #   label = "Years to include", 
                                               #   min = 2009,
                                               #   max = 2019, 
                                               #   step = 1
                                               # ),
                                               # %>%
                                               #   helper(type = "inline",
                                               #          title = "Inline Help",
                                               #          content = c("This helpfile is defined entirely in the UI!",
                                               #                      "This is on a new line.",
                                               #                      "This is some <b>HTML</b>."),
                                               #          size = "s"),
                                               
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
                                                   `select-all-text` = "Select All",
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
                                                   `select-all-text` = "Select All",
                                                   `none-selected-text` = "NA"
                                                 )
                                               ),
                                               #NEED TO ADD NTEE
                                               #NEED TO ADD NTEE.CC both NTEE and NTEE.CC need to be reactive to inside Major Group
                                               # select.HOSP
                                               radioButtons("filter.gender.HOSP", label = "Do you want to compare to organizations that are hospitals?",
                                                            choices = list("No, I do not want to compare with hospitals." = 2,
                                                                           "Yes, I want to include both hospitals and non-hospitals." = NA,
                                                                           "Yes, but I only want to include hospitals." = 1),
                                                            selected = NA),
                                               # select.UNIV
                                               radioButtons("filter.gender.UNIV", label = "Do you want to compare to organizations that are universities?",
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
                                                                 min = 0,
                                                                 max = 1000),
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
                                                 choices = list("Mean" = "Mean",
                                                                "Median"= "Median")
                                               ))
                          )#end tabsetPanel
             ), #end sidebarPanel
             
             
             mainPanel(
               ### Internal tabsetPanel for Gender pay differences
               # end internal tabsetPanel
               #submitButton("Update Graph"),
               "This gap represents the total gender pay gap for all nonprofits who filed with the IRS in 2019. ",
               plotlyOutput("gender.pay.graph"),
               #textOutput("test"),
             ) #end mainPanel
             
           )
           
  ) #end Gender Pay Differences tabPanel
  
#######################
)# end navbar page