library(shiny)
library(DT)
data(mpg)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Select only filtered rows using selectall button"),
  
  br(),
  br(),
  
  DT::dataTableOutput("table")
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- DT::renderDataTable({
    datatable(mpg, escape=F,
              rownames=F,
              filter = 'top',
              #  colnames = c("Data Type","Variable","Description", "Filename"),
              class = "compact hover row-border",
              extensions = c('Scroller','Select', 'Buttons'),
              
              options = list(
                select = list(style = "multi", items = "row"),
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                language = list(
                  info = 'Showing _START_ to _END_ of _TOTAL_ variables'),
                deferRender = TRUE,
                scrollY = 500,
                scroller = TRUE,
                dom = "Blfrtip",
                buttons = list(list(extend='selectAll',className='selectAll',
                                    text="select all rows",
                                    action=DT::JS("function () {
                                var table = $('#DataTables_Table_0').DataTable();
                                table.rows({ search: 'applied'}).deselect();
                                table.rows({ search: 'applied'}).select();
                }")
                ), list(extend='selectNone',
                        text="DeselectAll",
                        action=DT::JS("function () {
                                var table = $('#DataTables_Table_0').DataTable();
                                table.rows({ search: 'applied'}).select();
                                table.rows({ search: 'applied'}).deselect();
                }")
                ))
                
              ),
              selection="none"
    )#end data table
    }, server = F
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
