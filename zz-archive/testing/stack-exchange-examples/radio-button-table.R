#https://stackoverflow.com/questions/53603621/shiny-dt-datatable-with-vertical-radio-button/56613036#56613036
#example of radio buttons in a table format


library(shiny)
library(DT)


m = matrix(
  as.character(1:3), nrow = 3, ncol = 7, byrow = FALSE,
  dimnames = list(c("No", "Yes, include these codes as well as others not listed in this table.", "Yes, exclusivley include these codes.") , 
                  c("01 - Alliance/Advocacy Organizations", 
                    "02 - Management and Technical Assistance",
                    "03 - Professional Societies/Associations", 
                    "05 - Research Institutes and/or Public Policy Analysis", 
                    "11 - Monetary Support - Single Organization",
                    "12 - Monetary Support - Multiple Organizations",
                    "19 - Nonmonetary Support Not Elsewhere Classified (N.E.C.)"))
)
for (i in seq_len(ncol(m))) {
  m[, i] = sprintf(
    '<input type="radio" name="%s" value="%s" selected="2"/>',
    LETTERS[i], m[,i]
  )
}



callback <- c(
  "var LETTERS = ['A','B','C', 'D', 'E', 'F', 'G'];",
  "for(var i=0; i < LETTERS.length; ++i){",
  "  var L = LETTERS[i];",
  "  $('input[name=' + L + ']').on('click', function(){",
  "    var name = $(this).attr('name');",
  "    var value = $('input[name=' + name + ']:checked').val();",
  "    Shiny.setInputValue(name, value);",
  "  });",
  "}"
)

shinyApp(
  ui = fluidPage(
    title = 'Page',
    "Do you want to include the following common codes in your search?", 
    "Note, anything selected in the 3rd column will automatcially override anything selected in the second column on the data filtering step.",
    DT::dataTableOutput('foo'),
    verbatimTextOutput('sel')
  ),
  server = function(input, output, session) {
    output$foo = DT::renderDataTable(
      t(m), 
      escape = FALSE, 
      selection = 'none', 
      server = FALSE,
      options = list(dom = 't', paging = FALSE, ordering = FALSE),
      callback = JS(callback)
    )
    output$sel = renderPrint({
      c(input$A, input$B, input$C, input$D, input$E, input$F, input$G)
    })
  }
)
