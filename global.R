### Used packages
packs = c("shiny", "shinydashboard", "shinythemes", "shinyWidgets", "plotly", 
            "scales", "knitr", "kableExtra", "dplyr", "readr")

### Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(packs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

### Load Data set
dat <- read_csv("data-wrangle/data-by-sector.csv")

### Load internal functions
source("testing/applying-filters-func.R")
