### Used packages
packs = c("shiny", "shinydashboard", "shinythemes", "shinyWidgets",
          "shinyglide", "plotly", 
          "scales", "knitr", "kableExtra", "dplyr", "readr", "DT",
          "shinyhelper")

#invisible(lapply(packs, library, character.only = TRUE))


### Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(packs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

### Load Data set
#dat <- read_csv("data-wrangle/data-by-sector.csv")
dat <- read_csv("data-rodeo/dat-shinyapp.csv")

### Load internal functions
source("funcs/applying-filters-func.R")
source("funcs/distance-metric.R")
source("funcs/dollarize.R")

