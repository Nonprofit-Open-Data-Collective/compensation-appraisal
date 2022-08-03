### Used packages
packs = c("shiny", "shinydashboard", "shinythemes", "shinyWidgets","shinyvalidate",
          "shinyhelper", "shinyglide", "plotly", "DT",
          "scales", "knitr", 
          "dplyr", "readr", "tidyr","bslib", "datasets" )

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
dat <- readr::read_csv("data-rodeo/dat-shinyapp.csv")

### Load internal functions
source("funcs/applying-filters-func.R")
source("funcs/distance-metric.R")
source("funcs/dollarize.R")
source("funcs/state-distance.R")




### Needed variables 

## Needed for state_distance.R function
#get regions for state match # https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
# 1 = New England & Mid Atlantic 
# 3 = East North Central
# 4 = West North Central
# 5 = South Atlantic 
# 6 = East south central 
# 7 = West South Central
# 8 = Mountain 
# 9 = Pacific

#get regions with DC and PR
region <- data.table::data.table(state = sort(c(state.abb, "PR", "DC")),
                                 region = c(9, 6, 7, 8, 9, 8, 1, 5, 1, 5, 
                                            5, 9, 5, 8, 3, 3, 4, 6, 7, 1, 
                                            1, 1, 6, 4, 4, 6, 8, 5, 4, 4, 
                                            1, 1, 8, 8, 1, 3, 7, 9, 1, 5, 
                                            1, 5, 4, 6, 7, 8, 5, 1, 8, 3, 
                                            5, 8))

