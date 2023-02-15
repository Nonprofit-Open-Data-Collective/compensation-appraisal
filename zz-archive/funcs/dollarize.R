dollarize <- function(x)
{ paste0("$", format( round( x, 0 ), big.mark="," ) ) }
