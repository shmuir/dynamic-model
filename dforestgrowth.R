#' Forest growth model
#' @author Sam Muir and Melissa Widas
#' @param Time time period
#' @param params  list of parameters with 3 values: r (pre canopy closure growth rate (exponential)), g (post-canopy closure growth rate (linear)), and K (carrying capacity in units of carbon)
#' @param C initial size of the forest in units of carbon
#' @param thresh canopy closure threshold in units of carbon
#'
#' @return list: derivative of forest size with time
#'

dforestgrowth <- function(Time, C, params, thresh) {
  
  if (C < thresh) {
    dC_dT <- params$r * C
    return(list(dC_dT))
  }
  else if (C >= thresh) {
    dC_dT <- params$g * (1 - C / params$K)
    return(list(dC_dT))
  }
  
}