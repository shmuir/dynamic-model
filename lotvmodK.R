#' Lot. Voltera Model
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predictor
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction)
#'  \emph{beta} is the effect of predator hunting prey
#’  \emph{pmort}  mortality rate of predictor population
#' @examples
#' lotvod(t=1, pop=list(1,2), pop=list(0.5,0.3,0.2,0.2))
#'
#' pars = c(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2)
#' currpop  = c(prey = 1, pred=1)
#  days = seq(from=1,to=20)
#' res = ode(func=lotvmod, y=currpop, times=days, parms=pars)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#'}

# lotvmodK = function(t, pop, pars) {
#   if (prey >= 30) {
#     with(as.list(c(pars,pop)), {
#       dprey = rprey*(1-prey/K)*prey -  alpha*prey*pred - beta*prey*pred
#       dpred = eff*alpha*prey*pred - pmort*pred
#       message(paste("Running model with predation"))
#       return(list(c(dprey,dpred)))}) }
#   
#   else {
#     with(as.list(c(pars,pop)), {
#       dprey = rprey*(1-prey/K)*prey -  alpha*prey*pred
#       dpred = eff*alpha*prey*pred - pmort*pred
#       message(paste("Running model without predation. If you want to run with predation, please use a higher value for prey population."))
#       return(list(c(dprey,dpred)))})
#   }
# }

# lotvmodK = function(t, pop, pars) {
#   
#   with(as.list(c(pars,pop)), {
#     if (prey >= 30) {
#       message(paste("Running model with predation"))
#       dprey = rprey*(1-prey/K)*prey -  alpha*prey*pred - beta*prey*pred
#       dpred = eff*alpha*prey*pred - pmort*pred
#      
#       return(list(c(dprey,dpred)))} 
#     
#     else if (prey < 30) {
#       message(paste("Running model without predation. If you want to run with predation, please use a higher value for prey population."))
#       dprey = rprey*(1-prey/K)*prey -  alpha*prey*pred
#       dpred = eff*alpha*prey*pred - pmort*pred
#       
#       return(list(c(dprey,dpred)))}})
# }

# lotvmodK = function(t, pop, pars) {
# 
#   with(as.list(c(pars, pop)), {
#     print(paste("Prey population:", prey))
#     if (prey >= 30) {
#       dprey = rprey * (1 - prey / K) * prey - alpha * prey * pred - beta * prey * pred
#       dpred = eff * alpha * prey * pred - pmort * pred
#       message("Running model with predation")
#       return(list(c(dprey, dpred)))
#     } else if (prey < 30) {
#       dprey = rprey * (1 - prey / K) * prey - alpha * prey * pred
#       dpred = eff * alpha * prey * pred - pmort * pred
#       message("Running model without predation.")
#       return(list(c(dprey, dpred)))
#     }
#   })
# }


lotvmodK <- function(t, pop, pars, min_prey_pop) {
  with(as.list(c(pars, pop)), {
    dprey = rprey * (1 - prey / K) * prey - alpha * prey * pred - beta * prey * pred
    dpred <- eff * beta * prey * pred - pmort * pred
    
    # ensure prey population remains above min_prey_pop
    if (prey <= min_prey_pop) {
      dprey <- max(dprey, 0)  # prevent prey from decreasing below min_prey_population
    }
    
    return(list(c(dprey, dpred)))
  })
}







