#' Lot. Voltera Model
#'
#' function computes the rate of change of populations in a predictor prey interaction with hunting
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predictor
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction)
#'  \emph{beta} is the effect of hunting by humans
#’  \emph{pmort}  mortality rate of predictor population
#' @param thresh minimum threshold for hunting
#' @examples
#' lotvod(t=1, pop=list(1,2), pop=list(0.5,0.3,0.2,0.2), thresh=20)
#'
#' pars = c(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2)
#' currpop  = c(prey = 1, pred=1)
#  days = seq(from=1,to=20)
#' res = ode(func=lotvmod, y=currpop, times=days, parms=pars, thresh=20)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#'}

lotvmodK = function(t, pop, pars, thresh) {

  with(as.list(c(pars, pop)), {
    if (prey >= thresh) {
      dprey = rprey * (1 - prey / K) * prey - alpha * prey * pred - beta * prey
      dpred = eff * alpha * prey * pred - pmort * pred
      return(list(c(dprey, dpred)))
    } else if (prey < thresh) {
      dprey = rprey * (1 - prey / K) * prey - alpha * prey * pred
      dpred = eff * alpha * prey * pred - pmort * pred
      return(list(c(dprey, dpred)))
    }
  })
}







