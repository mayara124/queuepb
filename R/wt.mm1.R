#' @title Cumulative distribution function
#' @name wt.mm1
#' @description Cumulative waiting time distribution function
#' @param t waiting time.
#' @param lambda average rate that represents an exponential distribution for arrival times.
#' @param mu average rate that represents an exponential distribution for the times of services.
#' @return will return the probability value of the time that any user remains on the system.
#' @author Mayara Almeida
#' @examples  Wt.mm1(5,3, 4)
#' @export






Wt.mm1 <- function(t, lambda, mu){
  
  rho <- lambda/mu
  
  if(t < 0 || rho >= 1){
    print("The value of t is less than zero")
  } else{
    prob <- 1-exp(-mu*(1-rho)*t)
    return(prob)
  }
  
}
