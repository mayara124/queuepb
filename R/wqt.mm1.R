#' @title Cumulative distribution function
#' @name wqt.mm1
#' @description Cumulative queue waiting time distribution function.
#' @param t waiting time.
#' @param lambda average rate that represents an exponential distribution for arrival times.
#' @param mu average rate that represents an exponential distribution for the times of services.
#' @return will return the probability value of the time that any user remains in the queue waiting for assistance.
#' @author Mayara Almeida
#' @examples  Wqt.mm1 <- function(5,3,4)
#' @export



Wqt.mm1<- function(t, lambda, mu){
  
  rho <- lambda/mu
  
  if(t < 0 || rho >= 1){
    print("The value of t is less than zero")
  } else{
    prob <- 1-rho*exp(-(mu-lambda)*t)
    return(prob)
  }
  
}