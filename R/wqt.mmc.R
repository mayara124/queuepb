#' @title Cumulative distribution function
#' @name wqt.mmc
#' @description Cumulative queue waiting time distribution function.
#' @param t waiting time.
#' @param c number of servers.
#' @param lambda average rate that represents an exponential distribution for arrival times.
#' @param mu average rate that represents an exponential distribution for the times of services.
#' @return will return the probability value of the time that any user remains in the queue waiting for assistance.
#' @author Mayara Almeida
#' @example Wqt.mmc(2,3,1,0.55)
#' @export

Wqt.mmc <- function(t, c,lambda, mu){
  r<- lambda/mu
  rho <- r/c
  
  soma1 = 0
  for (i in 0:(c-1)) {
    soma1 <- soma1 + (r^i/factorial(i)) 
  }
  p0 <- (soma1  + (c*(r^c))/((factorial(c)*(c-r))))^(-1)
  
  if(t < 0 || rho >= 1){
    print("The value of t is less than zero")
  } else{
    prob <- 1-p0*(((r^c))/((factorial(c)*(1-rho)))*exp(-((c*mu)-lambda)^t))
    return(prob)
  }
  
}
Wqt.mmc(0.01,2,1,0.55)
