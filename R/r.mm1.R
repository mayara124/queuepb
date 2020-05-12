#' @title data simulation for the model M\ M\ 1
#' @name r.mm1
#' @description function for the simulation of data imitating the operation of a queue model.
#' @param n number of simulations interest you want to perform.
#' @param lambda average rate that represents an exponential distribution for arrival times.
#' @param mu average rate that represents an exponential distribution for the times of services.
#' @return will return values in the form of  three column matrix, with the arrival, service and departure times.
#' @author Mayara Almeida
#' @example data <- r.mm1(5,3,4)
#' @importFrom stats rexp
#' @export

r.mm1 <- function(n,lambda,mu){
  
  time.arrival <- rexp(n,1/lambda) # Simulando 1000 tempos entre chegadas sucessivas com lambda = 2
  time.service <- rexp(n,1/mu)
  tdc <- cumsum(time.arrival)# tempo de chegada
  saida <- vector("numeric", length=n)
  tempo.waitting<- NULL
  tcs<- NULL
  
  tempo.waitting [1] <- 0 # tempo de
  saida[1] <- time.arrival[1] + time.service[1]
  tcs[1]<- time.arrival[1]
  for (i in 2:n) {
    saida[i] <- max(tdc[i], saida[i-1]) + time.service[i]
    tempo.waitting[i] <- max(0,saida[i-1]-tdc[i])
    tcs[i] <- max(saida[i-1],tdc[i])# tempo que come?ou o servi?o
  }
  
  rho <- lambda/mu # Taxa de ocupa??o do sistema (verificar de rho < 1)
  
  # Condi??o para quando rho for maior ou igual a 1
  if(rho >= 1)
    ## w <- warning("The system occupation rate is greater or equal to 1")
    warning("The system occupation rate is greater or equal to 1")
  
  data <- cbind(tdc,tcs,saida)
  return (data)
  
  
}