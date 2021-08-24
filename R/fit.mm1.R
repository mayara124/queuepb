#' @title performance measures
#' @name fit.mm1
#' @description It allows to evaluate the efficiency of a system by analyzing its characteristics using performance measures.
#' @param n number of simulations interest you want to perform.
#' @param data data set.
#' @param alpha significance level for the Kolmogorov-Smirnov test.
#' @return a list of all performance measures used.
#' @author Mayara Almeida
#' @examples fit.mm1(data,5,0.05)
#' @importFrom stats ks.test
#' @export
#' 


fit.mm1<- function(data,n,alpha){
  
  # OBS: "data" tem que ser uma matriz
  # alpha n?vel de signific?ncia definido pelo pesquisador.
  
  tdc <- data[,1] #Tempos de chegadas sucessivas
  tcs <- data[,2] # Tempos come?ou os atendimentos
  saida <- data[,3]# tempode sa?da
  time.arrival = vector("numeric", length = n)
  time.service = vector("numeric", length= n)
  time.arrival[1] <- tdc[1]-0
  time.service[1]<- saida[1]-tcs[1]
  for (i in 2:n) {
    time.arrival [i] <- tdc[i]-tdc[i-1]
    time.service [i] <- saida[i]-tcs[i]
  }
  
  lambda <- n /(sum(time.arrival)) # estimativa param?tro lambda
  mu <- n /(sum(time.service))# estimativa para?tro mu
  
  
  # lambda <- mean(time.arrival) # Taxa de chegadas
  #mu <- mean(time.service) # Taxa de atendimento
  
  rho <- lambda/mu # Taxa de ocupa??o do sistema (verificar de rho < 1)
  
  # Condi??o para quando rho for maior ou igual a 1
  # teste para verificar se os dados segue uma distribui??o exponencial
  
  if(rho >= 1){
    stop("The system occupation rate is greater or equal to 1")
  }
  
  test.1 <- ks.test(data[,1],"pexp",1/mean(data[,1]))
  p.value <- test.1$p.value
  #alpha <- 0.05
  if( p.value <= alpha){
    stop("the data does not follow an exponential distribution")
    
  }
  
  test.2 <- ks.test(data[,2],"pexp", 1/mean(data[,2]))
  p.value.2 <- test.2$p.value
  if(p.value <= alpha){
    stop("the data does not follow an exponential distribution")
  }
  
  # Calculando as medidas de desempenho
  
  L <- rho/(1-rho) # Nº medio de usuarios no sistema
  Lq <- rho^2/(1-rho) # N? m?dio de usu?rios na fila
  W <- 1/(mu-lambda) # Tempo m?dio de perman?ncia no sistema
  Wq <- rho/(mu-lambda);Wq # Tempo m?dio de perman?ncia na fila
  
  # Matriz com as medidas de desempenho (performance measures)
  
  values.pm <- c(L, Lq, W, Wq)
  
  pm <- matrix(values.pm, 1, 4)
  
  # Calculando a probabilidade do sistema estar vazio
  p0 <- 1-rho
  
  # Calculando a probabilidade de haver fila
  p.queue <- rho^1
  
  # Saidas
  
  cat("Rate Arrival = ", lambda, "\n\n")
  cat("Rate Service = ", mu, "\n\n")
  cat("System Probability is Empty  = ", p0, "\n\n")
  cat("Probability of Queuing = ", p.queue, "\n\n")
  
  colnames(pm) = paste(c("L", "Lq", "W", "Wq"))
  rownames(pm) = paste("Performance Measures")
  pm
  
}
