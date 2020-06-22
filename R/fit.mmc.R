#' @title performance measures
#' @name fit.mmc
#' @description It allows to evaluate the efficiency of a system by analyzing its characteristics using performance measures.
#' @param data data set.
#' @param n number of simulations interest you want to perform.
#' @param c number of servers.
#' @param alpha significance level for the Kolmogorov-Smirnov test.
#' @return a list of all performance measures used.
#' @author Mayara Almeida
#' @example fit.mmc(data,10,3,0.05)
#' @importFrom stats ks.test
#' @export
#' 

fit.mmc <- function(data,n,c,alpha){
  
  
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
  
  r <- lambda/mu # Taxa de ocupa??o do sistema (verificar de rho < 1)
  rho <- r/c
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
  
  # Calculando a probabilidade do sistema estar vazio
  
  soma1 = 0
  for (i in 0:(c-1)) {
    soma1 <- soma1 + (r^i/factorial(i)) 
  }
  p0 <- (soma1  + (c*(r^c))/((factorial(c)*(c-r))))^(-1)
  
  # calculando pn
  if (n < c){
    pn <- p0*((r^n)/factorial(n))
  }
  else {
    pn <- p0* ((r^n)/(c^(n-c)*factorial(c)))
  }
  
  
  # Calculando as medidas de desempenho
  
  L <- r + ((r^(c+1)*c)/factorial(c)*(c-r)^2)*p0 # N? m?dio de usu?rios no sistema
  Lq <- (p0*c*r^(c+1))/factorial(c)*(c-r)^2 # N? m?dio de usu?rios na fila
  W <- 1/mu + (r^c*mu/factorial(c-1)*(c*mu - lambda)^2)*p0  # Tempo m?dio de perman?ncia no sistema
  Wq <- (r^c*mu/factorial(c-1)*(c*mu - lambda)^2)*p0 ;Wq # Tempo m?dio de perman?ncia na fila
  
  # Matriz com as medidas de desempenho (performance measures)
  
  values.pm <- c(L, Lq, W, Wq)
  
  pm <- matrix(values.pm, 1, 4)
  
  

  # Calculando a probabilidade de haver fila
  #p.queue <- rho^1
  
  # Sa?das
  
  cat("Rate Arrival = ", lambda, "\n\n")
  cat("Rate Service = ", mu, "\n\n")
  cat("System Probability is Empty  = ", p0, "\n\n")
  #cat("Probability of Queuing = ", p.queue, "\n\n")
  
  colnames(pm) = paste(c("L", "Lq", "W", "Wq"))
  rownames(pm) = paste("Performance Measures")
  pm
  
}
