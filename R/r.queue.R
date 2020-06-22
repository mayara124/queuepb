
#' @title function for data simulation
#' @name r.queue
#' @description function for the simulation of data imitating the operation of a queue model.
#' @param n number of simulations interest you want to perform.
#' @param c number of servers.
#' @param lambda average rate that represents an exponential distribution for arrival times.
#' @param mu average rate that represents an exponential distribution for the times of services.
#' @return will return values in the form of a matrix of four columns, with the arrival times, the times when the services were started, the departures and the number of the counter that was attended. 
#' @author Mayara Almeida
#' @example set.seed(10)
#' data <- r.queue(10,2,1,4)
#' @importFrom stats rexp
#' @export


#set.seed(10)

r.queue<- function(n,c,lambda,mu){

  ## Criando um vetor para guardar os tempos de sa?da de cada guich?
  saida.guiches = vector("numeric", c)
  
  ## Simulando n tempos entre chegadas sucessivas com taxa lambda
  time.arrival <- rexp(n,1/lambda)
  tdc <- cumsum(time.arrival) ## tempos de chegada

  ## Simulando n tempos de atendimento com taxa mu
  time.service <- rexp(n,1/mu)
  
  ## Criando vetores para armazenar o seguinte:
  saida <- NULL ## Tempo de sa?da
  tcs <- NULL ## Tempo do come?o do servi?o
  guiche <- NULL ## Guiche de atendimento
  
  for(i in 1:n) {
    idx = which(saida.guiches < tdc[i]) # Verifica se existem guiches livres
    if(length(idx) > 0) { # Caso exista
      atendente <- sample(idx, 1) # Sorteia um entre os livres
      tcs[i] <- tdc[i] # Inicio do servi?o coincide com a chegada
      guiche[i] = atendente
      saida[i] <- saida.guiches[atendente] <- tcs[i] + time.service[i]
    }
    else { # Caso todos estejam ocupados
      atendente <- which.min(saida.guiches) # Pega o primeiro a desocupar
      tcs[i] <- saida.guiches[atendente] # Inicio do servi?o qdo desocupar o 1.
      guiche[i] = atendente
      saida[i] <- saida.guiches[atendente] <- tcs[i] + time.service[i]
      
    }
  }

  ## Retorna a matriz com os valores simulados
  return(cbind(tdc, tcs, saida, guiche))
  
}


#data <- r.queue(10,1,1,4)
