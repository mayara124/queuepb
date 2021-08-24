################### fun??o com paci?ncia ###################################
############################################################################

#' @title data simulation for the model M\ M\ c with withdrawal.
#' @name r.MMC_pacience
#' @description function for the simulation of data imitating the operation of a queue model considering withdrawal.
#' @param n number of simulations interest you want to perform.
#' @param lambda average rate that represents an exponential distribution for arrival times.
#' @param mu average rate that represents an exponential distribution for the times of services.
#' @param c number of servers.
#' @param tau average rate that represents an exponential distribution for the times of tolerance
#' @return will return values in the form of  three column matrix, with the arrival, service and departure times.
#' @author Debora de Sousa Cordeiro
#' @example data <- r.MMC_pacience(20,3,1,4,5)
#' @importFrom stats rexp
#' @export


r.MMC_pacience <- function(n,c,lambda,mu,tau){
  
  ## Criando um vetor para guardar os tempos de sa?da de cada guich?
  saida.guiches = vector("numeric", c)
  
  ## Simulando n tempos entre chegadas sucessivas com taxa lambda
  time.arrival <- rexp(n,1/lambda)
  tdc <- cumsum(time.arrival) ## tempos de chegada
  
  ## Simulando n tempos de atendimento com taxa mi
  time.service <- rexp(n,1/mu)
  
  #Simulando n tempos de toler?ncia com taxa tau
  time.tolerance <- rexp(n,1/tau)
  
  ## Criando vetores para armazenar o seguinte:
  saida <- NULL ## Tempo de sa?da
  tcs <- NULL ## Tempo do come?o do servi?o
  guiche <- NULL ## Guiche de atendimento
  tempo.waitting<- NULL #tempo de espera
 
  ## Criando uma fun??o auxiliar, para evitar o problema de usar o sample diretamente...
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  
  
  for(i in 1:n) {
    idx = which(saida.guiches < tdc[i]) # Verifica se existem guiches livres
    
    if(length(idx) > 0) { # Caso exista
      atendente <- resample(idx, 1) # Sorteia um entre os livres
      tcs[i] <- tdc[i] # Inicio do servi?o coincide com a chegada
      guiche[i] = atendente
      saida[i] <- saida.guiches[atendente] <- tcs[i] + time.service[i]
    }
    else { # Caso todos estejam ocupados
      atendente <- which.min(saida.guiches) # Pega o primeiro a desocupar
      tcs[i] <- saida.guiches[atendente] # Inicio do servi?o qdo desocupar o 1.
      tempo.waitting[i] = tcs[i] - tdc[i]
      if(tempo.waitting[i] < time.tolerance[i]) {
        # Tempo de espera menor que a toler?ncia...
        guiche[i] = atendente
        saida[i] <- saida.guiches[atendente] <- tcs[i] + time.service[i]
      }
      else {
        # Caso contr?rio, ele desiste...
        tcs[i] <- guiche[i] <- NA
        saida[i] <- tdc[i] + time.tolerance[i]        
      }
    }
    
  }
  
  ## Retorna a matriz com os valores simulados
  return(cbind(tdc, tcs, saida, guiche))
  
}

