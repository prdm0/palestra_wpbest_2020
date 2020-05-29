library(numDeriv)
library(parallel)
library(pbmcapply)
library(magrittr)


# Usando conceito de closures para implementar a função exp_g() de forma
# genérica.

exp_g <- function(G, ...) {
  function(x, ..., a) {
    grad(fun = function(x) G(x, ...) ^ a, x = x)
  }
}

cdf_weibull <- function(x, alpha, beta, ...) 
  pweibull(q = x, shape = alpha, scale= beta, ...)

# Construindo a função densidade de probabilidade da Exp-Weibull.
pdf_exp_weibull <- exp_g(G = cdf_weibull)

pdf_exp_weibull(x = 1.2, alpha = 1, beta = 2, a = 1)


# Iniciando a implementação da simulação de MC ----------------------------


mc <- function(M = 1e3L, n = 100L, par_true = c(2, 3, 1), cl = cl) {
  
  alpha <- par_true[1L]
  beta <- par_true[2L]
  a <- par_true[3L]
  
  starts <- rep(1, length(par_true))
  
  # Implementando a função log-verossimilhança da Exp-Weibull:
  log_lik <- function(par, x) {
    alpha <- par[1L]
    beta <- par[2L]
    a <- par[3L]
    -sum(log(pdf_exp_weibull(x = x, alpha = alpha, beta = beta, a = a)))
  }
  
  myoptim <- function(...)
    tryCatch(
      expr = optim(...),
      error = function(e)
        NA
    )
  
  # Uma única iteração de Monte-Carlo - MC:
  mc_one_step <- function(i){
    
    repeat{
      cenario <- rweibull(n = n, shape = alpha, scale = beta)
      result <- myoptim(fn = log_lik, par = starts, x = cenario)
      
      if(!is.na(result) && result$convergence == 0)
        break
    }
    
    result$par # Estimativas de máxima verossimilhança.
  }
  
  result_mc <-
    pbmclapply(X = 1L:M, FUN = mc_one_step, mc.cores = detectCores()) %>%
    unlist %>% 
    matrix(ncol = length(par_true), nrow = M, byrow = TRUE)  
  
  colnames(result_mc) <- c("alpha", "beta", "a")
  
  mean_mc <- apply(X = result_mc, MARGIN = 2L, FUN = mean) # Média das estimativas 
  
  bias <- mean_mc - par_true # Víes
  
  list(result_mc = result_mc, mean_mc = mean_mc, bias = bias)
  
}

system.time(result <- mc(M = 1000, n = 100, par_true = c(2, 3, 1), cl = cl))





