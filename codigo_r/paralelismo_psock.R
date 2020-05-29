library(numDeriv)
library(parallel)
library(pbapply)
library(magrittr)

# Usando o conceito de closures na função exp_G().
exp_g <- function(G, ...) {
  # Definindo uma função anônima. Funções anônimas são muito úteis na programação funcional.
  function(x, ..., a) {
    # Utilizano o operador dot-dot-dot.
    numDeriv::grad(
      func = function(x)
        G(x = x, ...) ^ a,
      x = x
    )
  }
}

# pdf() é a função Weibull de parâmetros alpha e beta.
cdf_weibull <-
  function(x, alpha, beta, ...)
    pweibull(q = x,
             shape = alpha,
             scale = beta,
             ...)

pdf_exp_weibull <- exp_g(G = cdf_weibull)

mc <-
  function(M = 1e4L,
           n = 250L,
           par_true = c(2, 3, 1)) {
    alpha <- par_true[1L]
    beta <- par_true[2L]
    a <- par_true[3L]
    
    starts <- rep(1, length(par_true))
    
    
    # Implementando a função de verossimilhança da weibull
    log_lik <- function(par, x) {
      alpha <- par[1L]
      beta <- par[2L]
      a <- par[3L]
      - sum(log(pdf_exp_weibull(
        x = x,
        alpha = alpha,
        beta = beta,
        a = a
      )))
    }
    
    # Tratamento de erro
    myoptim <-
      function(...)
        tryCatch(
          exp = optim(...),
          error = function(e)
            NA
        )
    
    # Uma única iteração de Monte-Carlo - MC:
    mc_one_step <- function(i) {
      # Não sei quantas vezes o repeat irá executar.
      repeat {
        cenario <- rweibull(n = n,
                            shape = alpha,
                            scale = beta)
        result <- myoptim(fn = log_lik, par = starts, x = cenario)
        
        
        if (!is.na(result) && result$convergence == 0)
          break
      } # Encontrando uma amostra adequada.
      
      # Retornando as estimativas de máxima verossimilhança.
      result$par
    }
    
    results_mc <-
      pblapply(X = 1L:M, FUN = mc_one_step, cl = cl) %>%
      unlist %>%
      matrix(nrow = M,
             ncol = length(par_true),
             byrow = TRUE) 
    
    colnames(results_mc) <- c("alpha", "beta", "a")
    
    # Obtendo a média das estimativas de máxima verossimilhança por parâmetro:
    mean_est <- apply(X = results_mc, MARGIN = 2L, FUN = mean)
    
    bias <- mean_est - par_true
    
    list(results_mc = results_mc,
         mean_est = mean_est,
         bias = bias)
    
  }

cores <- getOption("mc.cores", 8L) 

# Criando um cluster PSOCK:
cl <- makeCluster(cores, type = "PSOCK")

clusterExport(
  cl = cl,
  varlist = c("mc", "exp_g", "cdf_weibull", "pdf_exp_weibull"),
  envir = environment()
)

clusterEvalQ(cl = cl, expr = library(numDeriv))

# Garantindo reprodutibilidade:
clusterSetRNGStream(cl = cl, iseed = 1)

system.time(result <-
              mc(
                M = 1000L,
                n = 50,
                par_true = c(2, 3, 1)
              ))
stopCluster(cl)