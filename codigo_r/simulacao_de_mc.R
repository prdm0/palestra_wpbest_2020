library(purrr)
library(furrr)
library(numDeriv)
library(tibble)

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

# Integrando a função pdf_exp_weibull():
integrate(
  f = pdf_exp_weibull,
  lower = 0,
  upper = 100,
  alpha = 1.2,
  beta = 1.3,
  a = 1
)

# Exemplo: implementando a função exp_weibull apenas declarando G que é a cdf weibull.
# Note também que a função exp_g() é tão flexível a ponto de permitir parâmetros de g
# sem mesmo saber quais são. Por exemplo, é possível definir o argumento log na função
# pdf_exp_weibull(). Veja:

pdf_exp_weibull <- exp_g(G = cdf_weibull)
pdf_exp_weibull(
  x = 1,
  alpha = 0.3,
  beta = 1.2,
  a = 1,
  log = F
)


# Exemplo: vamos perceber o quanto exp_g permitirá a construção de funções flexíveis.

exp_beta <-
  exp_g(
    G = function(x, alpha, beta, ...)
      pbeta(
        q = x,
        shape1 = alpha,
        shape2 = beta,
        ...
      )
  )

# Integrando a função exp_beta():
integrate(
  f = exp_beta,
  lower = 0,
  upper = 1,
  alpha = 1.2,
  beta = 1.3,
  a = 6
)

# Exemplo: Passando um vetor x qualquer de dados e obtendo o valor da função
# log-verosimilhança multiplicada por -1, uma vez que optim() minimiza:
log_lik(par = c(1.2, 1.3, 1), x = c(0.2, 0.2, 0.5, 0.65))

# Testando a função de verossimilhança log_lik():
amostra <- rweibull(n = 5000, shape = 2, scale = 3)
result <-
  optim(
    fn = log_lik,
    par = c(2, 3, 1),
    x = amostra,
    method = "Nelder-Mead"
  )


mc <-
  function(M = 1e4L,
           n = 250L,
           par_true = c(2, 3, 1),
           parallel = TRUE) {
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

    plan(multiprocess)

    # Realizando todas as siterações de MC:



    if (parallel == TRUE) {
      results_mc <-
        future_map(
          .x = 1L:M,
          .f = mc_one_step,
          .progress = TRUE,
          .options = future_options(seed = TRUE)
        ) %>%
        unlist %>%
        matrix(nrow = M,
               ncol = length(par_true),
               byrow = TRUE) %>%
        as_tibble
    } else {
      results_mc <-
        map(.x = 1L:M,
            .f = mc_one_step) %>%
        unlist %>%
        matrix(nrow = M,
               ncol = length(par_true),
               byrow = TRUE) %>%
        as_tibble
    }

    colnames(results_mc) <- c("alpha", "beta", "a")

    # Obtendo a média das estimativas de máxima verossimilhança por parâmetro:
    mean_est <- apply(X = results_mc, MARGIN = 2L, FUN = mean)

    bias <- mean_est - par_true

    list(results_mc = results_mc,
         mean_est = mean_est,
         bias = bias)

  } # Fim da simulação.

set.seed(1)
system.time(result <-
              mc(
                M = 1e3L,
                n = 100,
                par_true = c(2, 3, 1),
                parallel = T
              ))
