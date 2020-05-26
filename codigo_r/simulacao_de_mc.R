# Usando o conceito de closures na função exp_G().
exp_g <- function(g, ...) {
  # Definindo uma função anônima. Funções anônimas são muito úteis na programação funcional.
  function(x, ..., a) { # Utilizano o operador dot-dot-dot.
    g(x = x, ...) ^ a
  }
}

# pdf() é a função Weibull de parâmetros alpha e beta.
pdf <- function(x, alpha, beta, ...) dweibull(x = x, shape = alpha, scale = beta, ...)
exp_weibull <- exp_g(g = pdf)

# Exemplo: implementando a função exp_weibull apenas declarando G que é a função weibull.
# Note também que a função exp_g() é tão flexível a ponto de permitir parâmetros de g
# sem mesmo saber quais são. Por exemplo, é possível definir o argumento log na função
# exp_weibull(). Veja:
exp_weibull <- exp_g(g = pdf) 
exp_weibull(x = 1, alpha = 0.3, beta = 1.2, a = 1, log = F)

# Exemplo: vamos perceber o quanto exp_G permitirá a construção de funções flexíveis. 

exp_beta <- exp_g(g = function(x, alpha, beta, ...) dbeta(x = x, shape1 = alpha, shape2 = beta, ...))


log_lik <- function(pdf) {
  function(x, ...) {
    -sum(log(pdf(x = x, ...)))
  }
}

log_lik_beta <- log_lik(pdf = exp_beta)
                        
log_lik_beta(x = 1, alpha = 1, beta = 1, a = 1)
