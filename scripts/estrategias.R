library(tidyverse)
set.seed(1313)

# Criando banco de dados --------------------------------------------------

criar_amostra <- function(n, perc_treino) {
  data_frame(
    x = runif(n, 0, 20),
    y = 500 + 0.4 * (x-10)^3 + rnorm(n, sd = 50),
    is_train = 1:n %in% sample.int(n, n*perc_treino)
  )
}

df <- criar_amostra(100, 0.5)

df_train <- df %>% filter(is_train)
df_test <- df %>% filter(!is_train)

ggplot(df, aes(x = x, y = y, color = is_train)) +
  geom_point() +
  geom_smooth(data = df_train, method = "lm", se = FALSE)

# Ajustando o primeiro modelo ---------------------------------------------

modelo <- lm(y ~ x, data = df_train)
summary(modelo)

df$predicao <- predict(modelo, df)
df %>% 
  group_by(is_train) %>% 
  summarise(rmse = sqrt(mean((predicao - y)^2)))


# Ajustando um modelo mais complexo ---------------------------------------

ggplot(df, aes(x = x, y = y, color = is_train)) +
  geom_point() +
  geom_smooth(data = df_train, method = "lm", se = FALSE, 
              formula = y ~ poly(x, 50, raw = TRUE))


modelo <- lm(y ~ poly(x, 50, raw = TRUE), data = df_train)

df$predicao <- predict(modelo, df)
df %>% 
  group_by(is_train) %>% 
  summarise(rmse = sqrt(mean((predicao - y)^2)))


# Ajustando diversos modelos ----------------------------------------------

ajustar_polinomios <- function(df, grau) {
  
  erros <- NULL
  for(g in grau) {
    
    modelo <- lm(y ~ poly(x, g, raw = TRUE), data = df %>% filter(is_train))
    
    df$predicao <- predict(modelo, df)
    erros <- bind_rows(
      erros,
      df %>% 
        group_by(is_train) %>% 
        summarise(mse = mean((predicao - y)^2)) %>% 
        mutate(grau = g)
    )
    
  }
  erros
}

erros <- ajustar_polinomios(df, 1:50)
ggplot(erros, aes(x = grau, y = mse)) + 
  geom_line() + 
  geom_point(
    data = erros %>% group_by(is_train) %>% filter(mse == min(mse)),
    size = 3
    ) +
  facet_wrap(~is_train) 


# Por que variância alta?

n_vezes <- 100
df <- criar_amostra(10000, 0.5)
gg <- ggplot(df, aes(x = x, y = y)) +
  geom_point() 

for(i in 1:n_vezes) {
  gg <- gg + 
    geom_smooth(
      data = df %>% sample_n(50),
      alpha = 0.01,
      color = "red",
      method = "lm", se = FALSE, 
      formula = y ~ poly(x, 20, raw = TRUE)
      )
}

gg + coord_cartesian(ylim = c(0, 1000))
  

# Qual o efeito do tamanho da amostra? -------------------------------------

df <- criar_amostra(1000, 0.5)
erros <- ajustar_polinomios(df, seq(1, 200, by = 5))
ggplot(erros, aes(x = grau, y = mse)) + 
  geom_line() + 
  geom_point(
    data = erros %>% group_by(is_train) %>% filter(mse == min(mse)),
    size = 3
  ) +
  facet_wrap(~is_train) 


# Qual o efeito do tamanho da amostra de teste? ---------------------------

df <- criar_amostra(1000, 0.99)
erros <- ajustar_polinomios(df, seq(1, 200, by = 5))
ggplot(erros, aes(x = grau, y = mse)) + 
  geom_line() + 
  geom_point(
    data = erros %>% group_by(is_train) %>% filter(mse == min(mse)),
    size = 3
  ) +
  facet_wrap(~is_train) 

# Implementando validação cruzada -----------------------------------------

df <- criar_amostra(100, 0.5)
k <- 100
df$fold <- 1:nrow(df)
erros <- NULL
for (i in 1:k) {
  df$is_train <- df$fold != i
  erro <- ajustar_polinomios(df, grau = 1:50)
  erro$fold <- i
  erros <- bind_rows(erros, erro)
}

erros2 <- erros %>% 
  group_by(grau, is_train) %>% 
  summarise(
    media = median(mse),
    minimo = min(mse),
    maximo = max(mse),
    quantil95 = quantile(mse, probs = 0.95),
    quantil5 = quantile(mse, probs = 0.05)
    )

ggplot(erros2, aes(x = grau, y = media)) + 
  geom_line() + 
  geom_point(
    data = erros2 %>% group_by(is_train) %>% filter(media == min(media)),
    size = 3
  ) +
  facet_wrap(~is_train) +
  #coord_cartesian(ylim = c(0, 5000)) +
  geom_ribbon(aes(ymin = quantil5, ymax = quantil95), alpha = 0.2)



# Exemplo regressão polinomial ---------------------------------------------

library(tidyverse)

# Gerando os dados

set.seed(7)

dados <- tibble(
  x = runif(10),
  y = 2*x + 3*x^2 + rnorm(10, 0, 0.15) 
)

ggplot(dados, aes(x = x, y = y)) + 
  geom_point() + 
  theme_bw() +
  ggtitle("(a)") +
  scale_y_continuous(limits = c(-4, 12), breaks = c(0, 4, 8))

# Ajustando modelos

ggplot(dados, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  #geom_smooth(formula = y ~ poly(x, 2), colour = "orange", se = FALSE, method = 'lm') +
  #geom_smooth(formula = y ~ poly(x, 9), colour = "blue", se = FALSE, method = 'lm') +
  theme_bw() +
  ggtitle("(b)") +
  scale_y_continuous(limits = c(-4, 12), breaks = c(0, 4, 8))

# Obtendo novos dados

dados2 <- tibble(
  x = runif(100),
  y = 2*x + 3*x^2 + rnorm(100, 0, 0.1) 
)

# Comparando os modelos com a nova base

ggplot(dados, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 2), colour = "orange", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 9), colour = "blue", se = FALSE, method = 'lm') +
  geom_point(data = dados2, aes(x = x, y = y)) +
  theme_bw() +
  ggtitle("(c)") +
  scale_y_continuous(limits = c(-4, 12), breaks = c(0, 4, 8))

# Ajustando os modelos na nova base

ggplot(dados2, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(formula = y ~ x, colour = "red", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 2), colour = "orange", se = FALSE, method = 'lm') +
  geom_smooth(formula = y ~ poly(x, 9), colour = "blue", se = FALSE, method = 'lm') +
  theme_bw()  +
  ggtitle("(d)") +
  scale_y_continuous(limits = c(-4, 12), breaks = c(0, 4, 8))

# Vamos ver o RMSE

poly_lm <- function(dados, d) {
  lm(y ~ poly(x, degree = d, raw = TRUE), data = dados)
}

modelo <- lm(y ~ x, data = dados)
modelo2 <- lm(y ~ poly(x, 2), data = dados)

calcula_rmse <- function(modelo) {
  mean(residuals(modelo)^2) %>%
    sqrt() %>% 
    round(3)
}

calcula_rmse2 <- function(modelo, dados) {
  mean((predict(modelo, newdata = dados) - dados$y)^2) %>% 
    sqrt() %>% 
    round(3)
}

rmse1 <-
  map(1:9, poly_lm, dados = dados) %>% 
  map(calcula_rmse) %>% 
  purrr::flatten_dbl()

rmse2 <-
  map(1:9, poly_lm, dados = dados) %>% 
  map(calcula_rmse2, dados = dados2) %>% 
  purrr::flatten_dbl()

tibble(
  `Grau do polinômio` = 1:9, 
  RMSE = rmse1, 
  RMSE2 = rmse2
)

# Implementando LOOC ------------------------------------------------------