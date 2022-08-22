
library(manipulate)
library(tidyverse)
library(cowplot)

mse = function(a, b) sum((a - b)**2) / length(b)

dgp_systematic <- function(x, complexity) {
  x + sin(complexity * x) 
}

FLEXIBILITY = 1:20
SEED = 42
N = 1000
FRAC_TRAIN = 0.2

manipulate({
  plot(cars)}, x=slider(0, 50, step=2, initial=0)
)

manipulate(
  {
    # simulamos datos (y = f(x) + epsilon) 
    x = runif(N, min=0, max=1)
    f_x = dgp_systematic(x, COMPLEXITY)
    epsilon_sd = ERROR_DGP * (max(f_x) - min(f_x)) 
    epsilon = rnorm(N, mean=0, sd=epsilon_sd) # 'error' es una proporcion del rango de y
    df = tibble(x, f_x, epsilon)
    df = df %>% mutate(y = f_x + epsilon)
    
    # entrenamos modelos (loess) con distintos niveles de flexibilidad
    set.seed(SEED)
    idx_train = sample(N, FRAC_TRAIN*N, rep=F)
    mse_train = rep(NA, length(FLEXIBILITY))
    mse_test = rep(NA, length(FLEXIBILITY))
    for (i in seq_along(FLEXIBILITY)) {
      f = FLEXIBILITY[i]
      model = loess(y ~ x, data=df[idx_train,], span=1/f, 
                    control=loess.control(surface="direct"))
      y_hat = predict(model, df)
      mse_train[i] = mse(df[["y"]][idx_train], y_hat[idx_train])
      mse_test[i] = mse(df[["y"]][-idx_train], y_hat[-idx_train])
      if (f == FLEXIBILITY_DISPLAY) {
        df["f_hat"] = y_hat
      }
    }
    optimal_flexibility = which.min(mse_test)
    df_mse = tibble(FLEXIBILITY, mse_train, mse_test)
    df_mse_long = df_mse %>%
      pivot_longer(!FLEXIBILITY, names_to="metric")
    
    # graficamos el DGP
    dgp_plot =
      ggplot(df) + 
      geom_point(aes(x=x, y=y), cex=.8) + 
      stat_function(
        fun=function(x) dgp_systematic(x, COMPLEXITY), color="orange", cex=1) +
      geom_line(aes(x=x, y=f_hat), color="forestgreen", cex=1) +
      theme_minimal() +
      labs(x=NULL, y=NULL) +
      NULL
    
    # graficamos el tradeoff sesgo-varianza
    titulo = paste0("optimal flexibility = ", optimal_flexibility)
    tradeoff_plot =
      ggplot(df_mse_long) +
      geom_line(aes(x=FLEXIBILITY, y=value, color=metric), cex=1) + 
      theme_minimal() + 
      theme(legend.position="bottom") +
      labs(x=NULL, y=NULL, subtitle=titulo) +
      NULL
    
    plot_grid(dgp_plot, tradeoff_plot)
    # tradeoff_plot
    
  }, 
  # parametros para mover
  FLEXIBILITY_DISPLAY = slider(
    min(FLEXIBILITY), max(FLEXIBILITY) , step=1, initial=1),
  ERROR_DGP = slider(0, .5 , step=.025, initial=.2),
  COMPLEXITY = slider(0, 50, step=2, initial=0)
)

