library(ggplot2)
library(dplyr)
library(purrr)

set.seed(10)


# regression vs classification --------------------------------------------


df_regression = tibble(
  x = runif(100, 0, 1)
  ,mu = 10 * x + 5
  ,y = rnorm(100, mu, 1)
)

plt_regression = 
  ggplot(df_regression, aes(x,y)) + 
  geom_point(color="navy") + 
  labs(x="Humedad en t-1 (%)", y="Lluvia en t (mm)") +
  scale_x_continuous(labels = scales::percent) +
  NULL

df_classification = tibble(
  x1 = runif(100, 0, 1)
  ,x2 = runif(100, 950, 1050)
  ,y = 3 * x1 / mean(x1) - 3 * x2 / mean(x2) + rnorm(100, 0, 1)
  ,proba = 1 / (1 + exp(-y))
  ,lluvia = (proba > 0.5)
)

(
  plt_classification = 
    ggplot(df_classification, aes(x1, x2)) + 
    geom_point(aes(color=lluvia)) + 
    labs(x="Humedad en t-1 (%)", y="Presión atmosférica en t-1 (hPa)", 
         color="Lluvia en t") +
    scale_x_continuous(labels = scales::percent) +
    theme(legend.position="bottom") +
    NULL
)


ggsave("10-intro_supervised/img/regression.png", plt_regression, width=4, height=4)
ggsave("10-intro_supervised/img/classification.png", plt_classification, width=4, height=4)



# plots dgp ---------------------------------------------------------------

# plot function
fplot = function(data, label_x, label_y) { 
  ggplot(data, aes(x,y)) + 
    geom_point(color="navy") + 
    geom_line(aes(x,mu), color="orange", size=1) + 
    labs(x=label_x, y=label_y) +
    scale_x_continuous(labels = scales::percent) +
    NULL
}

# plot 1
dat1 = tibble(
  x = runif(100, 0, 1)
  ,mu = 10 * x + 5
  ,y = rnorm(100, mu, 1)
)

# plot 2
dat2 = tibble(
  x = runif(100, 0, 1)
  ,mu = 10 * x + 15
  ,y = rnorm(100, mu, 5)
)

# plot 3
dat3 = tibble(
  x = runif(100, 0, 1)
  ,mu = 10 * x +  sin(x * 10) + 5
  ,y = rnorm(100, mu, .5)
)

# plot 4
dat4 = tibble(
  x = runif(100, 0, 1)
  ,mu = 10 * x +  sin(x * 10) + 15
  ,y = rnorm(100, mu, 1.5)
)

plist = list(dat1, dat2, dat3, dat4) %>% 
  map(function(x) fplot(
    x, 
    label_x="Humedad en t-1 (%)", 
    label_y="Lluvia en t (mm)"
  ))

for (i in 1:4) {
  filename = paste0("10-intro_supervised/img/dgp_",i,".png")
  ggsave(filename, plot=plist[[i]], width=4, height=4)
}


# plots estimacion --------------------------------------------------------

kplot = function(data, k, plot_mu=F) {
  if (is.na(k)) {
    mod = lm(y ~ x, data)
    data$fitted = mod$fitted.values
  } else {
    mod = kknn::kknn(y ~ x, data, data, k=k)
    data$fitted = mod$fitted.values
  }
  g = ggplot(data, aes(x,y)) + 
    geom_point(color="navy") + 
    geom_line(aes(x,fitted), color="green", size=1) + 
    labs(x="Humedad en t-1 (%)", y="Lluvia en t (mm)") +
    scale_x_continuous(labels = scales::percent) +
    NULL
  if (!plot_mu) {
    return(g)
  } else {
    return(g + geom_line(aes(x,mu), color="orange", size=1)) 
  }
}

dat = tibble(
  x = runif(300, 0, 1)
  ,mu = 10 * x +  sin(x * 10) + 15
  ,y = rnorm(300, mu, 1.5)
)

klist = list()
ks = c(2, 5, 50, NA)
for (i in seq_along(ks)) {
  klist[[i]]= kplot(dat, ks[i], plot_mu=T)
  filename = paste0("10-intro_supervised/img/fit_k",ks[i],".png")
  ggsave(filename, plot=klist[[i]], width=4, height=4)
}


# residuals MSE -----------------------------------------------------------

residuals_plot = function(data, k) { 
  mod = kknn::kknn(y ~ x, data, data, k=k)
  data$fitted = mod$fitted.values
  g = ggplot(data, aes(x,y)) + 
    geom_point(color="navy") + 
    geom_segment(aes(xend=x, yend=fitted), linetype="dashed", cex=0.5) +
    geom_line(aes(x, fitted), color="green", size=1) + 
    labs(x="Humedad en t-1 (%)", y="Lluvia en t (mm)") +
    scale_x_continuous(labels = scales::percent) +
    NULL
  return(g)
}

plt2 = residuals_plot(dat, 2)
plt50 = residuals_plot(dat, 50)

ggsave("10-intro_supervised/img/residuals_k2.png",  plot=plt2, width=6, height=4)
ggsave("10-intro_supervised/img/residuals_k50.png", plot=plt50, width=6, height=4)

       


