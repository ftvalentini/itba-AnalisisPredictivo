

library(tidyverse)
library(glmnet)
library(ggfortify)

set.seed(666)

# sample sizes
N_TRAIN = 100
N_TEST = 2000
N = N_TRAIN + N_TEST

# DGP
df_dgp = tibble(
  x1 =  runif(N, 0, 1)
  ,x2 = runif(N, 0, 1)
  ,y =  rnorm(N, 1 + 2*x1 + 2*x2, 0.25)
)

ggplot(df_dgp) +
  geom_point(aes(x2, y))

ggplot(df_dgp) +
  geom_point(aes(x1, y))

ggplot(df_dgp) +
  geom_point(aes(x1, x2))

# MULTICOLINEALIDAD

# Sumamos N_COLS covariables correlacionadas con cada feature
N_COLS = 20
NOISE = 0.25 # cuanto mas chico, mas correlacionadas

df_cor_cols_1 = replicate(
  N_COLS, rnorm(N, 1 + 2*df_dgp$x1, NOISE), simplify=T) %>% 
  magrittr::set_colnames(paste0("x1_corr_", 1:ncol(.))) %>% 
  as_tibble()
df_cor_cols_2 = replicate(
  N_COLS, rnorm(N, 1 + 2*df_dgp$x2, NOISE), simplify=T) %>% 
  magrittr::set_colnames(paste0("x2_corr_", 1:ncol(.))) %>% 
  as_tibble()

df = list(df_dgp, df_cor_cols_1, df_cor_cols_2) %>% bind_cols()

# analizamos las correlaciones
df_cor = cor(df) %>% 
  as.table() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  filter(Var1 != Var2) %>% 
  rename(cor=Freq) %>% 
  arrange(-abs(cor))
df_cor

ggplot(df) +
  geom_point(aes(x2, x2_corr_10))

# train-test split
idx_train = sample(nrow(df), N_TRAIN)
df_train = df[idx_train,]
df_test =  df[-idx_train,]

dim(df_train)

# modelo base
mod_base = lm(y ~ x1 + x2, df_train)
pred_base = predict(mod_base, newdata=df_test)

# modelo con multicolinealidad
mod_multicol = lm(y ~ ., df_train)
pred_multicol = predict(mod_multicol, newdata=df_test)

# ridge (alpha=0)
X = df_train %>% select(-y) %>% as.matrix()
Y = df_train$y
lambda_seq = exp(seq(-4, 4, length.out=50)) # NOTE lambda seq se elige a prueba-error
cv_ridge = glmnet::cv.glmnet(
  x=X, y=Y, alpha=0, nfolds=5, type.measure="mse", lambda=lambda_seq)
mod_ridge = glmnet::glmnet(x=X, y=Y, alpha=0)
pred_ridge = predict(
  mod_ridge,
  s = cv_ridge$lambda.1se,
  newx = df_test %>% select(-y) %>% as.matrix()
)
# NOTE por default: standardize=TRUE

# lasso (alpha=1)
lambda_seq = exp(seq(-6, -1, length.out=50))
cv_lasso = glmnet::cv.glmnet(
  x=X, y=Y, alpha=1, nfolds=5, type.measure="mse", lambda=lambda_seq)
mod_lasso = glmnet::glmnet(x=X, y=Y, alpha=1)
pred_lasso = predict(
  mod_lasso,
  s = cv_lasso$lambda.1se,
  newx = df_test %>% select(-y) %>% as.matrix()
)

# plot de lambda
plot(cv_ridge)
plot(cv_lasso)
# ggfortify!
autoplot(cv_lasso)


# plot de coeficientes
plot(mod_ridge, xvar="lambda", label=T)
plot(mod_lasso, xvar="lambda", label=T)


# RMSE
rmse = function(y, y_pred) sqrt(sum((y - y_pred)**2))

rmse(df_test$y, pred_base)
rmse(df_test$y, pred_multicol)
rmse(df_test$y, pred_ridge)
rmse(df_test$y, pred_lasso)


# coeficientes
broom::tidy(mod_base) %>% arrange(-abs(estimate))
broom::tidy(mod_multicol) %>% arrange(-abs(estimate))
df_coefs_ridge = coef(mod_ridge, s=cv_ridge$lambda.1se) %>% 
  as.matrix() %>% 
  as_tibble(rownames="term") %>% 
  rename(estimate=s1) %>% 
  arrange(-abs(estimate))
df_coefs_lasso = coef(mod_lasso, s=cv_lasso$lambda.1se) %>% 
  as.matrix() %>% 
  as_tibble(rownames="term") %>% 
  rename(estimate=s1) %>% 
  arrange(-abs(estimate))


as.data.frame(df_coefs_ridge)
as.data.frame(df_coefs_lasso)
