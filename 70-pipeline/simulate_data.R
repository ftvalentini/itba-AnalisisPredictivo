

library(tidyverse)
library(caret)

set.seed(42)


# DINO --------------------------------------------------------------------

# 2 dinosaur

dino_data = datasauRus::datasaurus_dozen %>%
  filter(dataset == "dino") %>% 
  select(x, y)

dino_data_expanded = dino_data

for (i in 1:4) {
  dino_data_perturbed_ = dino_data %>% 
    mutate_all(function(x) x + runif(nrow(.)))
  dino_data_expanded = bind_rows(dino_data_expanded, dino_data_perturbed_) 
}
names(dino_data_expanded) = c("v1", "v2")

n = dino_data_expanded %>% nrow()

# RANDOM ------------------------------------------------------------------

# 2 uniforms
random_data = matrix(runif(n * 2, 0, 1), n, 2) %>% as.data.frame()

# 3 gammas
for (i in 1:3) {
  random_data[letters[i]] = rgamma(n, i, i+1)
}  

# 2 categoricals
random_data["x"] = sample(LETTERS[1:4], n, replace=T, prob=rep(1/4, 4)) 
random_data["y"] = sample(LETTERS[1:4], n, replace=T, prob=c(0.2, 0.2, 0.50, 0.10))

names(random_data) = paste0("v", 1:ncol(random_data))
random_data = as_tibble(random_data)


# CATEGORICALS ------------------------------------------------------------

# 2 correlated categoricals
categorical_data = tibble(
  x = sample(LETTERS[1:4], n, replace=T, prob=c(0.5, 0.2, 0.2, 0.1))
)
categorical_data = categorical_data %>% 
  mutate(
    y = case_when(
      x == "A" ~ "B",
      x == "B" ~ "C",
      x == "C" ~ "D",
      x == "D" ~ "A"
    ))
n_to_perturb = 0.15 * nrow(categorical_data)
idx_to_perturb = sample(n, n_to_perturb, rep=FALSE)
categorical_data[idx_to_perturb, "y"] = sample(
  LETTERS[1:4], n_to_perturb, replace=T)

names(categorical_data) = paste0("v", 1:ncol(categorical_data))


# BINARY+NUMERIC ----------------------------------------------------------

# 1 gamma correlated con 1 binaria
binary_cont_data = tibble(
  v1 = rgamma(n, 2, 3)
)
temp = binary_cont_data$v1 + rnorm(n, 0, .05)
binary_cont_data = binary_cont_data %>% 
  mutate(v2 = round(temp > mean(.$v1)))


# CATEGORICAL+NUMERIC ----------------------------------------------------------

# 1 categ correlated con 1 normal
cats = LETTERS[1:4]
x = sample(cats, n, rep=T, prob=c(0.1, 0.2, 0.65, 0.05))
y = rep(NA, length(x))
for (i in seq_along(cats)) {
  is_cat = x == cats[i]
  n_i = sum(is_cat)
  y[is_cat] = rnorm(n_i, mean=i, sd=1)
}

categorical_cont_data = tibble(v1=x, v2=y)


# NUMERICAL ---------------------------------------------------------------

# 2 correlated lineal en escala log
numerical_data = tibble(
  v1 = exp(runif(n, 1, 10)),
  v2 = 10 + log(v1) + rnorm(n, 0, 1)
)

# 2 uniform correlated lineal
numerical_data = numerical_data %>% 
  mutate(
  v3 = runif(n, -1, 1),
  v4 = 2 * v3 + rnorm(n, 0, .5) 
)

# 1 squared relacionada
numerical_data = numerical_data %>% 
  mutate(v5 = v3 ** 2 + rnorm(n, 0, .1))

# TARGET ------------------------------------------------------------------

# Utiles:
# 5 numerical + 2 dino + 1 categ + 1 numerica + 1 binaria + 1 numerica + 2 categ
# Ruido:
# 2 unif + 3 gamma + 2 categ

df = list(
  numerical_data %>% setNames(paste0("numeric_", names(.))),
  dino_data_expanded %>% setNames(paste0("dino_", names(.))),
  categorical_cont_data %>% setNames(paste0("categ_num_", names(.))),
  binary_cont_data %>% setNames(paste0("binary_num_", names(.))),
  categorical_data %>% setNames(paste0("categ_", names(.))),
  random_data %>% setNames(paste0("random_", names(.)))
) %>% bind_cols()

# make target
minmax = function(x) (x-min(x)) / (max(x)-min(x))
dummies = caret::dummyVars(" ~ .", data=df)
X = predict(dummies, newdata=df) %>% 
  data.frame() %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, minmax) %>%
  select(-starts_with("random_"))
betas = sample(0:1, ncol(X), replace=T)
target = as.vector(as.matrix(X) %*% betas)
betas_df = tibble(var=names(X), beta=betas)

# imbalanced binary target
target_centered = target - mean(target)
target_probas = 1 / (1 + exp(-target_centered))
p_80 = quantile(target_probas, .8)
target_binary = round(target_probas > p_80)

# add random Missing to 2 ruido (1cat/1num) and 2 relevant (1cat/1num)
cols = c("numeric_v1","categ_v1","random_v1","random_v7")

df = df %>% 
  mutate(
    categ_v1 = if_else(categ_v1 == "D", NA_character_, categ_v1),
    numeric_v1 = if_else(runif(nrow(.)) < .025, NA_real_, numeric_v1),
    random_v1 = if_else(runif(nrow(.)) < .05, NA_real_, random_v1),
    random_v7 = if_else(runif(nrow(.)) < .05, NA_character_, random_v7),
  )

# shuffle columns
shuffled_idx_cols = seq_along(df) %>% sample(replace=F)
df = df[shuffled_idx_cols]
names_df = tibble(var=names(df), name=paste0("v", 1:ncol(df)))
names(df) = names_df$name

# add target
df = df %>% 
  mutate(target=target_binary) %>% 
  select(target, everything())

# SAVE --------------------------------------------------------------------

write_csv(df, "clase_07/data.csv")
write_csv(names_df, "clase_07/names.csv")
write_csv(betas_df, "clase_07/betas.csv")

