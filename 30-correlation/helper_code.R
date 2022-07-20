

library(tidyverse)

# data --------------------------------------------------------------------

# df = tibble(
#   x1 = runif(100, 0, 1),
#   x2 = runif(100, 0, 1),
#   x3 = runif(100, 0, 1),
#   c1 = sample(letters[1:4], 100, rep=T),
#   c2 = sample(letters[1:4], 100, rep=T),
#   b1 = sample(0:1, 100, rep=T),
#   b2 = sample(0:1, 100, rep=T)
# )

df = read_csv("clase_03/data/data.csv")


# metrics -----------------------------------------------------------------

names_num = df %>% select_if(
  function(x) is.numeric(x) & length(unique(x)) > 2) %>% names()
names_cat = df %>% select_if(
  function(x) !is.numeric(x)) %>% names()
names_bin = df %>% select_if(
  function(x) is.numeric(x) & length(unique(x)) == 2) %>% names()

numeric_cors = function(df, col_names, method="spearman") {
  cor_mat = cor(df[col_names], method=method) 
  cor_mat[upper.tri(cor_mat)] = NA
  df_cor = cor_mat %>% 
    as.table() %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    filter((!is.na(Freq)) & (Var1 != Var2)) %>% 
    rename(!!sym(method) := Freq) %>% 
    arrange(-!!sym(method))
  return(df_cor)
}

omega_sq = function(df, x_num, x_cat) {
  form = as.formula(paste0(x_num, " ~ ", x_cat))
  anova = aov(form, df)
  tab = anova %>% sjstats::anova_stats() 
  res = tab$omegasq[1]
  return(res)
}

epsilon_sq = function(df, x_num, x_cat) {
  form = as.formula(paste0(x_num, " ~ ", x_cat))
  res_kruskal = rstatix::kruskal_test(df, form)
  res = res_kruskal$statistic / ((res_kruskal$n**2 - 1) / (res_kruskal$n + 1))
  return(res)
}

cramer_v = function(df, x1, x2) {
  tab = table(df[[x1]], df[[x2]])
  res = as.matrix(tab) %>% rstatix::cramer_v()
  return(res)
}

omega_cors = function(df, numeric_col_names, categ_col_names) {
  df_cor = expand_grid(Var1=numeric_col_names, Var2=categ_col_names)
  df_cor = df_cor %>% mutate(
    omega_sq = map2_dbl(Var1, Var2, function(x,y) omega_sq(df, x, y))
  ) %>% arrange(-omega_sq)
  return(df_cor)
}

epsilon_cors = function(df, numeric_col_names, categ_col_names) {
  df_cor = expand_grid(Var1=numeric_col_names, Var2=categ_col_names)
  df_cor = df_cor %>% mutate(
    epsilon_sq = map2_dbl(Var1, Var2, function(x,y) epsilon_sq(df, x, y))
  ) %>% arrange(-epsilon_sq)
  return(df_cor)
}

cramer_cors = function(df, col_names) {
  df_cor = combn(col_names, 2) %>% 
    t() %>% 
    as.data.frame() %>% 
    set_names(c("Var1", "Var2"))
  df_cor = df_cor %>% mutate(
    cramer_v = map2_dbl(Var1, Var2, function(x,y) cramer_v(df, x, y))
  ) %>% arrange(-cramer_v)
  return(df_cor)
}

dcor_cors = function(df, col_names) {
  df_cor = combn(col_names, 2) %>% 
    t() %>% 
    as.data.frame() %>% 
    set_names(c("Var1", "Var2")) %>% 
    as_tibble() 
  df_cor = df_cor %>% mutate(
    dcor = map2_dbl(Var1, Var2, function(x,y) energy::dcor(df[[x]], df[[y]]))
  ) %>% arrange(-dcor)
  return(df_cor)
}

mic_cors = function(df, col_names) {
  df_cor = combn(col_names, 2) %>% 
    t() %>% 
    as.data.frame() %>% 
    set_names(c("Var1", "Var2"))
  df_cor = df_cor %>% mutate(
    mic = map2_dbl(Var1, Var2, function(x,y) minerva::mine(df[[x]], df[[y]])$MIC)
  ) %>% arrange(-mic)
  return(df_cor)
}

numeric_cors(df, c(names_num, names_bin), method="spearman")
epsilon_cors(df, names_num, names_cat)
cramer_cors(df, c(names_cat, names_bin)) # warning con conteos pequeños?
dcor_cors(df, names_num)
# omega_cors(df, names_num, names_cat)
# mic_cors(df, names_num)
