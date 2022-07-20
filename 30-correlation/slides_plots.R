
library(tidyverse)

set.seed(33)

# classic --------------------------------------------------------------

n = 100

df_numeric = tibble(
  x1 = runif(100, 0, 1),
  x2 = x1 + rnorm(n, 0, .1),
  x3 = x1 + rnorm(n, 0, 1),
  x4 = x1**5 + rnorm(n, 0, .01),
  x5 = x2,
  rank_x1 = rank(x1),
  rank_x4 = rank(x4),
  flag = round(x2, 0)
)
idx_outliers = c(2,50,90) 
df_numeric[idx_outliers, "x5"] = df_numeric[idx_outliers, "x5"] + 2

plot_scatter = function(df, x, y, method="pearson") {
  corr = cor(df[[x]], df[[y]], method=method) %>% round(3)
  df %>% 
    ggplot() + 
    geom_point(aes_string(x, y)) +
    geom_vline(xintercept=mean(df[[x]]), linetype="dashed", color="red") +
    geom_hline(yintercept=mean(df[[y]]), linetype="dashed", color="red") +
    labs(subtitle=paste0(method, " = ", corr)) +
    theme_minimal() +
    NULL
}

plt_1 = plot_scatter(df_numeric, "x1", "x2")
plt_1
plt_2 = plot_scatter(df_numeric, "x1", "x3")
plt_2
plt_3 = plot_scatter(df_numeric, "x1", "x4")
plt_3
plt_3 = plot_scatter(df_numeric, "x1", "x4")
plt_3
plt_4 = plot_scatter(df_numeric, "x1", "x4", method="spearman")
plt_4
plt_5 = plot_scatter(df_numeric, "rank_x1", "rank_x4", method="pearson")
plt_5
plt_6 = plot_scatter(df_numeric, "x1", "flag", method="pearson")
plt_6
plt_7 = plot_scatter(df_numeric, "x1", "x5", method="pearson")
plt_7


ggsave("clase_03/img/scatter_1.png", plt_1, width=5, height=4)
ggsave("clase_03/img/scatter_2.png", plt_2, width=5, height=4)
ggsave("clase_03/img/scatter_3.png", plt_3, width=5, height=4)
ggsave("clase_03/img/scatter_4.png", plt_4, width=5, height=4)
ggsave("clase_03/img/scatter_5.png", plt_5, width=5, height=4)
ggsave("clase_03/img/scatter_6.png", plt_6, width=5, height=4)
ggsave("clase_03/img/scatter_7.png", plt_7, width=5, height=4)


df_short = data.frame(
  x = sample(10:99, 6, replace=T)
)
df_short$rank_x = rank(df_short$x)
df_short


# spearman es mas robusta a outliers en cor(binaria,continua)
cor(df_numeric$x5, df_numeric$flag, method="spearman")
plot(rank(df_numeric$x5), rank(df_numeric$flag))
cor(df_numeric$x5, df_numeric$flag, method="pearson")
plot(df_numeric$x5, df_numeric$flag)
cor(df_numeric$x5, df_numeric$flag, method="kendall") # ???
cor(rank(df_numeric$x5), rank(df_numeric$flag))


# anova -------------------------------------------------------------------

n = 200

df_anova = tibble(x1 = sample(letters[1:4], n, rep=T))

df_anova = df_anova %>% mutate(
  x2 = case_when(
    x1 == "a" ~ rnorm(n, 10, 1),
    x1 == "c" ~ rnorm(n, 5, 1),
    TRUE ~ rnorm(n, 1, 2)
  ),
  x3 = rnorm(n, 5, 1)
)

plot_anova = function(df, x, y) {
  df %>% 
    ggplot() +
    geom_jitter(aes_string(x, y), color="black", alpha=.5, size=.5) +
    geom_boxplot(aes_string(x, y, color=x, fill=x), alpha=.2, outlier.shape=NA) +
    theme_minimal() +
    theme(legend.position="none") +
    NULL
}

anova_table = function(df, x, y, full=F) {
  form <- as.formula(paste0(x, " ~ ", y))
  anova = aov(form, df)
  tab = anova %>% 
    sjstats::anova_stats() 
  if (!full) {
    tab = tab %>% select(term,df,sumsq,meansq,statistic,p.value)
  }
  tab %>%
    knitr::kable() %>% 
    kableExtra::kable_styling(full_width=FALSE)
  
}

plt_anova_1 = plot_anova(df_anova, "x1", "x2")
plt_anova_2 = plot_anova(df_anova, "x1", "x3")

ggsave("clase_03/img/anova_1.png", plt_anova_1, width=6, height=3)
ggsave("clase_03/img/anova_2.png", plt_anova_2, width=6, height=3)

(tab1 = anova_table(df_anova, "x2", "x1"))
(tab2 = anova_table(df_anova, "x3", "x1"))

anova_table(df_anova, "x2", "x1", full=T) #0.86
anova_table(df_anova, "x3", "x1", full=T) #0.01



# cramer ------------------------------------------------------------------

library(janitor)

df_cat = tibble(
  equipo = c(rep("Boca", 190), rep("River", 16), rep("Otros", 18),
             rep("Boca", 11), rep("River", 150), rep("Otros", 15)),
  region = c(rep("Sur", 190), rep("Sur", 16), rep("Sur", 18),
             rep("Norte", 11), rep("Norte", 150), rep("Norte", 15))
)

# matrix(c(200, 20, 20, 150, 10, 10), nrow = 2)

tab = df_cat %>% 
  mutate(equipo = fct_inorder(equipo)) %>% 
  tabyl(region, equipo)

tab %>% 
  adorn_totals(c("row","col")) %>%
  adorn_title("combined") %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling(full_width=FALSE)

res = chisq.test(tab)
res$expected %>% 
  adorn_rounding(1) %>% 
  adorn_totals(c("row","col")) %>%
  adorn_title("combined") %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling(full_width=FALSE)

tab %>% 
  adorn_totals("col") %>%
  adorn_title("combined") %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling(full_width=FALSE)

tab %>% 
  adorn_totals("row") %>%
  adorn_title("combined") %>% 
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling(full_width=FALSE)

tab[,-1] %>% 
  as.matrix() %>% 
  rstatix::cramer_v() %>%
  round(3)



# DCORR -------------------------------------------------------------------

library(tidyverse)

dfa = tibble(x=1:4, y=1:4, obs=LETTERS[1:4])
dfb = tibble(x=c(1,1,4,4), y=c(1,4,1,4), obs=LETTERS[1:4])
dfc = tibble(x=1:5, y=c(3,2,1,2,3), obs=LETTERS[1:5])

df_list = list(dfa, dfb, dfc)
scatter_list = list()

for (i in seq_along(df_list)) {
  scatter_list[[i]] = ggplot(df_list[[i]]) +
    geom_text(aes(x, y, label=obs)) +
    theme_minimal() + 
    NULL
}

tabs_raw = vector("list", length=length(df_list))
tabs_centered = vector("list", length=length(df_list))
tabs_obs = list()
for (i in seq_along(df_list)) {
  df_ = df_list[[i]]
  dx = dist(df_$x) %>% as.matrix()
  dimnames(dx) = list(df_$obs, df_$obs)
  dy = dist(df_$y) %>% as.matrix()
  dimnames(dy) = dimnames(dx)
  dx_centered = energy::D_center(dx) # promedio 0 gral, por fila y por columna
  dy_centered = energy::D_center(dy)
  dimnames(dx_centered) = dimnames(dx)
  dimnames(dy_centered) = dimnames(dx)
  cov_matrix = (dx_centered * dy_centered) / (nrow(df_) ** 2)
  tabs_raw[[i]][["raw_x"]] = dx %>% knitr::kable() %>% kableExtra::kable_styling(full_width=F)
  tabs_raw[[i]][["raw_y"]] = dy %>% knitr::kable() %>% kableExtra::kable_styling(full_width=F)
  tabs_centered[[i]][["cent_x"]] = dx_centered %>% knitr::kable() %>% 
    kableExtra::kable_styling(full_width=F)
  tabs_centered[[i]][["cent_y"]] = dy_centered %>% knitr::kable() %>% 
    kableExtra::kable_styling(full_width=F)
  tabs_obs[[i]] = cov_matrix %>% apply(1, sum) %>% t() %>% 
    knitr::kable() %>% kableExtra::kable_styling(full_width=F) # equals a suma por col
}

tabs_raw[[1]]
tabs_centered[[1]]
tabs_obs[[1]]
energy::dcor(dfa$x, dfa$y)

tabs_raw[[2]]
tabs_centered[[2]]
tabs_obs[[2]]
energy::dcor(dfb$x, dfb$y)

tabs_raw[[3]]
tabs_centered[[3]]
tabs_obs[[3]]
energy::dcor(dfc$x, dfc$y)


# sum(cov_matrix)
# energy::dcov(df_a$x, df_a$y) ** 2

