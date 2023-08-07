
library(tidyverse)
library(hexbin)
library(energy)
library(dHSIC)
library(minerva)
library(sjstats)
library(rstatix)

n = 1000

set.seed(33)
x = runif(n, -1, 1)
y = 0 + 1*x + rnorm(n, 0, .5)
z = sample(letters[1:4], n, rep=T)
u = sample(letters[1:4], n, rep=T)
df = data.frame(x, y, z, u)

plot(x, y)

# scatter plot ------------------------------------------------------------

ggplot(df) +
    geom_point(aes(x,y), alpha=0.5, size=0.5)

# "scatter plots" (para N grande) -----------------------------------------

ggplot(df) +
  geom_bin2d(aes(x, y)) +
  scale_fill_viridis_c()

# install.packages("hexbin")
ggplot(df) +
  geom_hex(aes(x, y)) +
  scale_fill_viridis_c()

# para despegar puntitos pegados:
ggplot(df, aes(x, y)) + 
  geom_jitter(aes(x, y))

ggplot(df, aes(y, x)) + 
  geom_boxplot(aes(group=cut_number(x, 10)))

# pearson -----------------------------------------------------------------

cor(x, y, method="pearson")

# spearman ----------------------------------------------------------------

cor(x, y, method="spearman")
cor(rank(x), rank(y), method="pearson")

# dCor ----------------------------------------------------------------

energy::dcor(x, y)

# HSIC ----------------------------------------------------------------

dHSIC::dhsic(x, y)$dHSIC

# MIC ----------------------------------------------------------------

minerva::mine(x, y)$MIC

# OmegaSq (ANOVA) ---------------------------------------------------------

anova_res = aov(x ~ z, df)
anova_tab = anova_res %>% sjstats::anova_stats() 
anova_tab$omegasq[1]

ggplot(df) + 
  geom_boxplot(aes(x=z, y=x))

# EpsilonSq (Kruskal-Wallis) ----------------------------------------------

res_kruskal = rstatix::kruskal_test(df, x ~ z)
epsilon_sq = res_kruskal$statistic / 
  ((res_kruskal$n**2 - 1) / (res_kruskal$n + 1))

# Cramer's V (chi-square) ----------------------------------------------

tab = table(z, u)
as.matrix(tab) %>% rstatix::cramer_v()

# pvalue vs effect size ---------------------------------------------------

set.seed(33)
y_china = rnorm(n=1000000, mean=1000)
y_taiwan = rnorm(n=800000, mean=1010)

(mean(y_taiwan) / mean(y_china) - 1) * 100

# H0: medias iguales
# H1: medias difieren
t.test(y_china, y_taiwan)
# pvalue bajo (significativo) pero efecto pequeño (dif=1%) 


# OJO CON LA AUSENCIA DE CORRELACIÓN! -------------------------------------

df = datasauRus::datasaurus_dozen %>% 
  filter(dataset == "slant_up")
ggplot(df) + 
  geom_point(aes(x, y), size=1) +
  labs(x="edad", y="ingreso (target)") +
  NULL
# x: edad, y: ingreso, color: sector_actividad
cor(df$x, df$y, method="spearman")

