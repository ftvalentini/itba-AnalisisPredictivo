
library(tidyverse)
library(hexbin)
library(energy)
library(dHSIC)
library(minerva)
library(sjstats)
library(rstatix)

n = 1000

x = runif(n, -1, 1)
y = x + rnorm(n, 0, .5)
z = sample(letters[1:4], n, rep=T)
u = sample(letters[1:4], n, rep=T)
df = data.frame(x, y, z, u)

# scatter plot ------------------------------------------------------------

ggplot(df) +
    geom_point(aes(x,y))

# "scatter plots" (para N grande) -----------------------------------------

ggplot(df) +
  geom_bin2d(aes(x, y)) +
  scale_fill_viridis_c()

# install.packages("hexbin")
ggplot(df) +
  geom_hex(aes(x, y)) +
  scale_fill_viridis_c()

ggplot(df, aes(x, y)) + 
  geom_jitter(aes(x, y))
  # geom_boxplot(aes(group=cut_number(x, 10)))
  # geom_boxplot(aes(group=cut_number(x, 10)))

# pearson -----------------------------------------------------------------

cor(x, y, method="pearson")

# spearman ----------------------------------------------------------------

cor(x, y, method="spearman")

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

# EpsilonSq (Kruskal-Wallis) ----------------------------------------------

res_kruskal = rstatix::kruskal_test(df, x ~ z)
epsilon_sq = res_kruskal$statistic / ((res_kruskal$n**2 - 1) / (res_kruskal$n + 1))

# Cramer's V (chi-square) ----------------------------------------------

tab = table(z, u)
as.matrix(tab) %>% rstatix::cramer_v()
