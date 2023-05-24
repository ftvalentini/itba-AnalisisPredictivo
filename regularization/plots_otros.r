
library(ggplot2)
library(grid)

# norms l1 y l2 -----------------------------------------------------------

df = data.frame(
  x = c(2, 1),
  y = c(5, 4),
  model = c("A", "B")
)

p_vectores = ggplot(df) + 
  geom_segment(aes(x=0, y=0, xend=x, yend=y), arrow=arrow(), size=.7) + 
  geom_text(aes(x, y, label=model), nudge_x=.07, nudge_y=.05, color="blue", size=5) +
  geom_point(aes(x, y), color="blue", size=2) +
  labs(x=expression(beta[1]), y=expression(beta[2])) +
  theme_minimal() +
  NULL

ggsave("clase_08/img/vectores.png", plot=p_vectores, width=4, height=3)



# Add an arrow
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line(arrow = arrow())+
  geom_point()
# Add a closed arrow to the end of the line
myarrow=arrow(angle = 15, ends = "both", type = "closed")
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line(arrow=myarrow)+
  geom_point()







# regression N=1 and N=2 --------------------------------------------------

df = data.frame(x=0:1, y=c(2,4))

p2n2 = ggplot(df) + 
  geom_point(aes(x, y), size=4) + 
  # theme_minimal() + 
  lims(x=c(-0.5,1.5), y=c(1,5)) +
  NULL

ggsave("clase_08/img/p2n2.png", plot=p2n2, width=4, height=3)

p2n1 = ggplot(df[1,]) + 
  geom_point(aes(x, y), size=4) + 
  # theme_minimal() + 
  lims(x=c(-0.5,1.5), y=c(1,5)) +
  NULL

ggsave("clase_08/img/p2n1.png", plot=p2n1, width=4, height=3)
