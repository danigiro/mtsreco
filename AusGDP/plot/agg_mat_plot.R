library(tidyverse)
load("./aus/Cmat.RData")
rownames(Cgdp)[1] <-  "GDP"
PLOT_F <- ggplot() +
  geom_tile(aes(y = rev(Var1), x = Var2, fill = as.character(value)), 
            data = reshape2::melt(as.matrix(Cgdp)), colour = "grey25", size = 0.01) +
  scale_y_discrete(labels = NULL)+
  scale_x_discrete(labels = NULL)+
  labs(y = NULL, x = NULL) + 
  scale_fill_manual(values = c("#9b2226", "white", "#adc178"))+
  coord_fixed() + 
  theme_void()+
  theme(legend.position = "bottom",
    plot.caption = element_text(size = 10, hjust = 0.5),
        legend.title = element_blank(),
    legend.text = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8, hjust=0.95),
        axis.text.x = element_text(size = 8, angle = 90, hjust=0.95,vjust=0.2),
    plot.margin = margin(b = 10, t = 10, r = 10, l = 10))

colnames(Cgdp)
ggsave(PLOT_F, filename = "aus_MAT.pdf",
       width = 300,
       height = 165,
       units = "mm")
