library(tidyverse)
load("./data/agg_gdp_sep.RData")
rownames(Cinc)[1] <- rownames(Cexp)[1] <- rownames(Cprod)[1] <- "GDP"
plot2 <- ggplot() +
  geom_tile(aes(y = rev(Var1), x = Var2, fill = as.character(value)), 
            data = reshape2::melt(Cinc), colour = "black", size = 0.1) +
  scale_y_discrete(labels = rev(rownames(Cinc)))+
  labs(title = "Income", caption = "(b)") + 
  scale_fill_manual(values = c("#9b2226", "white", "#adc178"))+
  coord_fixed() + 
  theme_void()+
  theme(legend.position = "none",
        plot.caption = element_text(size = 10, hjust = 0.5),
        legend.title = element_blank(),
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8, hjust=0.95),
        axis.text.x = element_text(size = 8, angle = 90, hjust=0.95,vjust=0.2),
        plot.margin = unit(rep(0.25, 4), "cm"))

plot3 <- ggplot() +
  geom_tile(aes(y = rev(Var1), x = Var2, fill = as.character(value)), 
            data = reshape2::melt(Cexp), colour = "black", size = 0.1) +
  scale_y_discrete(labels = rev(rownames(Cexp)))+
  labs(title = "Expenditure", caption = "(c)") + 
  scale_fill_manual(values = c("#9b2226", "white", "#adc178"))+
  coord_fixed() + 
  theme_void()+
  theme(legend.position = "right",
        plot.caption = element_text(size = 10, hjust = 0.5),
        legend.title = element_blank(),
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8, hjust=0.95),
        axis.text.x = element_text(size = 8, angle = 90, hjust=0.95,vjust=0.2),
        plot.margin = unit(rep(0.25, 4), "cm"))

plot1 <- ggplot() +
  geom_tile(aes(y = rev(Var1), x = Var2, fill = as.character(value)), 
            data = reshape2::melt(Cprod), colour = "black", size = 0.1) +
  scale_y_discrete(labels = rev(rownames(Cprod)))+
  labs(title = "Output", caption = "(a)") + 
  scale_fill_manual(values = c( "#adc178"))+
  coord_fixed() + 
  theme_void()+
  theme(legend.position = "none",
        plot.caption = element_text(size = 10, hjust = 0.5),
        plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 8, hjust=0.95),
        axis.text.x = element_text(size = 8, angle = 90, hjust=0.95,vjust=0.2),
        plot.margin = unit(rep(0.25, 4), "cm"))

PLOT_F <- cowplot::plot_grid(cowplot::plot_grid(plot1, plot2, rel_heights = c(1,1.5), nrow = 2), 
                             plot3, nrow = 1, rel_widths = c(1,2))
ggsave(PLOT_F, filename = "CMAT.pdf",
       width = 200,
       height = 110,
       units = "mm")
