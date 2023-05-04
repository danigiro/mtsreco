library(tidyverse)
library(reshape2)
library(ggpubr)
load("./data/agg_gdp.Rdata")
load("./data/group_names.RData")

reco2tibble <- function(base_list, reco_array){
  reco <- melt(reco_array)
  colnames(reco) <- c("h", "id", "rep", "reco", "value")
  reco <- as_tibble(reco)
  
  scale <- melt(base_list$scale)
  colnames(scale) <- c("rep", "id", "scale")
  scale <- as_tibble(scale)
  
  test <- melt(base_list$test_array)
  colnames(test) <- c("h", "id", "rep", "test")
  test <- as_tibble(test)
  
  
  base <- melt(base_list$base_array)
  colnames(base) <- c("h", "id", "rep", "base")
  base <- as_tibble(base)
  
  out <- left_join(left_join(left_join(reco, test, by = c("h", "id", "rep")), 
                             base, by = c("h", "id", "rep")), 
                   scale, by = c("id", "rep"))
  
  out
}

load("./data/base.Rdata",  temp_env <- new.env())
base_list <- as.list(temp_env)

load("./data/reco_point.Rdata",  temp_env <- new.env())
reco_array <- as.list(temp_env)$reco_array

ea_results <- reco2tibble(base_list, reco_array)
ea_results <- na.omit(ea_results) %>%
  separate(id, c("geo", "NAc"), sep = " ")

score <- ea_results %>%
  group_by(geo, NAc, h, reco) %>%
  summarise(MSE = mean((test-value)^2),
            MSE_base = mean((test-base)^2),
            MASE = mean(abs(test-value)/scale),
            MASE_base = mean(abs(test-base)/scale))

score_geo_h1 <- ea_results %>%
  filter(h==1) %>%
  group_by(geo, h, reco) %>%
  summarise(MSE = mean((test-value)^2),
            MSE_base = mean((test-base)^2),
            MASE = mean(abs(test-value)/scale),
            MASE_base = mean(abs(test-base)/scale))%>%
  ungroup() %>%
  rename("type" = geo) %>%
  mutate(h = "h = 1",
         facet = "Geopgraphical division")

score_geo_h1_4 <- ea_results %>%
  group_by(geo, reco) %>%
  summarise(MSE = mean((test-value)^2),
            MSE_base = mean((test-base)^2),
            MASE = mean(abs(test-value)/scale),
            MASE_base = mean(abs(test-base)/scale))%>%
  ungroup() %>%
  rename("type" = geo) %>%
  mutate(h = "h = 1:4",
         facet = "Geopgraphical division")

Exp <- setNames(rep("Expenditure", length(unlist(names$Exp)[-1])), unlist(names$Exp)[-1])
Inc <- setNames(rep("Income", length(unlist(names$Inc)[-1])), unlist(names$Inc)[-1])
Pro <- setNames(rep("Output", length(unlist(names$Pro)[-1])), unlist(names$Pro)[-1])
nnn <- c(Exp, Inc, Pro)

score_na_h1 <- ea_results %>%
  filter(h==1) %>%
  mutate(NAc = recode(NAc, !!!nnn)) %>%
  group_by(NAc, h, reco) %>%
  summarise(MSE = mean((test-value)^2),
            MSE_base = mean((test-base)^2),
            MASE = mean(abs(test-value)/scale),
            MASE_base = mean(abs(test-base)/scale)) %>%
  ungroup() %>%
  rename("type" = NAc)%>%
  mutate(h = "h = 1",
         facet = "Economic system")
  
score_na_h1_4 <- ea_results %>%
  mutate(NAc = recode(NAc, !!!nnn)) %>%
  group_by(NAc, reco) %>%
  summarise(MSE = mean((test-value)^2),
            MSE_base = mean((test-base)^2),
            MASE = mean(abs(test-value)/scale),
            MASE_base = mean(abs(test-base)/scale)) %>%
  ungroup() %>%
  rename("type" = NAc)%>%
  mutate(h = "h = 1:4",
         facet = "Economic system")

library(plyr)
MSEplot_data <- rbind(score_na_h1, score_na_h1_4, score_geo_h1,score_geo_h1_4) %>%
  mutate(type = factor(type, rev(c("GDP", "Expenditure", "Income", "Output", names$Geo)), ordered = TRUE)) %>%
  filter(reco %in% c("wls", "shr", "ols"))  %>%
  mutate(type = recode(type, !!!names_geo))

MSEplot <- MSEplot_data %>%
  filter(1-MSE/MSE_base >= (-0.45)) %>%
  ggplot(aes(y = type, x = 1-MSE/MSE_base, col = reco, shape = reco)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0.05, 0.05),
                     breaks = seq(-0.6, 0.7, by = 0.15))+
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  geom_point(aes(x = -0.45), shape=17, size = 3, show.legend = FALSE, data = subset(MSEplot_data, 1-MSE/MSE_base < (-0.45))) +
  #geom_text(label = expression(symbol('\256')~ols), aes(x = -0.55), show.legend = FALSE, data = subset(prova, 1-MSE/MSE_base < (-0.55)))+
  facet_grid(facet~h, scales = "free", space = "free_y")+
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + labs(x="Skill Score (MSE) %", y=NULL)+
  theme_minimal()+
  coord_cartesian(xlim = c(-0.45, NA))+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.margin = margin(b = -10),
        text = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.margin = margin(b = 6),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.text.x = element_text(margin = margin(b = 3, t = 0), size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"))

ggsave(MSEplot, filename = "EA_GDP_MSE_h1_h14.pdf",
       width = 210,
       height = 180,
       units = "mm")

MASEplot <- MSEplot_data %>%
  filter(1-MASE/MASE_base >= (-0.30)) %>%
  ggplot(aes(y = type, x = 1-MASE/MASE_base, col = reco, shape = reco)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0.05, 0.05),
                     breaks = seq(-0.6, 0.7, by = 0.15))+
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  geom_point(aes(x = -0.30), shape=17, size = 3, show.legend = FALSE, data = subset(MSEplot_data, 1-MASE/MASE_base < (-0.30))) +
  #geom_text(label = expression(symbol('\256')~ols), aes(x = -0.55), show.legend = FALSE, data = subset(prova, 1-MSE/MSE_base < (-0.55)))+
  facet_grid(facet~h, scales = "free", space = "free_y")+
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + labs(x="Skill Score (MASE) %", y=NULL)+
  theme_minimal()+
  coord_cartesian(xlim = c(-0.30, NA))+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.margin = margin(b = -10),
        text = element_text(size = 9),
        axis.text = element_text(size = 9),
        plot.margin = margin(b = 6),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.text.x = element_text(margin = margin(b = 3, t = 0), size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"))

ggsave(MASEplot, filename = "EA_GDP_MASE_h1_h14.pdf",
       width = 210,
       height = 180,
       units = "mm")

library(sf)
library(scales)
library(rnaturalearth)
worldmap <- ne_countries(scale = 'medium', type = 'countries',
                         returnclass = 'sf')

names_geo <- c("AT" = "Austria", "BE" = "Belgium", "CY" = "Cyprus", "EE" = "Estonia", 
               "FI" = "Finland", "FR" = "France", "DE" = "Germany", "EL" = "Greece", 
               "IE" = "Ireland", "IT" = "Italy", "LV" = "Latvia", "LT" = "Lithuania", 
               "LU" = "Luxembourg", "MT" = "Malta", "NL" = "Netherlands", "PT" = "Portugal",
               "SI" = "Slovenia", "SK" = "Slovakia", "ES" = "Spain")
geo_tibble <- ea_results %>%
  group_by(geo, h, reco) %>%
  filter(geo != "EA19", NAc == "GDP") %>%
  dplyr::summarise(MSE = mean((test-value)^2),
            MSE_base = mean((test-base)^2),
            MASE = mean(abs(test-value)/scale),
            MASE_base = mean(abs(test-base)/scale)) %>%
  mutate(value = 1-MSE/MSE_base) %>%
  filter(h == 1) %>%
  ungroup() %>%
  select(geo, reco, value) %>%
  pivot_wider(names_from = reco)

data_plot <- geo_tibble %>% 
  mutate(admin = recode(geo, !!!names_geo)) %>% 
  full_join(worldmap, by = "admin") %>% 
  #pivot_longer(c("shr", "wls"), names_to = "reco") %>%
  st_as_sf()

geo_plot <- data_plot %>% ggplot() + 
  geom_sf(aes(fill = 100*wls), size = 0.1, color = "white") + 
  scale_x_continuous(expand = c(0.1,0.1), breaks = seq(-10, 40, 10)) +
  scale_y_continuous(breaks = seq(30, 70, 10))+
  coord_sf(xlim = c(2750000, 6500000), ylim = c(1530000, 5000000), expand = TRUE,
           crs = 3035)+
  ggrepel::geom_label_repel(aes(label = scales::percent(wls, accuracy = 1), 
                                geometry = geometry),
                            seed = 1234,
                            stat = "sf_coordinates", min.segment.length = 0, size = 2) +
  ggrepel::geom_label_repel(aes(label = scales::percent(wls, accuracy = 1), 
                                geometry = geometry, fill = 100*wls),
                            seed = 1234, alpha = 0.3,
                            stat = "sf_coordinates", min.segment.length = 0, size = 2) +
  scale_color_gradient2(low = "#cf2a10", mid = "#b5ba61", high = "#7c8d4c",
                        midpoint = 5,name = "% MSE", #na.value = NA, breaks = seq(-1,25,5),
                        guide = guide_colorbar(direction = "vertical", 
                                               title.position = "top",
                                               label.position = "right",
                                               barwidth = unit(0.4, "cm"),
                                               barheight = unit(4, "cm"),
                                               ticks = TRUE))+
  scale_fill_gradient2(low = "#cf2a10", mid = "#b5ba61", high = "#7c8d4c", 
                       midpoint = 5, name = "% MSE", #na.value = NA, breaks = seq(-1,25,5),
                       guide = guide_colorbar(direction = "vertical", 
                                              title.position = "top",
                                              label.position = "right", 
                                              barwidth = unit(0.4, "cm"),
                                              barheight = unit(4, "cm"), ticks = TRUE)) +
  theme_minimal() + labs(x=NULL, y =NULL) +
  theme(legend.text = element_text(size = 8),
        strip.text = element_text(size = 9, face = "bold"),
        strip.background = element_rect(fill = NA, color = "gray80", size = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        text = element_text(size = 9),
        panel.border = element_rect(color = "gray80", fill = NA, size = 1),
        panel.grid.minor = element_line(),
        plot.title.position="plot",
        plot.title = element_text(face = "bold"))
geo_plot
ggsave(geo_plot, filename = "EA_GDP_MSE_MAP_h1_wls.pdf",
       width = 210,
       height = 160,
       units = "mm")


geo_tibble_MASE <- ea_results %>%
  group_by(geo, h, reco) %>%
  filter(geo != "EA19", NAc == "GDP") %>%
  dplyr::summarise(MSE = mean((test-value)^2),
                   MSE_base = mean((test-base)^2),
                   MASE = mean(abs(test-value)/scale),
                   MASE_base = mean(abs(test-base)/scale)) %>%
  mutate(value = 1-MASE/MASE_base) %>%
  filter(h == 1) %>%
  ungroup() %>%
  select(geo, reco, value) %>%
  pivot_wider(names_from = reco)

data_plot_MASE <- geo_tibble %>% 
  mutate(admin = recode(geo, !!!names_geo)) %>% 
  full_join(worldmap, by = "admin") %>% 
  #pivot_longer(c("shr", "wls"), names_to = "reco") %>%
  st_as_sf()

geo_plot_MASE <- data_plot_MASE %>% ggplot() + 
  geom_sf(aes(fill = 100*wls), size = 0.1, color = "white") + 
  scale_x_continuous(expand = c(0.1,0.1), breaks = seq(-10, 40, 10)) +
  scale_y_continuous(breaks = seq(30, 70, 10))+
  coord_sf(xlim = c(2750000, 6500000), ylim = c(1530000, 5000000), expand = TRUE,
           crs = 3035)+
  ggrepel::geom_label_repel(aes(label = scales::percent(wls, accuracy = 1), 
                                geometry = geometry),
                            seed = 1234,
                            stat = "sf_coordinates", min.segment.length = 0, size = 2) +
  ggrepel::geom_label_repel(aes(label = scales::percent(wls, accuracy = 1), 
                                geometry = geometry, fill = 100*wls),
                            seed = 1234, alpha = 0.3,
                            stat = "sf_coordinates", min.segment.length = 0, size = 2) +
  scale_color_gradient2(low = "#cf2a10", mid = "#b5ba61", high = "#7c8d4c",
                        midpoint = 5,name = "% MASE", #na.value = NA, breaks = seq(-1,25,5),
                        guide = guide_colorbar(direction = "vertical", 
                                               title.position = "top",
                                               label.position = "right",
                                               barwidth = unit(0.4, "cm"),
                                               barheight = unit(4, "cm"),
                                               ticks = TRUE))+
  scale_fill_gradient2(low = "#cf2a10", mid = "#b5ba61", high = "#7c8d4c", 
                       midpoint = 5, name = "% MASE", #na.value = NA, breaks = seq(-1,25,5),
                       guide = guide_colorbar(direction = "vertical", 
                                              title.position = "top",
                                              label.position = "right", 
                                              barwidth = unit(0.4, "cm"),
                                              barheight = unit(4, "cm"), ticks = TRUE)) +
  theme_minimal() + labs(x=NULL, y =NULL) +
  theme(legend.text = element_text(size = 8),
        strip.text = element_text(size = 9, face = "bold"),
        strip.background = element_rect(fill = NA, color = "gray80", size = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        text = element_text(size = 9),
        panel.border = element_rect(color = "gray80", fill = NA, size = 1),
        panel.grid.minor = element_line(),
        plot.title.position="plot",
        plot.title = element_text(face = "bold"))

ggsave(geo_plot_MASE, filename = "EA_GDP_MASE_MAP_h1_wls.pdf",
       width = 210,
       height = 160,
       units = "mm")
