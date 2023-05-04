library(tidyverse)
library(reshape2)
library(ggpubr)
load("./data/agg_gdp.Rdata")
load("./data/prob_scores_boot.RData")
load("./data/group_names.RData")
# Fully reconciled
ea_crps_data <- tibble(melt(simplify2array(ea_crps)))
colnames(ea_crps_data) <- c("id", "h", "reco", "rep", "value")
ea_crps_data <- ea_crps_data %>%
  separate(id, c("geo", "NAc"), sep = " ")
ea_crps_data <- na.omit(ea_crps_data)

ea_es_data <- tibble(melt(simplify2array(ea_es)))
colnames(ea_es_data) <- c("type", "h", "group", "reco", "rep", "value")
ea_es_data <- na.omit(ea_es_data)

ea_vs_data <- tibble(melt(simplify2array(ea_vs)))
colnames(ea_vs_data) <- c("type", "h", "group", "reco", "rep", "value")
ea_vs_data <- na.omit(ea_vs_data)

score_es <- ea_es_data %>%
  group_by(h, reco, group, type) %>%
  dplyr::summarise(value = mean(value)) 

score_es_base <- score_es %>%
  ungroup() %>%
  filter(reco == "base") %>%
  dplyr::rename(base = value) %>%
  select(-reco)

names_geo <- c("AT" = "Austria", "BE" = "Belgium", "CY" = "Cyprus", "EE" = "Estonia", 
               "FI" = "Finland", "FR" = "France", "DE" = "Germany", "EL" = "Greece", 
               "IE" = "Ireland", "IT" = "Italy", "LV" = "Latvia", "LT" = "Lithuania", 
               "LU" = "Luxembourg", "MT" = "Malta", "NL" = "Netherlands", "PT" = "Portugal",
               "SI" = "Slovenia", "SK" = "Slovakia", "ES" = "Spain")
score_es_plot <- rbind(full_join(score_es, score_es_base) %>%
                         filter(type == "All", group %in% names$Geo) %>%
                         mutate(type = "Geopgraphical division",
                                group = recode(group, !!!names_geo)),
                       full_join(score_es, score_es_base) %>%
                         filter(type == "All (no GDP)", group %in% c("Exp", "Inc", "Pro")) %>%
                         mutate(type = "Economic system",
                                group = recode(group, "Exp" = "Expenditure", "Inc" = "Income", "Pro" = "Output")),
                       full_join(score_es, score_es_base) %>%
                         filter(type == "GDP", group == "all") %>%
                         mutate(type = "Economic system", group = "GDP")) 

score_vs <- ea_vs_data %>%
  group_by(h, reco, group, type) %>%
  dplyr::summarise(value = mean(value)) 

score_vs_base <- score_vs %>%
  ungroup() %>%
  filter(reco == "base") %>%
  dplyr::rename(base = value) %>%
  select(-reco)

score_vs_plot <- rbind(full_join(score_vs, score_vs_base) %>%
                         filter(type == "All", group %in% names$Geo) %>%
                         mutate(type = "Geopgraphical division",
                                group = recode(group, !!!names_geo)),
                       full_join(score_vs, score_vs_base) %>%
                         filter(type == "All (no GDP)", group %in% c("Exp", "Inc", "Pro")) %>%
                         mutate(type = "Economic system",
                                group = recode(group, "Exp" = "Expenditure", "Inc" = "Income", "Pro" = "Output")),
                       full_join(score_vs, score_vs_base) %>%
                         filter(type == "GDP", group == "all") %>%
                         mutate(type = "Economic system", group = "GDP")) 

score_es_plot_h <- score_es_plot %>%
  mutate(sk = 1-value/base) %>%
  mutate(h = paste0("h = ", h))

plot_es_data_h <- score_es_plot_h %>%
  mutate(group = factor(group, c("GDP", "Expenditure", "Income", "Output", "EA19", sort(names_geo)), ordered = TRUE)) %>%
  filter(reco %in% c("wls", "shr", "ols")) 

plot_es_data_h$type[plot_es_data_h$group == "GDP"] <- ""
plot_es_data_h$type[plot_es_data_h$group %in% c("Expenditure", "Income", "Output")] <- "Sides"
plot_es_data_h$type[!(plot_es_data_h$group %in% c("GDP", "Expenditure", "Income", "Output"))] <- "Countries"

plot_es_data_h <- plot_es_data_h %>%
  mutate(type = factor(type, c("", "Sides", "Countries"), ordered = TRUE))
plot_es_h <- plot_es_data_h %>%
  filter(sk >= (-0.30)) %>%
  mutate(reco = factor(reco, c("ols", "wls", "shr"), ordered = TRUE)) %>%
  ggplot(aes(x = group, y = sk, col = reco, shape = reco)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0.05, 0.05),
                     breaks = seq(-0.6, 0.7, by = 0.1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  geom_point(aes(y = -0.30), shape=17, size = 3, show.legend = FALSE, data = subset(plot_es_data_h, sk < (-0.30))) +
  facet_grid(h~type, scales = "free", space = "free_x")+
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + labs(y="ES (%)", x=NULL)+
  theme_minimal()+
  coord_cartesian(ylim = c(-0.30, NA))+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.margin = margin(b = -10),
        text = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.ticks = element_line(),
        axis.text.x = element_text(size = 9, angle = 90, hjust=1, vjust = 0.5),
        plot.margin = margin(b = 6, l = 6),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.text.x = element_text(margin = margin(b = 3, t = 0), size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"))

ggsave(plot_es_h, filename = "EA_BOOT_ES_h.pdf",
       width = 210,
       height = 200,
       units = "mm")


score_es_plot_h1_4 <- score_es_plot %>%
  group_by(reco, group, type) %>%
  dplyr::summarise(sk = 1-mean(value)/mean(base)) %>%
  mutate(h = "h = 1:4")


score_es_data_h1_4 <- score_es_plot_h1_4 %>%
  mutate(group = factor(group, c("GDP", "Expenditure", "Income", "Output", "EA19", sort(names_geo)), ordered = TRUE)) %>%
  filter(reco %in% c("wls", "shr", "ols")) 
score_es_data_h1_4$type[score_es_data_h1_4$group == "GDP"] <- ""
score_es_data_h1_4$type[score_es_data_h1_4$group %in% c("Expenditure", "Income", "Output")] <- "Sides"
score_es_data_h1_4$type[!(score_es_data_h1_4$group %in% c("GDP", "Expenditure", "Income", "Output"))] <- "Countries"

score_es_data_h1_4 <- score_es_data_h1_4 %>%
  mutate(type = factor(type, c("", "Sides", "Countries"), ordered = TRUE))
plot_es_h1_4 <- score_es_data_h1_4 %>%
  filter(sk >= (-0.30)) %>%
  mutate(reco = factor(reco, c("ols", "wls", "shr"), ordered = TRUE)) %>%
  ggplot(aes(x = group, y = sk, col = reco, shape = reco)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0.05, 0.05),
                     breaks = seq(-0.6, 0.7, by = 0.1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  geom_point(aes(y = -0.30), shape=17, size = 3, show.legend = FALSE, data = subset(score_es_data_h1_4, sk < (-0.30))) +
  facet_grid(h~type, scales = "free", space = "free_x")+
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + labs(y="ES (%)", x=NULL)+
  theme_minimal()+
  coord_cartesian(ylim = c(-0.30, NA))+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.margin = margin(b = -10),
        text = element_text(size = 9),
        axis.ticks = element_line(),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(size = 9, angle = 90, hjust=1, vjust = 0.5),
        plot.margin = margin(b = 6, l = 6),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.text.x = element_text(margin = margin(b = 3, t = 0), size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"))

ggsave(plot_es_h1_4, filename = "EA_BOOT_ES_h14.pdf",
       width = 210,
       height = 90,
       units = "mm")


score_vs_plot_h <- score_vs_plot %>%
  mutate(sk = 1-value/base) %>%
  mutate(h = paste0("h = ", h))

plot_vs_data_h <- score_vs_plot_h %>%
  mutate(group = factor(group, c("GDP", "Expenditure", "Income", "Output", "EA19", sort(names_geo)), ordered = TRUE)) %>%
  filter(reco %in% c("wls", "shr", "ols")) 

plot_vs_data_h$type[plot_vs_data_h$group == "GDP"] <- ""
plot_vs_data_h$type[plot_vs_data_h$group %in% c("Expenditure", "Income", "Output")] <- "Sides"
plot_vs_data_h$type[!(plot_vs_data_h$group %in% c("GDP", "Expenditure", "Income", "Output"))] <- "Countries"

plot_vs_data_h <- plot_vs_data_h %>%
  mutate(type = factor(type, c("", "Sides", "Countries"), ordered = TRUE))
plot_vs_h <- plot_vs_data_h %>%
  filter(sk >= (-0.30)) %>%
  mutate(reco = factor(reco, c("ols", "wls", "shr"), ordered = TRUE)) %>%
  ggplot(aes(x = group, y = sk, col = reco, shape = reco)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0.05, 0.05),
                     breaks = seq(-0.6, 0.7, by = 0.1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  geom_point(aes(y = -0.30), shape=17, size = 3, show.legend = FALSE, data = subset(plot_vs_data_h, sk < (-0.30))) +
  facet_grid(h~type, scales = "free", space = "free_x")+
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + labs(y="VS (%)", x=NULL)+
  theme_minimal()+
  coord_cartesian(ylim = c(-0.30, NA))+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.margin = margin(b = -10),
        text = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.ticks = element_line(),
        axis.text.x = element_text(size = 9, angle = 90, hjust=1, vjust = 0.5),
        plot.margin = margin(b = 6, l = 6),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.text.x = element_text(margin = margin(b = 3, t = 0), size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"))

ggsave(plot_vs_h, filename = "EA_BOOT_VS_h.pdf",
       width = 210,
       height = 200,
       units = "mm")


score_vs_plot_h1_4 <- score_vs_plot %>%
  group_by(reco, group, type) %>%
  dplyr::summarise(sk = 1-mean(value)/mean(base)) %>%
  mutate(h = "h = 1:4")


score_vs_data_h1_4 <- score_vs_plot_h1_4 %>%
  mutate(group = factor(group, c("GDP", "Expenditure", "Income", "Output", "EA19", sort(names_geo)), ordered = TRUE)) %>%
  filter(reco %in% c("wls", "shr", "ols")) 
score_vs_data_h1_4$type[score_vs_data_h1_4$group == "GDP"] <- ""
score_vs_data_h1_4$type[score_vs_data_h1_4$group %in% c("Expenditure", "Income", "Output")] <- "Sides"
score_vs_data_h1_4$type[!(score_vs_data_h1_4$group %in% c("GDP", "Expenditure", "Income", "Output"))] <- "Countries"

score_vs_data_h1_4 <- score_vs_data_h1_4 %>%
  mutate(type = factor(type, c("", "Sides", "Countries"), ordered = TRUE))
plot_vs_h1_4 <- score_vs_data_h1_4 %>%
  filter(sk >= (-0.30)) %>%
  mutate(reco = factor(reco, c("ols", "wls", "shr"), ordered = TRUE)) %>%
  ggplot(aes(x = group, y = sk, col = reco, shape = reco)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0.05, 0.05),
                     breaks = seq(-0.6, 0.7, by = 0.1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  geom_point(aes(y = -0.30), shape=17, size = 3, show.legend = FALSE, data = subset(score_vs_data_h1_4, sk < (-0.30))) +
  facet_grid(h~type, scales = "free", space = "free_x")+
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + labs(y="VS (%)", x=NULL)+
  theme_minimal()+
  coord_cartesian(ylim = c(-0.30, NA))+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.margin = margin(b = -10),
        text = element_text(size = 9),
        axis.ticks = element_line(),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(size = 9, angle = 90, hjust=1, vjust = 0.5),
        plot.margin = margin(b = 6, l = 6),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.text.x = element_text(margin = margin(b = 3, t = 0), size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"))

ggsave(plot_vs_h1_4, filename = "EA_BOOT_VS_h14.pdf",
       width = 210,
       height = 90,
       units = "mm")

library(sf)
library(scales)
library(rnaturalearth)

score_crps <- ea_crps_data %>%
  group_by(geo, NAc, reco, h) %>%
  dplyr::summarise(crps = mean(value))
score_crps_base <- score_crps %>%
  ungroup() %>%
  filter(reco == "base") %>%
  dplyr::rename(base = crps) %>%
  select(-reco)

worldmap <- ne_countries(scale = 'medium', type = 'countries',
                         returnclass = 'sf')

names_geo <- c("AT" = "Austria", "BE" = "Belgium", "CY" = "Cyprus", "EE" = "Estonia", 
               "FI" = "Finland", "FR" = "France", "DE" = "Germany", "EL" = "Greece", 
               "IE" = "Ireland", "IT" = "Italy", "LV" = "Latvia", "LT" = "Lithuania", 
               "LU" = "Luxembourg", "MT" = "Malta", "NL" = "Netherlands", "PT" = "Portugal",
               "SI" = "Slovenia", "SK" = "Slovakia", "ES" = "Spain")
geo_tibble <- full_join(score_crps, score_crps_base) %>% 
  filter(NAc == "GDP") %>%
  filter(h == 1) %>% 
  mutate(value = 1-crps/base) %>%
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
  ggrepel::geom_label_repel(aes(label = scales::percent(wls, accuracy = 0.1), 
                                geometry = geometry),
                            seed = 1234,
                            stat = "sf_coordinates", min.segment.length = 0, size = 2.5) +
  ggrepel::geom_label_repel(aes(label = scales::percent(wls, accuracy = 0.1), 
                                geometry = geometry, fill = 100*wls),
                            seed = 1234, alpha = 0.3,
                            stat = "sf_coordinates", min.segment.length = 0, size = 2.5) +
  scale_color_gradient2(low = "#cf2a10", mid = "#b5ba61", high = "#7c8d4c",
                        midpoint = 3,name = "%", #na.value = NA, breaks = seq(-1,25,5),
                        guide = guide_colorbar(direction = "vertical", 
                                               title.position = "top",
                                               label.position = "right",
                                               barwidth = unit(0.4, "cm"),
                                               barheight = unit(4, "cm"),
                                               ticks = TRUE))+
  scale_fill_gradient2(low = "#cf2a10", mid = "#b5ba61", high = "#7c8d4c", 
                       midpoint = 3, name = "CRPS (%)", #na.value = NA, breaks = seq(-1,25,5),
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
ggsave(geo_plot, filename = "EA_BOOT_MAP_h1_wls.pdf",
       width = 210,
       height = 160,
       units = "mm")
