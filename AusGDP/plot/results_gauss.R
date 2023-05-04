library(tidyverse)
library(reshape2)
library(ggpubr)
load("./exp/Cmat.Rdata")
load("./inc/Cmat.Rdata")
load("./prob_scores_gaussian.RData")

# Expenditure
exp_crps_data <- tibble(melt(simplify2array(exp_crps))) %>% 
  separate(Var3, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(exp_crps_data) <- c("id", "h", "reco", "rep", "value")
exp_crps_data <- exp_crps_data %>%
  mutate(Levels = ifelse(id %in% rownames(Cexp), "Aggregate levels", "Bottom Level"))
exp_crps_data$rt <- "Expenditure"
exp_crps_data$side <- "Expenditure"
exp_crps_data$id <- recode(exp_crps_data$id, "Gdpe" = "Gdp")
exp_crps_data <- na.omit(exp_crps_data)

exp_es_data <- tibble(melt(simplify2array(exp_es)))%>% 
  separate(Var3, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(exp_es_data) <- c("type", "h", "reco", "rep", "value")
exp_es_data$rt <- "Expenditure"
exp_es_data$side <- "Expenditure"
exp_es_data <- na.omit(exp_es_data)

exp_vs_data <- tibble(melt(simplify2array(exp_vs)))%>% 
  separate(Var3, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(exp_vs_data) <- c("type", "h", "reco", "rep", "value")
exp_vs_data$rt <- "Expenditure"
exp_vs_data$side <- "Expenditure"
exp_vs_data <- na.omit(exp_vs_data)

# Income
inc_crps_data <- tibble(melt(simplify2array(inc_crps)))%>% 
  separate(Var3, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(inc_crps_data) <- c("id", "h", "reco", "rep", "value")
inc_crps_data <- inc_crps_data %>%
  mutate(Levels = ifelse(id %in% rownames(Cinc), "Aggregate levels", "Bottom Level"))
inc_crps_data$rt <- "Income"
inc_crps_data$side <- "Income"
inc_crps_data$id <- recode(inc_crps_data$id, "Gdpi" = "Gdp")
inc_crps_data <- na.omit(inc_crps_data)

inc_es_data <- tibble(melt(simplify2array(inc_es)))%>% 
  separate(Var3, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(inc_es_data) <- c("type", "h", "reco", "rep", "value")
inc_es_data$rt <- "Income"
inc_es_data$side <- "Income"
inc_es_data <- na.omit(inc_es_data)

inc_vs_data <- tibble(melt(simplify2array(inc_vs)))%>% 
  separate(Var3, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(inc_vs_data) <- c("type", "h", "reco", "rep", "value")
inc_vs_data$rt <- "Income"
inc_vs_data$side <- "Income"
inc_vs_data <- na.omit(inc_vs_data)

# Fully reconciled
aus_crps_data <- tibble(melt(simplify2array(aus_crps)))%>% 
  separate(Var3, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(aus_crps_data) <- c("id", "h", "reco", "rep", "value")
aus_crps_data <- aus_crps_data %>%
  mutate(Levels = ifelse(id %in% c(rownames(Cinc), rownames(Cexp), "Gdp"), "Aggregate levels", "Bottom Level"),
         side = ifelse(id %in% c(rownames(Cinc), colnames(Cinc)), "Income", "Expenditure"))
aus_crps_data$rt <- "Fully reconciled"
aus_crps_data <- na.omit(aus_crps_data)

aus_es_data <- tibble(melt(simplify2array(aus_es)))%>% 
  separate(Var4, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(aus_es_data) <- c("type", "h", "side", "reco", "rep", "value")
aus_es_data$rt <- "Fully reconciled"
aus_es_data$side <- recode(aus_es_data$side, 
                           "income" = "Income", 
                           "expenditure"="Expenditure", 
                           "all" = "Fully reconciled")
aus_es_data <- na.omit(aus_es_data)

aus_vs_data <- tibble(melt(simplify2array(aus_vs)))%>% 
  separate(Var4, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(aus_vs_data) <- c("type", "h", "side", "reco", "rep", "value")
aus_vs_data$rt <- "Fully reconciled"
aus_vs_data$side <- recode(aus_vs_data$side, 
                           "income" = "Income", 
                           "expenditure"="Expenditure", 
                           "all" = "Fully reconciled")
aus_vs_data <- na.omit(aus_vs_data)

score_es <- rbind(inc_es_data, exp_es_data, aus_es_data) %>%
  group_by(h, reco, side, rt, type) %>%
  summarise(value = mean(value)) 

score_es_base <- score_es %>%
  ungroup() %>%
  filter(reco == "base") %>%
  rename(base = value) %>%
  select(-reco)

plot_es <- full_join(score_es, score_es_base) %>%
  mutate(side = factor(recode(side, "Fully reconciled" = ""), c("Income", "Expenditure", ""), ordered = TRUE),
         rt = ifelse(rt != "Fully reconciled", "", "Fully reconciled")) %>%
  mutate(sk = 1-value/base) %>% 
  filter(reco %in% c("ols", "wls", "shr"), side != "", type == "All") %>%
  mutate(type = recode(type, "All" = "All variables"),
         reco = factor(reco, c("ols", "wls", "shr"), ordered = TRUE)) %>%
  ggplot(aes(x = as.factor(h), y = sk, shape = reco, col = reco)) +
  facet_grid(type~side+rt, scales = "free")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + labs(x="Forecast horizon", y="ES (%)")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks = element_line(),
        legend.text = element_text(size = 9),
        text = element_text(size = 10),
        axis.text = element_text(size = 9),
        legend.margin = margin(t = 0),
        panel.border = element_rect(fill = NA, colour = "grey"),
        strip.text.x = element_text(margin = margin(b = 3, t = 0), size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"))

score_vs <- rbind(inc_vs_data, exp_vs_data, aus_vs_data) %>%
  group_by(h, reco, side, rt, type) %>%
  summarise(value = mean(value)) 

score_vs_base <- score_vs %>%
  ungroup() %>%
  filter(reco == "base") %>%
  rename(base = value) %>%
  select(-reco)

plot_vs <- full_join(score_vs, score_vs_base) %>%
  mutate(side = factor(recode(side, "Fully reconciled" = ""), c("Income", "Expenditure", ""), ordered = TRUE),
         rt = ifelse(rt != "Fully reconciled", "", "Fully reconciled")) %>%
  mutate(sk = 1-value/base) %>% 
  filter(reco %in% c("ols", "wls", "shr"), side != "", type == "All") %>%
  mutate(type = recode(type, "All" = "All variables"),
         reco = factor(reco, c("ols", "wls", "shr"), ordered = TRUE)) %>%
  ggplot(aes(x = as.factor(h), y = sk, shape = reco, col = reco)) +
  facet_grid(type~side+rt, scales = "free")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + labs(x="Forecast horizon", y="VS (%)")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks = element_line(),
        legend.text = element_text(size = 9),
        text = element_text(size = 10),
        axis.text = element_text(size = 9),
        legend.margin = margin(t = 0),
        panel.border = element_rect(fill = NA, colour = "grey"),
        strip.text.x = element_text(margin = margin(b = 3, t = 0), size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"))

score_crps <- rbind(inc_crps_data, exp_crps_data, aus_crps_data) %>%
  group_by(id, h, reco, side, rt, Levels) %>%
  summarise(crps = mean(value))
score_crps_base <- score_crps %>%
  ungroup() %>%
  filter(reco == "base") %>%
  rename(base = crps) %>%
  select(-reco)

plot_gdp <- full_join(score_crps, score_crps_base) %>% filter(id == "Gdp", reco %in% c("ols", "wls", "shr"))%>%
  mutate(sk = 1-crps/base) %>%
  mutate(rt = factor(rt, c("Income", "Expenditure", "Fully reconciled"), ordered = TRUE),
         id = "GDP",
         reco = factor(reco, c("ols", "wls", "shr"), ordered = TRUE)) %>%
  ggplot(aes(x = as.factor(h), y = sk, shape = reco, col = reco)) +
  facet_grid(id~rt, scales = "free")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + labs(x="Forecast horizon", y="CRPS (%)")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks = element_line(),
        legend.text = element_text(size = 9),
        text = element_text(size = 10),
        axis.text = element_text(size = 9),
        legend.margin = margin(t = 0),
        panel.border = element_rect(fill = NA, colour = "grey"),
        strip.text.x = element_text(margin = margin(b = 3, t = 0), size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"))

figure_prob <- ggarrange(plot_gdp, plot_es, nrow = 2, heights = c(0.9,1), common.legend = TRUE)
figure_probvs <- ggarrange(plot_gdp, plot_vs, nrow = 2, heights = c(0.9,1), common.legend = TRUE)

ggsave(plot = figure_prob, file = paste0("AUS_Gauss_CRPS_ES.pdf"),
       width = 210, height = 140, units = "mm")
ggsave(plot = figure_probvs, file = paste0("AUS_Gauss_CRPS_VS.pdf"),
       width = 210, height = 140, units = "mm")
ggsave(plot = plot_vs, file = paste0("AUS_Gauss_VS.pdf"),
       width = 210, height = 90, units = "mm")
