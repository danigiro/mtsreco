library(tidyverse)
library(reshape2)
library(ggpubr)
load("./data/agg_gdp.Rdata")
load("./data/prob_scores_gaussian.RData")
load("./data/group_names.RData")
# Fully reconciled
ea_crps_data <- tibble(melt(simplify2array(ea_crps)))%>% 
  separate(Var3, into = c("reco", "trash")) %>%
  filter((reco == "base" & trash == "meth2") | (reco != "base" & trash == "meth3")) %>%
  select(-trash)
colnames(ea_crps_data) <- c("id", "h", "reco", "rep", "value")
ea_crps_data <- ea_crps_data %>%
  separate(id, c("geo", "NAc"), sep = " ")
ea_crps_data <- na.omit(ea_crps_data)

crps_h <- ea_crps_data %>%
  group_by(geo, NAc, reco, h) %>%
  dplyr::summarise(crps = mean(value)) |>
  summarise(value = mean(crps)) |>
  add_column(type = "crps")

nemenyi_fun <- function(data){
  nemenyi <- tsutils::nemenyi(data, plottype = "none")
  df_plot <- full_join(as_tibble(nemenyi$means, rownames = "name"), 
                       full_join(rename(as_tibble(nemenyi$means-nemenyi$cd/2, rownames = "name"), "l" = "value"),
                                 rename(as_tibble(nemenyi$means+nemenyi$cd/2, rownames = "name"), "u" = "value"), 
                                 by = "name"), by = "name") |>
    arrange(value) |>
    mutate(#name = gsub(" ", "", name),
      name = paste0(name, " - ", format(round(value, 2), width = 5, nsmall = 2))) |>
    add_column(fpval = nemenyi$fpval,
               fH = nemenyi$fH)
  df_plot$col <- df_plot$l <= df_plot$u[1]
  
  as_tibble(df_plot)
}

Exp <- setNames(rep("Expenditure", length(unlist(names$Exp)[-1])), unlist(names$Exp)[-1])
Inc <- setNames(rep("Income", length(unlist(names$Inc)[-1])), unlist(names$Inc)[-1])
Pro <- setNames(rep("Output", length(unlist(names$Pro)[-1])), unlist(names$Pro)[-1])
nnn <- c(Exp, Inc, Pro)

data_crps <- crps_h |>
  filter(reco %in% c("base", "shr", "wls", "ols")) |>
  mutate(type2 = recode(NAc, !!!nnn))

plot <- rbind(data_crps |>
          filter(type2 %in% c("GDP", "Expenditure")) |>
          mutate(type2 = "Expenditure"),
      #data_crps |>
      #    filter(type2 %in% c("GDP", "Income")) |>
      #    mutate(type2 = "Income"),
      data_crps |>
          filter(type2 %in% c("GDP", "Income", "Output")) |>
          mutate(type2 = "Income & Output"))%>%
  group_by(type, type2) |>
  nest() |>
  mutate(data = lapply(data, pivot_wider, names_from = reco, values_from = value),
         data = lapply(data, select, ... = -c("geo", "NAc")),
         data = lapply(data, nemenyi_fun)) |>
  unnest(cols = c(data)) |>
  ungroup() |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE),
         pch_name = str_detect(name, "base")) |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE)) |>
  arrange(type) |>
  #mutate(facet = paste0(type2, " - ", str_to_upper(type))) |>
  mutate(facet = type2) |>
  ggplot() + 
  geom_rect(aes(xmin=l, xmax=u, fill = col), ymin=-Inf, ymax=Inf, alpha = 0.2, 
            data = function(x) summarise(group_by(x, facet), l = min(l), col = TRUE,
                                         u = min(u), .groups = "drop"))+
  geom_segment(aes(x = l, xend = u, yend = name, y = name)) + 
  geom_point(aes(x = l, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = u, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = value, fill = col, y = name, pch = pch_name), size = 3) +
  geom_label(data = function(x) select(x, facet, fpval) |>
               mutate(text = paste0("Friedman test p-value ", ifelse(fpval<0.001, " < 0.001", round(fpval, 3)))),
             aes(x = Inf, y = -Inf, label = text), vjust = "inward", hjust = "inward", size = 2.5,  label.size = NA) + 
  scale_shape_manual(values=c(21, 24))+
  facet_wrap(.~facet, ncol = 3, scales = "free")+
  labs(y = NULL, x = NULL) + 
  theme_minimal()+
  scale_y_discrete(labels = scales::label_parse())+
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 10),
        strip.text = element_text(size = 9),
        legend.margin = margin())

ggsave("./EA_gauss_mcb.pdf", plot,
       width = 7,
       height = 1.5)


load("./data/agg_gdp.Rdata")
load("./data/prob_scores_boot.RData")
load("./data/group_names.RData")
# Fully reconciled
ea_crps_data <- tibble(melt(simplify2array(ea_crps)))
colnames(ea_crps_data) <- c("id", "h", "reco", "rep", "value")
ea_crps_data <- ea_crps_data %>%
  separate(id, c("geo", "NAc"), sep = " ")
ea_crps_data <- na.omit(ea_crps_data)


crps_h <- ea_crps_data %>%
  group_by(geo, NAc, reco, h) %>%
  dplyr::summarise(crps = mean(value)) |>
  summarise(value = mean(crps)) |>
  add_column(type = "crps")

data_crps <- crps_h |>
  filter(reco %in% c("base", "shr", "wls", "ols")) |>
  mutate(type2 = recode(NAc, !!!nnn))

plot <- rbind(data_crps |>
                filter(type2 %in% c("GDP", "Expenditure")) |>
                mutate(type2 = "Expenditure"),
              #data_crps |>
              #    filter(type2 %in% c("GDP", "Income")) |>
              #    mutate(type2 = "Income"),
              data_crps |>
                filter(type2 %in% c("GDP", "Income", "Output")) |>
                mutate(type2 = "Income & Output"))%>%
  group_by(type, type2) |>
  nest() |>
  mutate(data = lapply(data, pivot_wider, names_from = reco, values_from = value),
         data = lapply(data, select, ... = -c("geo", "NAc")),
         data = lapply(data, nemenyi_fun)) |>
  unnest(cols = c(data)) |>
  ungroup() |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE),
         pch_name = str_detect(name, "base")) |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE)) |>
  arrange(type) |>
  #mutate(facet = paste0(type2, " - ", str_to_upper(type))) |>
  mutate(facet = type2) |>
  ggplot() + 
  geom_rect(aes(xmin=l, xmax=u, fill = col), ymin=-Inf, ymax=Inf, alpha = 0.2, 
            data = function(x) summarise(group_by(x, facet), l = min(l), col = TRUE,
                                         u = min(u), .groups = "drop"))+
  geom_segment(aes(x = l, xend = u, yend = name, y = name)) + 
  geom_point(aes(x = l, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = u, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = value, fill = col, y = name, pch = pch_name), size = 3) +
  geom_label(data = function(x) select(x, facet, fpval) |>
               mutate(text = paste0("Friedman test p-value ", ifelse(fpval<0.001, " < 0.001", round(fpval, 3)))),
             aes(x = Inf, y = -Inf, label = text), vjust = "inward", hjust = "inward", size = 2.5,  label.size = NA) + 
  scale_shape_manual(values=c(21, 24))+
  facet_wrap(.~facet, ncol = 3, scales = "free")+
  labs(y = NULL, x = NULL) + 
  theme_minimal()+
  scale_y_discrete(labels = scales::label_parse())+
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 10),
        strip.text = element_text(size = 9),
        legend.margin = margin())

ggsave("./EA_boot_mcb.pdf", plot,
       width = 7,
       height = 1.5)

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

base <- score |>
  ungroup() |>
  select(geo, NAc, h, MSE_base, MASE_base) |>
  mutate(reco = "base") |>
  rename(MSE = MSE_base, MASE = MASE_base)
score_h <- score |>
  ungroup() |>
  select(geo, NAc, reco, h, MSE, MASE) 

pro <- rbind(score_h, base) |>
  pivot_longer(-c("geo", "NAc", "reco", "h"), names_to = "type") |>
  group_by(geo, NAc, reco, type)|>
  summarise(value = mean(value)) |>
  filter(reco %in% c("base", "shr", "wls", "ols")) |>
  mutate(type2 = recode(NAc, !!!nnn))

plot <- rbind(pro |>
                filter(type2 %in% c("GDP", "Expenditure")) |>
                mutate(type2 = "Expenditure"),
              #data_crps |>
              #    filter(type2 %in% c("GDP", "Income")) |>
              #    mutate(type2 = "Income"),
              pro |>
                filter(type2 %in% c("GDP", "Income", "Output")) |>
                mutate(type2 = "Income & Output"))%>%
  filter(type == "MSE") |>
  group_by(type, type2) |>
  nest() |>
  mutate(data = lapply(data, pivot_wider, names_from = reco, values_from = value),
         data = lapply(data, select, ... = -c("geo", "NAc")),
         data = lapply(data, nemenyi_fun)) |>
  unnest(cols = c(data)) |>
  ungroup() |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE),
         pch_name = str_detect(name, "base")) |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE)) |>
  arrange(type) |>
  #mutate(facet = paste0(type2, " - ", str_to_upper(type))) |>
  mutate(facet = type2) |>
  ggplot() + 
  geom_rect(aes(xmin=l, xmax=u, fill = col), ymin=-Inf, ymax=Inf, alpha = 0.2, 
            data = function(x) summarise(group_by(x, facet), l = min(l), col = TRUE,
                                         u = min(u), .groups = "drop"))+
  geom_segment(aes(x = l, xend = u, yend = name, y = name)) + 
  geom_point(aes(x = l, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = u, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = value, fill = col, y = name, pch = pch_name), size = 3) +
  geom_label(data = function(x) select(x, facet, fpval) |>
               mutate(text = paste0("Friedman test p-value ", ifelse(fpval<0.001, " < 0.001", round(fpval, 3)))),
             aes(x = Inf, y = -Inf, label = text), vjust = "inward", hjust = "inward", size = 2.5,  label.size = NA) + 
  scale_shape_manual(values=c(21, 24))+
  facet_wrap(.~facet, ncol = 3, scales = "free")+
  labs(y = NULL, x = NULL) + 
  theme_minimal()+
  scale_y_discrete(labels = scales::label_parse())+
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 10),
        strip.text = element_text(size = 9),
        legend.margin = margin())

ggsave("./EA_point_mcb.pdf", plot,
       width = 7,
       height = 1.5)





















base <- score |>
  ungroup() |>
  select(geo, NAc, h, MSE_base, MASE_base) |>
  mutate(reco = "base") |>
  rename(MSE = MSE_base, MASE = MASE_base)
score_h <- score |>
  ungroup() |>
  select(geo, NAc, reco, h, MSE, MASE) 

pro <- rbind(score_h, base) |>
  pivot_longer(-c("geo", "NAc", "reco", "h"), names_to = "type") |>
  group_by(geo, NAc, reco, type)|>
  summarise(value = mean(value)) |>
  rbind(crps_h) |>
  filter(reco %in% c("base", "shr", "wls", "ols")) |>
  mutate(type2 = recode(NAc, !!!nnn))


plot <- rbind(pro |>
  filter(type2 %in% c("GDP", "Expenditure")) |>
  mutate(type2 = "Expenditure"),
  pro |>
    filter(type2 %in% c("GDP", "Income")) |>
    mutate(type2 = "Income"),
  pro |>
    filter(type2 %in% c("GDP", "Output")) |>
    mutate(type2 = "Output"))%>%
  group_by(type, type2) |>
  nest() |>
  mutate(data = lapply(data, pivot_wider, names_from = reco, values_from = value),
         data = lapply(data, select, ... = -c("geo", "NAc")),
         data = lapply(data, nemenyi_fun)) |>
  unnest(cols = c(data)) |>
  ungroup() |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE),
         pch_name = str_detect(name, "base")) |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE)) |>
  arrange(type) |>
  mutate(facet = paste0(type2, " - ", str_to_upper(type))) |>
  ggplot() + 
  geom_rect(aes(xmin=l, xmax=u, fill = col), ymin=-Inf, ymax=Inf, alpha = 0.2, 
            data = function(x) summarise(group_by(x, facet), l = min(l), col = TRUE,
                                         u = min(u), .groups = "drop"))+
  geom_segment(aes(x = l, xend = u, yend = name, y = name)) + 
  geom_point(aes(x = l, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = u, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = value, fill = col, y = name, pch = pch_name), size = 3) +
  geom_label(data = function(x) select(x, facet, fpval) |>
               mutate(text = paste0("Friedman test \np-value ", ifelse(fpval<0.001, " < 0.001", round(fpval, 3)))),
             aes(x = Inf, y = -Inf, label = text), vjust = "inward", hjust = "inward", size = 2.5,  label.size = NA) + 
  scale_shape_manual(values=c(21, 24))+
  facet_wrap(.~facet, ncol = 3, scales = "free")+
  labs(y = NULL, x = NULL) + 
  theme_minimal()+
  scale_y_discrete(labels = scales::label_parse())+
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 10),
        strip.text = element_text(size = 9),
        legend.margin = margin())


plot <- rbind(score_h, base) |>
  pivot_longer(-c("geo", "NAc", "reco", "h"), names_to = "type") |>
  group_by(geo, NAc, reco, type)|>
  summarise(value = mean(value)) |>
  rbind(crps_h) |>
  filter(reco %in% c("base", "shr", "wls", "ols")) |>
  mutate(NAc = recode(NAc, !!!nnn)) %>%
  group_by(type) |>
  nest() |>
  mutate(data = lapply(data, pivot_wider, names_from = reco, values_from = value),
         data = lapply(data, select, ... = -c("geo", "NAc")),
         data = lapply(data, nemenyi_fun)) |>
  unnest(cols = c(data)) |>
  ungroup() |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE),
         pch_name = str_detect(name, "base")) |>
  arrange(value) |>
  mutate(name = factor(name, unique(name), ordered = TRUE)) |>
  arrange(type) |>
  mutate(facet = str_to_upper(type)) |>
  ggplot() + 
  geom_rect(aes(xmin=l, xmax=u, fill = col), ymin=-Inf, ymax=Inf, alpha = 0.2, 
            data = function(x) summarise(group_by(x, facet), l = min(l), col = TRUE,
                                         u = min(u), .groups = "drop"))+
  geom_segment(aes(x = l, xend = u, yend = name, y = name)) + 
  geom_point(aes(x = l, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = u, y = name), pch = "|", size = 2) + 
  geom_point(aes(x = value, fill = col, y = name, pch = pch_name), size = 3) +
  geom_label(data = function(x) select(x, facet, fpval) |>
               mutate(text = paste0("Friedman test \np-value ", ifelse(fpval<0.001, " < 0.001", round(fpval, 3)))),
             aes(x = Inf, y = -Inf, label = text), vjust = "inward", hjust = "inward", size = 2.5,  label.size = NA) + 
  scale_shape_manual(values=c(21, 24))+
  facet_wrap(.~facet, ncol = 3, scales = "free", 
             labeller = label_parsed)+
  labs(y = NULL, x = NULL) + 
  theme_minimal()+
  scale_y_discrete(labels = scales::label_parse())+
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 10),
        strip.text = element_text(size = 9),
        legend.margin = margin())

ggsave("./mcb_all.pdf", plot,
       width = 7.5,
       height = 6)
