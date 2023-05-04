library(tidyverse)
library(reshape2)
library(ggpubr)
load("./exp/Cmat.Rdata")
load("./inc/Cmat.Rdata")

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

# Expenditure
load("./exp/base.Rdata",  temp_env <- new.env())
exp_list <- as.list(temp_env)

load("./exp/reco_point.Rdata",  temp_env <- new.env())
exp_reco_array <- as.list(temp_env)$reco_array

exp_results <- reco2tibble(exp_list, exp_reco_array) %>%
  mutate(Levels = ifelse(id %in% rownames(Cexp), "Aggregate levels", "Bottom Level"))
exp_results$rt <- "Expenditure"
exp_results$side <- "Expenditure"
exp_results$id <- recode(exp_results$id, "Gdpe" = "Gdp")
exp_results <- na.omit(exp_results)

# Income
load("./inc/base.Rdata",  temp_env <- new.env())
inc_list <- as.list(temp_env)

load("./inc/reco_point.Rdata",  temp_env <- new.env())
inc_reco_array <- as.list(temp_env)$reco_array

inc_results <- reco2tibble(inc_list, inc_reco_array)%>%
  mutate(Levels = ifelse(id %in% rownames(Cinc), "Aggregate levels", "Bottom Level"))
inc_results$rt <- "Income"
inc_results$side <- "Income"
inc_results$id <- recode(inc_results$id, "Gdpi" = "Gdp")
inc_results <- na.omit(inc_results)

# One-number-forecasts
load("./aus/base.Rdata",  temp_env <- new.env())
aus_list <- as.list(temp_env)

load("./aus/reco_point.Rdata",  temp_env <- new.env())
aus_reco_array <- as.list(temp_env)$reco_array

aus_results <- reco2tibble(aus_list, aus_reco_array)%>%
  mutate(Levels = ifelse(id %in% c(rownames(Cinc), rownames(Cexp)), "Aggregate levels", "Bottom Level"),
         side = ifelse(id %in% c(rownames(Cinc), colnames(Cinc)), "Income", "Expenditure"))
aus_results$rt <- "One-number-forecasts"
aus_results <- na.omit(aus_results)


score <- rbind(inc_results, exp_results, aus_results) %>%
  group_by(id, h, reco, side, rt, Levels) %>%
  summarise(MSE = mean((test-value)^2),
            MSE_base = mean((test-base)^2),
            MASE = mean(abs(test-value)/scale),
            MASE_base = mean(abs(test-base)/scale))

score_all <- score %>% #filter(id != "Gdp") %>%
  group_by(h, reco, side, rt) %>%
  summarise(MSE = mean(MSE),
            MSE_base = mean(MSE_base),
            MASE = mean(MASE),
            MASE_base = mean(MASE_base),
            Sk_MSE = 1-MSE/MSE_base,
            Sk_MASE = 1-MASE/MASE_base)
#score_all$Levels <- "All Levels (no GDP)"
score_all$Levels <- "All variables"
score_lev <- score %>%filter(id != "Gdp") %>%
  group_by(h, reco, side, rt, Levels) %>%
  summarise(MSE = mean(MSE),
            MSE_base = mean(MSE_base),
            MASE = mean(MASE),
            MASE_base = mean(MASE_base),
            Sk_MSE = 1-MSE/MSE_base,
            Sk_MASE = 1-MASE/MASE_base)

MSE_group <- rbind(score_all, score_lev) %>% filter(reco %in% c("ols", "wls", "shr")) %>%
  mutate(#Levels = factor(Levels, c("All Levels (no GDP)", "Aggregate levels", "Bottom Level"), ordered = TRUE),
         Levels = factor(Levels, c("All variables", "Aggregate levels", "Bottom Level"), ordered = TRUE),
         rt = ifelse(rt != "One-number-forecasts", "", "Fully reconciled")) %>%
  filter(Levels == "All variables") %>%
  ggplot(aes(x = as.factor(h), y = Sk_MSE, shape = reco, col = reco)) +
  facet_grid(Levels~side+rt, scales = "free")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + 
  labs(x="Forecast horizon", y=NULL)+
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

MSE_gdp <- score %>% filter(id == "Gdp", reco %in% c("ols", "wls", "shr"))%>%
  mutate(Sk_MSE = 1-MSE/MSE_base,
            Sk_MASE = 1-MASE/MASE_base) %>%
  ggplot(aes(x = as.factor(h), y = Sk_MSE, shape = reco, col = reco)) +
  facet_grid(id~rt, scales = "free")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + 
  labs(x="Forecast horizon", y=NULL)+
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

figure_MSE <- ggarrange(MSE_gdp, MSE_group, nrow = 2, heights = c(0.9,1), common.legend = TRUE)
figure_MSE <- annotate_figure(figure_MSE,
                #top = text_grob("Visualizing Tooth Growth", color = "red", face = "bold", size = 14),
                #bottom = text_grob("Forecast horizon", color = "black", size = 10),
                left = text_grob("MSE (%)", color = "black", rot = 90, size = 10),
                #right = text_grob(bquote("Superscript: ("*kg~NH[3]~ha^-1~yr^-1*")"), rot = 90),
                #fig.lab = "Figure 1", fig.lab.face = "bold"
)
ggsave(plot = figure_MSE, file = "AUS_MSE.pdf",
       width = 210, height = 140, units = "mm")


MASE_group <- rbind(score_all, score_lev) %>% filter(reco %in% c("ols", "wls", "shr")) %>%
  mutate(#Levels = factor(Levels, c("All Levels (no GDP)", "Aggregate levels", "Bottom Level"), ordered = TRUE),
    Levels = factor(Levels, c("All variables", "Aggregate levels", "Bottom Level"), ordered = TRUE),
    rt = ifelse(rt != "One-number-forecasts", "", "Fully reconciled")) %>%
  filter(Levels == "All variables") %>%
  ggplot(aes(x = as.factor(h), y = Sk_MASE, shape = reco, col = reco)) +
  facet_grid(Levels~side+rt, scales = "free")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) + 
  labs(x="Forecast horizon", y=NULL)+
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

MASE_gdp <- score %>% filter(id == "Gdp", reco %in% c("ols", "wls", "shr"))%>%
  mutate(Sk_MASE = 1-MASE/MASE_base,
         Sk_MASE = 1-MASE/MASE_base) %>%
  ggplot(aes(x = as.factor(h), y = Sk_MASE, shape = reco, col = reco)) +
  facet_grid(id~rt, scales = "free")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 19, size = 0.1)+
  geom_point(size = 3) +
  scale_color_manual(values = 3:1) +
  scale_shape_manual(values = 2:0) +  
  labs(x="Forecast horizon", y=NULL)+
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

figure_MASE <- ggarrange(MASE_gdp, MASE_group, nrow = 2, heights = c(0.9,1), common.legend = TRUE)
figure_MASE <- annotate_figure(figure_MASE,
                              #top = text_grob("Visualizing Tooth Growth", color = "red", face = "bold", size = 14),
                              #bottom = text_grob("Forecast horizon", color = "black", size = 10),
                              left = text_grob("MASE (%)", color = "black", rot = 90, size = 10),
                              #right = text_grob(bquote("Superscript: ("*kg~NH[3]~ha^-1~yr^-1*")"), rot = 90),
                              #fig.lab = "Figure 1", fig.lab.face = "bold"
)
ggsave(plot = figure_MASE, file = "AUS_MASE.pdf",
       width = 210, height = 140, units = "mm")

