library(readr)
library(tibble)
library(tidyverse)
library(Matrix)
load("./data/agg_gdp.RData")

dataTSV <- read_delim("./data/namq_10_gdp.tsv", 
                          delim = "\t", escape_double = FALSE, 
                          na = ":", trim_ws = TRUE, show_col_types = FALSE)

info <- as_tibble(t(apply(dataTSV[,1], 1, function(x) strsplit(x, split = ",")[[1]])))
colnames(info) <- c("unit", "s_adj", "na_item", "geo")


value <- apply(dataTSV[,-1], 2, function(x){
  if(is.numeric(x)){
    x
  }else{
    as.numeric(unname(sapply(x, function(z){
      out <- strsplit(z, split = " ")[[1]][1]
      if(is.na(out)){
        NA
      }else if(out == ":"){
        NA
      }else{
        as.numeric(out)
      }
    })))
  }
})
value <- as_tibble(value)
data <- cbind(info, value)

id_series <- colnames(Cgdp)
data_id_series <- as_tibble(t(sapply(id_series, function(x) strsplit(x, " ")[[1]])), rownames = "id")
fts <- data %>%
  pivot_longer(-c("unit", "s_adj", "na_item", "geo")) %>%
  pivot_wider(names_from = "geo") %>%
  mutate(date = zoo::as.yearqtr(name)) %>%
  filter(date >= "2000 Q1" & date < "2020 Q1")%>%
  select(-name) %>%
  filter(s_adj == "NSA", unit == "CP_MEUR") %>% 
  select(-unit, -s_adj) %>%
  pivot_longer(-c(na_item, date), names_to = "geo") %>%
  filter(paste(geo, na_item) %in% colnames(Cgdp)) %>%
  mutate(na_item = factor(na_item, unique(data_id_series$V2), ordered = TRUE),
         geo = factor(geo, unique(data_id_series$V1), ordered = TRUE)) %>%
  arrange(geo, na_item, date) %>%
  pivot_wider(names_from = c(geo, na_item), names_sep = " ")

fts_mat <- fts %>%
  select(-date) %>%
  as.matrix()

all_data <- fts_mat[, colnames(Cgdp)] %*% t(Cgdp)

all_data <- cbind(all_data, fts_mat[, colnames(Cgdp)])
all_data <- as_tibble(as.matrix(all_data)) %>%
  add_column(date = fts$date, .before = 1) 
write.csv(all_data, file = "./data/data.csv")
save(all_data, file = "./data/data.RData")
saveRDS(all_data, file = "./data/data.rds")
