library(tidyverse)
library(reshape2)
library(FoReco)

Cprod <- matrix(c(1,1,1), nrow = 1, byrow = TRUE)
colnames(Cprod) <- c("D21X31", "B1G", "YA1")
rownames(Cprod) <- c("GDP")


Cinc <- matrix(c(1,1,1,1,-1,1,
                 1,1,0,0,0,0,
                 0,0,0,1,-1,0), nrow = 3, byrow = TRUE)
colnames(Cinc) <- c("D11", "D12", "B2A3G", "D2", "D3","YA2")
rownames(Cinc) <- c("GDP", "D1", "D2X3")

Cexp <- matrix(c(1,1,1,1,1,1,1,1,1,-1,-1,1,
                 1,1,1,1,0,0,0,0,0, 0, 0,0,
                 1,1,0,0,0,0,0,0,0, 0, 0,0,
                 0,0,1,1,0,0,0,0,0, 0, 0,0,
                 0,0,0,0,1,1,1,0,0, 0, 0,0,
                 0,0,0,0,0,1,1,0,0, 0, 0,0,
                 0,0,0,0,0,0,0,1,1, 0, 0,0,
                 0,0,0,0,0,0,0,0,0, 1, 1,0,
                 0,0,0,0,0,0,0,1,1,-1,-1,0,
                 0,0,0,0,0,0,0,1,0,-1, 0,0,
                 0,0,0,0,0,0,0,0,1, 0,-1,0,
                 1,1,1,1,1,1,1,0,0, 0, 0,0,
                 1,1,1,1,1,1,1,1,1, 0, 0,0), nrow = 13, byrow = TRUE)
colnames(Cexp) <- c("P31_S14", "P31_S15", "P31_S13", "P32_S13",
                    "P51G", "P52", "P53", "P61", "P62", "P71", "P72", "YA0")
rownames(Cexp) <- c("GDP", "P3", "P31_S14_S15", "P3_S13", "P5G", "P52_P53",
                    "P6", "P7", "B11", "B111", "B112", "P3_P5", "P3_P6")
library(Matrix)

ord_name_col_c <- c(colnames(Cexp), colnames(Cinc), colnames(Cprod))
ord_name_row <- c(rownames(Cexp), rownames(Cinc), rownames(Cprod))
ord_name_col_u <- c(unique(ord_name_row), colnames(Cexp), colnames(Cinc), colnames(Cprod))
Ut <- cbind(matrix(0, nrow = length(ord_name_row), ncol = length(unique(ord_name_row))), -bdiag(list(Cexp, Cinc, Cprod)))
colnames(Ut) <- ord_name_col_u
rownames(Ut) <- ord_name_row
for(i in unique(ord_name_row)){
  Ut[ord_name_row == i, ord_name_col_u == i] <- 1
}


geoUt <- function(x){
  if(x %in% c("BE", "CY", "FR", "DE", "EL", "IT", "LV", "LU", "LT","MT", "NL", "SI", "SK", "ES")){
    out <- Ut[, !(colnames(Ut) %in% c("YA0", "YA1", "YA2"))]
  }else if(x %in% c("PT")){
    out <- Ut[, !(colnames(Ut) %in% c("YA0", "YA2"))]
  }else if(x %in% c("FI", "EE", "AT")){
    out <- Ut[, !(colnames(Ut) %in% c("YA1", "YA2"))]
  }else{
    out <- Ut
  }
  
  colnames(out) <- paste(x, colnames(out))
  rownames(out) <- paste(x, rownames(out))
  
  out
}
geof <- c("EA19", "AT", "BE", "CY", "EE", "FI", "FR", "DE", "EL", 
          "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PT",
          "SI", "SK", "ES")
list_ut <- lapply(geof, geoUt)
Ut_big <- bdiag(list_ut)
colnames(Ut_big) <- unlist(lapply(list_ut, colnames))
rownames(Ut_big) <- unlist(lapply(list_ut, rownames))


colut <- t(unname(sapply(colnames(Ut_big), function(x) str_split(x, " ")[[1]])))
rowut <- t(unname(sapply(colnames(Ut_big)[!(colnames(Ut_big) %in% rownames(Ut_big))], function(x) str_split(x, " ")[[1]])))
rowut_ea19 <- rowut[rowut[,1]=="EA19", ]
Ut2 <- matrix(0, nrow = NROW(rowut_ea19), ncol = length(colnames(Ut_big)))
for(i in unique(colut[,2])){
  Ut2[rowut_ea19[,2]==i, colut[,2] == i] <- -1
  Ut2[rowut_ea19[,2]==i, colut[,1] == "EA19" & colut[,2] == i] <- 1
}
colnames(Ut2) <- paste(colut[,1], colut[,2])
rownames(Ut2) <- paste(rowut_ea19[,1], rowut_ea19[,2])

dataUt <- rbind(as_tibble(Ut2, rownames = "Var1") %>%
  add_column(id = 1:NROW(Ut2), .before = 1) %>%
  pivot_longer(-c(id, Var1), names_to = "Var2")%>%
  filter(value != 0),
  as_tibble(as.matrix(Ut_big), rownames = "Var1") %>%
  add_column(id = 1:NROW(Ut_big), .before = 1) %>%
  pivot_longer(-c(id, Var1), names_to = "Var2")%>%
  filter(value != 0))%>%
  separate(Var1, sep = " ", into = c("geo_r", "na_r"))%>%
  separate(Var2, sep = " ", into = c("geo_c", "na_c")) %>%
  mutate(geo_r = factor(geo_r, geof, ordered = TRUE),
         geo_c = factor(geo_c, geof, ordered = TRUE),
         na_r = factor(na_r, ord_name_col_u, ordered = TRUE),
         na_c = factor(na_c, ord_name_col_u, ordered = TRUE)) %>%
  arrange(geo_c, na_c, geo_r, na_r) %>%
  pivot_wider(names_from = c("geo_c", "na_c"), names_sep = " ", values_fill = 0)

Ut_gdp <- dataUt %>%
  select(-c("id", "geo_r", "na_r")) %>%
  as.matrix()
rownames(Ut_gdp) <- paste(dataUt$geo_r, dataUt$na_r)

Cgdp <- lcmat(Ut_gdp)$Cbar
Cgdp <- Cgdp[rowSums(abs(Cgdp))!=1,]
Ut_gdp <- cbind(Diagonal(NROW(Cgdp)), -Cgdp)
save(Cgdp, Ut_gdp, file = "./data/agg_gdp.RData")

save(Cinc, Cexp, Cprod, Ut_gdp, file = "./data/agg_gdp_sep.RData")

names <- list(Exp = list(uts = rownames(Cexp), bts = colnames(Cexp)),
              Inc = list(uts = rownames(Cinc), bts = colnames(Cinc)),
              Pro = list(uts = rownames(Cprod), bts = colnames(Cprod)),
              Geo = geof)
save(names, file = "./data/group_names.RData")
