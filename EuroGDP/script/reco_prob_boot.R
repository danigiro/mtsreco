library(FoReco)
library(forecast)
library(progress)
library(furrr)

source("R/fupa.R")

load("./data/base.RData")
load("./data/agg_gdp.RData")
C <- Cgdp

plan(multisession, workers = 14)
H <- 4
B <- 5000

id <- c(rownames(C), colnames(C))
Index_seq <- matrix(0, B, H)
method  <- c("ols", "wls", "shr")
set.seed(1234)
path <- NULL
for(i in 1:dim(base_array)[3]){
  cat(paste0("Replication n. ", i, " base path","\n"))
  Index <- base::sample(c(1:(nrow(na.omit(res_array[,,1]))-(H-1))), size = B , replace = TRUE)
  for (k in 1:B) {
    Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+H-1), by = 1)
  }
  model_list <- setNames(model$model[model$rep==i], model$id[model$rep==i])
  model_list <- model_list[id]
  base <- fupa_map(B = B, H = 4, fit = model_list, idx = t(Index_seq))
  res <- na.omit(res_array[,id,i])
  
  pb <- progress_bar$new(format = paste0("Replication n. ", i, " reco path",
                                         " [:bar] :percent in :elapsed (ETA: :eta)"),
                         total = length(method), clear = FALSE, width= 60, show_after = 0)
  for(j in method){
    reco_fp <- future_map(asplit(base$path,3), function(x){
      FoReco::htsrec(basef = x, comb = j, C = C, keep = "recf", res = res)
    })
    reco_fp <- simplify2array(reco_fp)
    path[[j]] <- reco_fp
    pb$tick()
  }
  path$base <- base$path
  save(path, file = paste0("./data/path/boot/mod_rep", i, ".RData"))
}
