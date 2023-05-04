library(FoReco)
library(forecast)
library(tibble)
library(progress)

load("./data/base.RData")
load("./data/agg_gdp.RData")
C <- Cgdp

id <- c(rownames(C), colnames(C))
base_array <- base_array[,id,]
res_array <- res_array[,id,]
method  <- c("ols", "bu", "struc", "wls", "shr")
reco_array <- array(NA, dim = c(dim(base_array), length(method)),
                    dimnames = c(dimnames(base_array), list(method)))

pb <- progress_bar$new(format = paste0("Reco",
                                       " [:bar] :percent in :elapsed (ETA: :eta)"),
                       total = dim(base_array)[3], clear = FALSE, width= 60, show_after = 0)
for(i in 1:dim(base_array)[3]){
  base <- na.omit(base_array[,id,i])
  res <- na.omit(res_array[,id,i])
  
  for(j in method){
    tmp <- htsrec(basef = base, comb = j, C = C, 
                  keep = "recf", res = res)
    reco_array[1:NROW(base),,i,j] <- tmp
  }
  pb$tick()
}
save(reco_array, file = "./data/reco_point.RData")
