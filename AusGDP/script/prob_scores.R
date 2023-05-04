library(progress)
library(furrr)
library(scoringRules)
library(Matrix)

if(!exists("type")){
  type <- "boot"
}

source("./R/prob_score.R")
load("./inc/Cmat.RData")
load("./inc/base.RData",  temp_env <- new.env())
inc_test <- as.list(temp_env)$test_array

load("./exp/Cmat.RData")
load("./exp/base.RData",  temp_env <- new.env())
exp_test <- as.list(temp_env)$test_array

load("./aus/Cmat.RData")
load("./aus/base.RData",  temp_env <- new.env())
aus_test <- as.list(temp_env)$test_array

plan(multisession, workers = 12)
H <- 4
B <- 5000
inc_crps <- inc_es <- inc_vs <- NULL
exp_crps <- exp_es <- exp_vs <- NULL
aus_crps <- aus_es <- aus_vs <- NULL
pb <- progress_bar$new(format = paste0("[:bar] :percent in :elapsed (ETA: :eta)"),
                       total = 3*dim(aus_test)[3], clear = FALSE, width= 60, show_after = 0)
for(i in 1:dim(aus_test)[3]){
  # Income ----
  load(paste0("./inc/path/", type, "/mod_rep", i, ".RData"))
  test <- inc_test[!is.na(inc_test[,1,i]),,i, drop = FALSE]
  
  if(is.list(path[[1]])){
    path <- unlist(path,recursive=FALSE)
    #path_tmp$base <- path$base
    #path <- path_tmp
  }
  
  # CRPS
  inc_crps[[i]] <- simplify2array(future_map(path, function(xpath){
    tmp <- matrix(NA, nrow = dim(test)[2], ncol = H)
    tmp[,1:NROW(test)] <- sapply(1:NROW(test), function(x) crps_sample(test[x,,], dat = xpath[x,,],
                                                                       method = "edf"))
    rownames(tmp) <- c(rownames(Cinc), colnames(Cinc))
    tmp
  }))
  
  # ES
  inc_es[[i]] <- simplify2array(future_map(path, function(xpath){
    test_list <- split(test[,,], 1:NROW(test))
    tmp <- matrix(NA, 4, H)
    tmp[1,1:NROW(test)] <- unname(mapply(es_sample, dat = asplit(xpath, 1)[1:length(test_list)], y = test_list))
    
    tmp[2,1:NROW(test)] <- unname(mapply(es_sample, dat = asplit(xpath[,colnames(Cinc),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cinc),], 1:NROW(test))))
    tmp[3,1:NROW(test)] <- unname(mapply(es_sample, dat = asplit(xpath[,rownames(Cinc),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cinc),], 1:NROW(test))))
    tmp[4,1:NROW(test)] <- unname(mapply(es_sample, dat = asplit(xpath[,rownames(Cinc)[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cinc)[-1],], 1:NROW(test))))
    rownames(tmp) <- c("All", "Free variables", "Basic variables", "Basic variables (no GDP)")
    tmp
  },
  .options = furrr_options(seed = TRUE)))
  
  # VS
  inc_vs[[i]] <- simplify2array(future_map(path, function(xpath){
    test_list <- split(test[,,], 1:NROW(test))
    tmp <- matrix(NA, 4, H)
    tmp[1,1:NROW(test)] <- unname(mapply(vs_sample, dat = asplit(xpath, 1)[1:length(test_list)], y = test_list))
    
    tmp[2,1:NROW(test)] <- unname(mapply(vs_sample, dat = asplit(xpath[,colnames(Cinc),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cinc),], 1:NROW(test))))
    tmp[3,1:NROW(test)] <- unname(mapply(vs_sample, dat = asplit(xpath[,rownames(Cinc),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cinc),], 1:NROW(test))))
    tmp[4,1:NROW(test)] <- unname(mapply(vs_sample, dat = asplit(xpath[,rownames(Cinc)[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cinc)[-1],], 1:NROW(test))))
    rownames(tmp) <- c("All", "Free variables", "Basic variables", "Basic variables (no GDP)")
    tmp
  },
  .options = furrr_options(seed = TRUE)))
  pb$tick()
  # Expenditure ----
  load(paste0("./exp/path/", type, "/mod_rep", i, ".RData"))
  test <- exp_test[!is.na(exp_test[,1,i]),,i, drop = FALSE]
  
  if(is.list(path[[1]])){
    path <- unlist(path,recursive=FALSE)
    #path_tmp$base <- path$base
    #path <- path_tmp
  }
  
  # CRPS
  exp_crps[[i]] <- simplify2array(future_map(path, function(xpath){
    tmp <- matrix(NA, nrow = dim(test)[2], ncol = H)
    tmp[,1:NROW(test)] <- sapply(1:NROW(test), function(x) crps_sample(test[x,,], dat = xpath[x,,],
                                                                       method = "edf"))
    rownames(tmp) <- c(rownames(Cexp), colnames(Cexp))
    tmp
  },
  .options = furrr_options(seed = TRUE)))
  
  # ES
  exp_es[[i]] <- simplify2array(future_map(path, function(xpath){
    test_list <- split(test[,,], 1:NROW(test))
    tmp <- matrix(NA, 4, H)
    tmp[1,1:NROW(test)] <- unname(mapply(es_sample, dat = asplit(xpath, 1)[1:length(test_list)], y = test_list))
    
    tmp[2,1:NROW(test)] <- unname(mapply(es_sample, dat = asplit(xpath[,colnames(Cexp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cexp),], 1:NROW(test))))
    tmp[3,1:NROW(test)] <- unname(mapply(es_sample, dat = asplit(xpath[,rownames(Cexp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cexp),], 1:NROW(test))))
    tmp[4,1:NROW(test)] <- unname(mapply(es_sample, dat = asplit(xpath[,rownames(Cexp)[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cexp)[-1],], 1:NROW(test))))
    rownames(tmp) <- c("All", "Free variables", "Basic variables", "Basic variables (no GDP)")
    tmp
  },
  .options = furrr_options(seed = TRUE)))
  
  # VS
  exp_vs[[i]] <- simplify2array(future_map(path, function(xpath){
    test_list <- split(test[,,], 1:NROW(test))
    tmp <- matrix(NA, 4, H)
    tmp[1,1:NROW(test)] <- unname(mapply(vs_sample, dat = asplit(xpath, 1)[1:length(test_list)], y = test_list))
    
    tmp[2,1:NROW(test)] <- unname(mapply(vs_sample, dat = asplit(xpath[,colnames(Cexp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cexp),], 1:NROW(test))))
    tmp[3,1:NROW(test)] <- unname(mapply(vs_sample, dat = asplit(xpath[,rownames(Cexp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cexp),], 1:NROW(test))))
    tmp[4,1:NROW(test)] <- unname(mapply(vs_sample, dat = asplit(xpath[,rownames(Cexp)[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cexp)[-1],], 1:NROW(test))))
    rownames(tmp) <- c("All", "Free variables", "Basic variables", "Basic variables (no GDP)")
    tmp
  },
  .options = furrr_options(seed = TRUE)))
  
  pb$tick()
  
  # Expenditure ----
  load(paste0("./aus/path/", type, "/mod_rep", i, ".RData"))
  test <- aus_test[!is.na(aus_test[,1,i]),,i, drop = FALSE]
  rCinc <- rownames(Cinc)
  rCexp <- rownames(Cexp)
  rCinc[1] <- rCexp[1] <- "Gdp"
  
  if(is.list(path[[1]])){
    path <- unlist(path,recursive=FALSE)
    #path_tmp$base <- path$base
    #path <- path_tmp
  }
  
  # CRPS
  aus_crps[[i]] <- simplify2array(future_map(path, function(xpath){
    tmp <- matrix(NA, nrow = dim(test)[2], ncol = H)
    tmp[,1:NROW(test)] <- sapply(1:NROW(test), function(x) crps_sample(test[x,,], dat = xpath[x,,],
                                                                       method = "edf"))
    rownames(tmp) <- c(rownames(Cgdp), colnames(Cgdp))
    tmp
  }))
  
  # ES
  aus_es[[i]] <- simplify2array(future_map(path, function(xpath){
    test_list <- split(test[,,], 1:NROW(test))
    tmp <- array(NA, c(4, H, 3), dimnames = list( c("All", "Free variables", "Basic variables", "Basic variables (no GDP)"), NULL, c("all", "income", "expenditure")))
    tmp[1,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath, 1)[1:length(test_list)], y = test_list))
    tmp[2,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath[,colnames(Cgdp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cgdp),], 1:NROW(test))))
    tmp[3,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath[,rownames(Cgdp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cgdp),], 1:NROW(test))))
    tmp[4,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath[,rownames(Cgdp)[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cgdp)[-1],], 1:NROW(test))))
    
    tmp[1,1:NROW(test),"income"] <- unname(mapply(es_sample, dat = asplit(xpath[,c(rCinc, colnames(Cinc)),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,c(rCinc, colnames(Cinc)),], 1:NROW(test))))
    tmp[2,1:NROW(test),"income"] <- unname(mapply(es_sample, dat = asplit(xpath[,colnames(Cinc),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cinc),], 1:NROW(test))))
    tmp[3,1:NROW(test),"income"] <- unname(mapply(es_sample, dat = asplit(xpath[,rCinc,,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rCinc,], 1:NROW(test))))
    tmp[4,1:NROW(test),"income"] <- unname(mapply(es_sample, dat = asplit(xpath[,rCinc[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rCinc[-1],], 1:NROW(test))))
    
    tmp[1,1:NROW(test),"expenditure"] <- unname(mapply(es_sample, dat = asplit(xpath[,c(rCexp, colnames(Cexp)),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,c(rCexp, colnames(Cexp)),], 1:NROW(test))))
    tmp[2,1:NROW(test),"expenditure"] <- unname(mapply(es_sample, dat = asplit(xpath[,colnames(Cexp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cexp),], 1:NROW(test))))
    tmp[3,1:NROW(test),"expenditure"] <- unname(mapply(es_sample, dat = asplit(xpath[,rCexp,,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rCexp,], 1:NROW(test))))
    tmp[4,1:NROW(test),"expenditure"] <- unname(mapply(es_sample, dat = asplit(xpath[,rCexp[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rCexp[-1],], 1:NROW(test))))
    tmp
  },
  .options = furrr_options(seed = TRUE)))
  
  # VS
  aus_vs[[i]] <- simplify2array(future_map(path, function(xpath){
    test_list <- split(test[,,], 1:NROW(test))
    tmp <- array(NA, c(4, H, 3), dimnames = list( c("All", "Free variables", "Basic variables", "Basic variables (no GDP)"), NULL, c("all", "income", "expenditure")))
    tmp[1,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath, 1)[1:length(test_list)], y = test_list))
    tmp[2,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath[,colnames(Cgdp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cgdp),], 1:NROW(test))))
    tmp[3,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath[,rownames(Cgdp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cgdp),], 1:NROW(test))))
    tmp[4,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath[,rownames(Cgdp)[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(Cgdp)[-1],], 1:NROW(test))))
    
    tmp[1,1:NROW(test),"income"] <- unname(mapply(vs_sample, dat = asplit(xpath[,c(rCinc, colnames(Cinc)),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,c(rCinc, colnames(Cinc)),], 1:NROW(test))))
    tmp[2,1:NROW(test),"income"] <- unname(mapply(vs_sample, dat = asplit(xpath[,colnames(Cinc),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cinc),], 1:NROW(test))))
    tmp[3,1:NROW(test),"income"] <- unname(mapply(vs_sample, dat = asplit(xpath[,rCinc,,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rCinc,], 1:NROW(test))))
    tmp[4,1:NROW(test),"income"] <- unname(mapply(vs_sample, dat = asplit(xpath[,rCinc[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rCinc[-1],], 1:NROW(test))))
    
    tmp[1,1:NROW(test),"expenditure"] <- unname(mapply(vs_sample, dat = asplit(xpath[,c(rCexp, colnames(Cexp)),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,c(rCexp, colnames(Cexp)),], 1:NROW(test))))
    tmp[2,1:NROW(test),"expenditure"] <- unname(mapply(vs_sample, dat = asplit(xpath[,colnames(Cexp),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(Cexp),], 1:NROW(test))))
    tmp[3,1:NROW(test),"expenditure"] <- unname(mapply(vs_sample, dat = asplit(xpath[,rCexp,,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rCexp,], 1:NROW(test))))
    tmp[4,1:NROW(test),"expenditure"] <- unname(mapply(vs_sample, dat = asplit(xpath[,rCexp[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rCexp[-1],], 1:NROW(test))))
    tmp
  },
  .options = furrr_options(seed = TRUE)))
  
  pb$tick()
}

save(inc_crps, inc_es, inc_vs, 
     exp_crps, exp_es, exp_vs, 
     aus_crps, aus_es, aus_vs, file = paste0("prob_scores", "_", type, ".RData"))
