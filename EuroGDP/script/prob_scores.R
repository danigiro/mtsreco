library(progress)
library(furrr)
library(scoringRules)
library(Matrix)

if(!exists("type")){
  type <- "boot"
}

load("./data/group_names.RData")
load("./data/agg_gdp.RData")
C <- Cgdp

load("./data/base.RData",  temp_env <- new.env())
ea_test <- as.list(temp_env)$test_array

plan(multisession, workers = 12)
H <- 4
B <- 5000
ea_crps <- ea_es <- ea_vs <- NULL
pb <- progress_bar$new(format = paste0("[:bar] :percent in :elapsed (ETA: :eta)"),
                       total = dim(ea_test)[3], clear = FALSE, width= 60, show_after = 0)
for(i in 1:dim(ea_test)[3]){
  load(paste0("./data/path/", type, "/mod_rep", i, ".RData"))
  test <- ea_test[!is.na(ea_test[,1,i]),,i, drop = FALSE]
  
  if(is.list(path[[1]])){
    path <- unlist(path,recursive=FALSE)
    #path_tmp$base <- path$base
    #path <- path_tmp
  }
  
  # CRPS
  ea_crps[[i]] <- simplify2array(future_map(path, function(xpath){
    tmp <- matrix(NA, nrow = dim(test)[2], ncol = H)
    tmp[,1:NROW(test)] <- sapply(1:NROW(test), function(x) crps_sample(test[x,,], dat = xpath[x,,],
                                                                       method = "edf"))
    rownames(tmp) <- c(rownames(C), colnames(C))
    as.matrix(tmp)
  }))
  
  # ES
  ea_es[[i]] <- simplify2array(future_map(path, function(xpath){
    test_list <- split(test[,,], 1:NROW(test))
    split_geo <- sapply(colnames(xpath), function(x) strsplit(x, " ")[[1]][1])
    split_hie <- sapply(colnames(xpath), function(x) strsplit(x, " ")[[1]][2])
    
    tmp <- array(NA, c(6, H, 24), 
                 dimnames = list(c("All", "All (no GDP)","Free variables", "Basic variables", "Basic variables (no GDP)", "GDP"), 
                                 NULL, c("all", "Exp", "Inc", "Pro", names$Geo)))
    tmp[1,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath, 1)[1:length(test_list)], y = test_list))
    tmp[2,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath[,-1,,drop=FALSE], 1)[1:length(test_list)], y =  split(test[,-1,], 1:NROW(test))))
    #tmp[3,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath[,colnames(C),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(C),], 1:NROW(test))))
    #tmp[4,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath[,rownames(C),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(C),], 1:NROW(test))))
    #tmp[5,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath[,rownames(C)[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(C)[-1],], 1:NROW(test))))
    tmp[6,1:NROW(test),"all"] <- unname(mapply(es_sample, dat = asplit(xpath[,split_hie=="GDP",,drop=FALSE], 1)[1:length(test_list)], y = split(test[,split_hie=="GDP",], 1:NROW(test))))
    
    for(j in c("Exp", "Inc", "Pro")){
      #id <- split_hie %in% c(names[[j]]$uts, names[[j]]$bts)
      #tmp[1,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      id <- split_hie %in% c(names[[j]]$uts[-1], names[[j]]$bts)
      tmp[2,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
                                             y = split(test[,id,], 1:NROW(test))))
      #id <- split_hie %in% c(names[[j]]$bts)
      #tmp[3,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- split_hie %in% c(names[[j]]$uts)
      #tmp[4,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- split_hie %in% c(names[[j]]$uts[-1])
      #tmp[5,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- split_hie == "GDP"
      #tmp[6,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
    }
    
    for(j in names$Geo){
      id <- split_geo == j
      tmp[1,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
                                             y = split(test[,id,], 1:NROW(test))))
      #id <- (split_geo == j) & (split_hie != "GDP")
      #tmp[2,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- (split_geo == j) & (split_hie %in% unique(unlist(sapply(names[1:3], function(x) x$bts))))
      #tmp[3,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- (split_geo == j) & (split_hie %in% unique(unlist(sapply(names[1:3], function(x) x$uts))))
      #tmp[4,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- (split_geo == j) & (split_hie %in% unique(unlist(sapply(names[1:3], function(x) x$uts[-1]))))
      #tmp[5,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- (split_geo == j) & (split_hie == "GDP")
      #tmp[6,1:NROW(test),j] <- unname(mapply(es_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
    }
    tmp
  },
  .options = furrr_options(seed = TRUE)))
  
  # VS
  ea_vs[[i]] <- simplify2array(future_map(path, function(xpath){
    test_list <- split(test[,,], 1:NROW(test))
    split_geo <- sapply(colnames(xpath), function(x) strsplit(x, " ")[[1]][1])
    split_hie <- sapply(colnames(xpath), function(x) strsplit(x, " ")[[1]][2])
    
    tmp <- array(NA, c(6, H, 24), 
                 dimnames = list(c("All", "All (no GDP)","Free variables", "Basic variables", "Basic variables (no GDP)", "GDP"), 
                                 NULL, c("all", "Exp", "Inc", "Pro", names$Geo)))
    tmp[1,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath, 1)[1:length(test_list)], y = test_list))
    tmp[2,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath[,-1,, drop = FALSE], 1)[1:length(test_list)], y =  split(test[,-1,], 1:NROW(test))))
    #tmp[3,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath[,colnames(C),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,colnames(C),], 1:NROW(test))))
    #tmp[4,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath[,rownames(C),,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(C),], 1:NROW(test))))
    #tmp[5,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath[,rownames(C)[-1],,drop=FALSE], 1)[1:length(test_list)], y = split(test[,rownames(C)[-1],], 1:NROW(test))))
    tmp[6,1:NROW(test),"all"] <- unname(mapply(vs_sample, dat = asplit(xpath[,split_hie=="GDP",,drop=FALSE], 1)[1:length(test_list)], y = split(test[,split_hie=="GDP",], 1:NROW(test))))
    
    for(j in c("Exp", "Inc", "Pro")){
      #id <- split_hie %in% c(names[[j]]$uts, names[[j]]$bts)
      #tmp[1,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      id <- split_hie %in% c(names[[j]]$uts[-1], names[[j]]$bts)
      tmp[2,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
                                             y = split(test[,id,], 1:NROW(test))))
      #id <- split_hie %in% c(names[[j]]$bts)
      #tmp[3,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- split_hie %in% c(names[[j]]$uts)
      #tmp[4,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- split_hie %in% c(names[[j]]$uts[-1])
      #if(sum(id)!=0){
      #  tmp[5,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                         y = split(test[,id,], 1:NROW(test))))
      #}
      #id <- split_hie == "GDP"
      #tmp[6,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
    }
    
    for(j in names$Geo){
      id <- split_geo == j
      tmp[1,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
                                             y = split(test[,id,], 1:NROW(test))))
      #id <- (split_geo == j) & (split_hie != "GDP")
      #tmp[2,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- (split_geo == j) & (split_hie %in% unique(unlist(sapply(names[1:3], function(x) x$bts))))
      #tmp[3,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- (split_geo == j) & (split_hie %in% unique(unlist(sapply(names[1:3], function(x) x$uts))))
      #tmp[4,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
      #id <- (split_geo == j) & (split_hie %in% unique(unlist(sapply(names[1:3], function(x) x$uts[-1]))))
      #tmp[5,1:NROW(test),j] <- unname(mapply(vs_sample, dat = asplit(xpath[,id,,drop=FALSE], 1)[1:length(test_list)], 
      #                                       y = split(test[,id,], 1:NROW(test))))
    }
    tmp
  },
  .options = furrr_options(seed = TRUE)))
  pb$tick()
}

save(ea_crps, ea_es, ea_vs, file = paste0("./data/prob_scores", "_", type, ".RData"))
