library(FoReco)
library(forecast)
library(progress)

source("R/prgd.R")

if(!exists("side")){
  side <- "inc"
}

if(side == "inc"){
  load("./inc/base.RData")
  load("./inc/Cmat.RData")
  output_files <- "./inc/reco_prob.RData"
  path_files <- "./inc/path"
  
  C <- Cinc
}else if(side == "exp"){
  load("./exp/base.RData")
  load("./exp/Cmat.RData")
  output_files <- "./exp/reco_point.RData"
  path_files <- "./exp/path"
  
  C <- Cexp
}else{
  load("./aus/base.RData")
  load("./aus/Cmat.RData")
  output_files <- "./aus/reco_point.RData"
  path_files <- "./aus/path"
  
  C <- Cgdp
}
H <- 4
B <-5000

id <- c(rownames(C), colnames(C))
method  <- c("ols", "wls", "shr")
set.seed(1234)
path <- NULL
for(i in 1:dim(base_array)[3]){
  base <- na.omit(base_array[,id,i])
  res <- na.omit(res_array[,id,i])
  
  pb <- progress_bar$new(format = paste0("Replication n. ", i, " reco path",
                                         " [:bar] :percent in :elapsed (ETA: :eta)"),
                         total = length(method), clear = FALSE, width= 60, show_after = 0)
  for(j in method){
    recf <- FoReco::htsrec(basef = base, res = res, comb = j, C = C,
                                 keep = "list")
    
    # Method 1: Sigma = M W
    meth1_cov <- recf$M %*% recf$W
    meth1_path <- simplify2array(lapply(1:NROW(recf$recf), function(x){
      MASS::mvrnorm(n = B, mu = recf$recf[x,], Sigma = meth1_cov)
    }))
    path[[j]]$meth1 <- aperm(simplify2array(meth1_path), c(3,2,1))
    
    # Method 2: Sigma = M We M
    meth2_cov <- recf$M %*% (crossprod(res)/NROW(res)) %*% t(recf$M)
    meth2_path <- simplify2array(lapply(1:NROW(recf$recf), function(x){
      MASS::mvrnorm(n = B, mu = recf$recf[x,], Sigma = meth2_cov)
    }))
    path[[j]]$meth2 <- aperm(simplify2array(meth2_path), c(3,2,1))
    
    # Method 3: Sigma = M Ws M
    meth3_cov <- recf$M %*% (FoReco::shrink_estim(res)$scov) %*% t(recf$M)
    meth3_path <- simplify2array(lapply(1:NROW(recf$recf), function(x){
      MASS::mvrnorm(n = B, mu = recf$recf[x,], Sigma = meth3_cov)
    }))
    path[[j]]$meth3 <- aperm(simplify2array(meth3_path), c(3,2,1))
    
    # Method 4: Sigma = M Wf M, dove Wf = 
    R <- cov2cor(crossprod(res)/NROW(res))
    model_list <- setNames(model$model[model$rep==i], model$id[model$rep==i])
    model_list <- model_list[id]
    s <- sapply(model_list, function(x) ((x$upper[, "95%"] - x$lower[, "95%"])/qnorm(0.975))/2)
    meth4_cov <- apply(s, 1, function(x) diag(x)%*%R%*%diag(x), simplify = FALSE)
    meth4_path <- simplify2array(lapply(1:NROW(recf$recf), function(x){
      MASS::mvrnorm(n = B, mu = recf$recf[x,], Sigma = recf$M%*%meth4_cov[[x]]%*%t(recf$M))
    }))
    path[[j]]$meth4 <- aperm(simplify2array(meth4_path), c(3,2,1))
    pb$tick()
  }
  
  path$base$meth1 <- aperm(simplify2array(lapply(1:NROW(base), function(x){
    MASS::mvrnorm(n = B, mu = base[x,], Sigma = diag(diag(FoReco::shrink_estim(res)$scov)))
  })), c(3,2,1))
  
  path$base$meth2 <- aperm(simplify2array(lapply(1:NROW(base), function(x){
    MASS::mvrnorm(n = B, mu = base[x,], Sigma = FoReco::shrink_estim(res)$scov)
  })), c(3,2,1))
  
  path$base$meth3 <- aperm(simplify2array(lapply(1:NROW(base), function(x){
    MASS::mvrnorm(n = B, mu = base[x,], Sigma = diag(s[x,]^2))
  })), c(3,2,1))
  
  path$base$meth4 <- aperm(simplify2array(lapply(1:NROW(base), function(x){
    MASS::mvrnorm(n = B, mu = base[x,], Sigma = meth4_cov[[x]])
  })), c(3,2,1))
  
  save(path, file = paste0(path_files, "/gaussian/mod_rep", i, ".RData"))
}