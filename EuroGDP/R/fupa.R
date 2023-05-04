#' FUture PAth
#'
#' @param B Number of bootstrap samples
#' @param fit List of models
#' @param H Forecasts Horizons
#' @param idx Index to get the block of bootstrap samples
#' @param seed next
#'
#' @return 3 objs: path (array B x n x H), idx (innovation index), seed (of innovation)
#' @export
#'
fupa <- function(B, fit, H, idx, seed){
  out <- list()
  
  if(is(fit[[1]], "forecast")){
    fit <- lapply(fit, function(x) x$model)
  }
  res <- simplify2array(lapply(fit, function(x) x$residuals))

  if(missing(idx)){
    if(missing(seed)){
      seed <- sample(1:1000, 1)
    }
    set.seed(seed)
    idx_start <- base::sample(c(1:(NROW(res)-(H-1))), size = B , replace = TRUE)
    idx <- sapply(idx_start, function(x) x:(x+H-1))
    
    out$seed <- seed
  }

  inn <- apply(idx, 2, function(x){
    as.list(as.data.frame(res[x,]))
    }, simplify = FALSE)
  
  path <- lapply(inn, function(xi)
    mapply(simulate, fit, future = TRUE, nsim = H, innov = xi))

  out$path <- simplify2array(path)
  out$idx <- idx
  return(out)
}


fupa_map <- function(B, fit, H, idx, seed){
  out <- list()
  
  if(is(fit[[1]], "forecast")){
    fit <- future_map(fit, function(x) x$model)
  }
  res <- simplify2array(future_map(fit, function(x) x$residuals))
  
  if(missing(idx)){
    if(missing(seed)){
      seed <- sample(1:1000, 1)
    }
    set.seed(seed)
    idx_start <- base::sample(c(1:(NROW(res)-(H-1))), size = B , replace = TRUE)
    idx <- sapply(idx_start, function(x) x:(x+H-1))
    
    out$seed <- seed
  }
  
  inn <- future_map(asplit(idx, 2), function(x){
    as.list(as.data.frame(res[x,]))
  })
  
  path <- future_map(inn, function(xi)
    mapply(simulate, fit, future = TRUE, nsim = H, innov = xi))
  
  out$path <- simplify2array(path)
  out$idx <- idx
  return(out)
}
