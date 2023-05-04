library(abind)
library(tibble)
library(Matrix)
load("./aus/Cmat.RData")
id <- c(rownames(Cgdp), colnames(Cgdp))

load("./inc/base.Rdata",  temp_env <- new.env())
inc_list <- as.list(temp_env)

load("./exp/base.Rdata",  temp_env <- new.env())
exp_list <- as.list(temp_env)

rm(temp_env)


base_array <- abind(exp_list$base_array, inc_list$base_array[,-1,], along = 2)
colnames(base_array)[colnames(base_array)=="Gdpe"] <- "Gdp"
base_array <- base_array[,id,]

test_array <- abind(exp_list$test_array, inc_list$test_array[,-1,], along = 2)
colnames(test_array)[colnames(test_array)=="Gdpe"] <- "Gdp"
test_array <- test_array[,id,]

res_array <- abind(exp_list$res_array, inc_list$res_array[,-1,], along = 2)
colnames(res_array)[colnames(res_array)=="Gdpe"] <- "Gdp"
res_array <- res_array[,id,]

scale <- cbind(exp_list$scale, inc_list$scale[,-1])
colnames(scale)[colnames(scale)=="Gdpe"] <- "Gdp"
scale <- scale[,id]

model <- rbind(exp_list$model,
      inc_list$model[inc_list$model$id != "Gdpi",])
model$id[model$id == "Gdpe"] <- "Gdp"

save(base_array, res_array, test_array, scale, model, file = "./aus/base.RData")
