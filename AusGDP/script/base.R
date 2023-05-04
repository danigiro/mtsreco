library(forecast)
library(tibble)
library(progress)

if(!exists("side")){
  side <- "inc"
}

if(side == "inc"){
  load("./inc/INC_inpdata.RData")
  data <- as.matrix(Inc)
  data <- as_tibble(data)
  data$date <- zoo::as.yearqtr(time(Inc$Gdpi))
  data <- data[,c("date", colnames(Inc))]
  output_files <- "./inc/base.RData"
}else{
  load("./exp/EXP_inpdata.RData")
  data <- as.matrix(Exp)
  data <- as_tibble(data)
  data$date <- zoo::as.yearqtr(time(Exp$Gdpe))
  data <- data[,c("date", colnames(Exp))]
  output_files <- "./exp/base.RData"
}

freq <- 4
test_date_h1 <- data$date[data$date>="1994 Q3"]
start_date <- min(data$date)
h <- 4
test_array <- array(NA, dim = c(h, NCOL(data)-1, length(test_date_h1)),
                    dimnames = list(NULL, colnames(data)[-1], NULL))

base_array <- array(NA, dim = c(h, NCOL(data)-1, length(test_date_h1)),
                    dimnames = list(NULL, colnames(data)[-1], NULL))
res_array <- array(NA, dim = c(NROW(data), NCOL(data)-1, length(test_date_h1)),
                   dimnames = list(NULL, colnames(data)[-1], NULL))
scale <- matrix(NA, ncol = NCOL(data)-1, nrow = length(test_date_h1))
colnames(scale) <- colnames(data)[-1]
model <- tibble(id = character(),
                rep = integer(),
                model = list())
#for(i in test_date_h1[-length(test_date_h1)]){
for(i in test_date_h1){
  train <- data[data$date<i,-1]
  test <- data[data$date>=i,-1]
  test_array[1:min(4, nrow(test)),,which(test_date_h1 == i)] <- as.matrix(test[1:min(4, nrow(test)),])
  
  pb <- progress_bar$new(format = paste0("(", side, ") Replication n. ", which(test_date_h1 == i), 
                                         " [:bar] :percent in :elapsed (ETA: :eta)"),
                         total = NCOL(train), clear = FALSE, width= 60, show_after = 0)
  for(j in colnames(train)){
    y <- ts(train[,j, drop = TRUE], start = start_date, frequency = freq)
    fity <- forecast(auto.arima(y), h = h)
    
    base_array[1:min(4, nrow(test)),j,which(test_date_h1 == i)] <- fity$mean[1:min(4, nrow(test))]
    res_array[1:length(fity$residuals),j,which(test_date_h1 == i)] <- fity$x - fity$fitted
    scale[which(test_date_h1 == i),j] <- mean(abs(diff(fity$x, lag = freq, differences = 1)))
    model <- add_row(model, id = j, model = list(fity), rep = which(test_date_h1 == i))
    pb$tick()
  }
  #cat("Replication n.", which(test_date_h1 == i))
}

save(base_array, res_array, test_array, scale, model, file = output_files)
