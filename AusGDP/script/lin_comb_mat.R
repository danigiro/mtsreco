library(Matrix)
load("./inc/INC_inpdata.RData")
nb <- m
na <- n-nb
rownames(S) <- colnames(Inc)
colnames(S) <- colnames(Inc)[-c(1:na)]
Cinc <- S[1:na,]
save(Cinc, nb, na, n, file = "./inc/Cmat.RData")

load("./exp/EXP_inpdata.RData")
nb <- m
na <- n-nb
rownames(S) <- colnames(Exp)
colnames(S) <- colnames(Exp)[-c(1:na)]
Cexp <- S[1:na,]
save(Cexp, nb, na, n, file = "./exp/Cmat.RData")

library(FoReco)
rownames(Cexp)[1] <- rownames(Cinc)[1] <- "Gdp"
ord_name_col_c <- c(colnames(Cexp), colnames(Cinc))
ord_name_row <- c(rownames(Cexp), rownames(Cinc))
ord_name_col_u <- c(unique(ord_name_row), colnames(Cexp), colnames(Cinc))
Ut <- cbind(matrix(0, nrow = length(ord_name_row), ncol = length(unique(ord_name_row))), -bdiag(list(Cexp, Cinc)))
colnames(Ut) <- ord_name_col_u
rownames(Ut) <- ord_name_row
for(i in unique(ord_name_row)){
  Ut[ord_name_row == i, ord_name_col_u == i] <- 1
}
Cgdp <- lcmat(Ut)$Cbar
save(Cgdp, file = "./aus/Cmat.RData")
