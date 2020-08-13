library(e1071); library(tibble); library(magrittr); library(dplyr)

# Functions --------------------------------------------
make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

# SVM steps --------------------------------------------
dfx <- readRDS("dfx.Rds")# %>% as_tibble()
x <- dfy <- dfx %>% dplyr::select(h1, T1_cl_wo_novel_size) #%>% as.matrix()
# colnames(x) <- NULL
y <- dfx$dif %>% gsub("general", -1, .) %>% gsub("notable", 1, .) %>% 
  as.integer()
plot(dfy, col = y + 3)

# # key_clusters <- readRDS("key_clusters.Rds")
# dfy <- tibble(x = dfx$h1, y = dfx$T1_cl_wo_novel_size)
# plot(dfy, col = y + 1)

dat = data.frame(y = factor(y), x)
svmfit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)

xgrid = expand.grid(h1 = seq(from = 0, to = 180), T1_cl_wo_novel_size = seq(from = 1, to = 127))
# xgrid = expand.grid(h1 = x$h1, T1_cl_wo_novel_size = x$T1_cl_wo_novel_size)
ygrid = predict(svmfit, xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
# Example -------------------------------------------
load(file = "ESL.mixture.rda")
names(ESL.mixture)
attach(ESL.mixture)

plot(x, col = y + 1)

dat = data.frame(y = factor(y), x)
svmfit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(svmfit, xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)
