library(tibble); library(magrittr); library(dplyr); library(kernlab); library(caret)

# Example 1 -------------------------------------------
# create data
x <- seq(-20,20,0.1) # 401 entries
y <- sin(x)/x + rnorm(401,sd=0.05)

# train relevance vector machine
foo <- rvm(x, y)
foo
# print relevance vectors
alpha(foo)
RVindex(foo)

# predict and plot
ytest <- predict(foo, x)
plot(x, y, type ="l")
lines(x, ytest, col="red")

# Example 2 -------------------------------------------
# key_clusters <- readRDS("key_clusters.Rds")
dfx <- readRDS("dfx.Rds")
x <- dfx %>% pull(T1_cl_wo_novel_size)
y <- dfx %>% pull(m2)
y <- ((y - min(y))/(max(y) - min(y)))/10


rvmfit <- rvm(x, y)
rvmfit

alpha(rvmfit)
RVindex(rvmfit)

ypred <- predict(rvmfit, x)
plot(x, y, type = "l")
lines(x, ypred, col="red")

# 10-fold cross  - EXAMPLE ----------------------------
# Load the data
data("swiss")
# Inspect the data
sample_n(swiss, 3)
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# RVM with linear kernel
# Train the model
model_lin <- train(Fertility ~., data = swiss, method = "rvmLinear",
               trControl = train.control)

# RVM with polynomial kernel
model_pol <- train(Fertility ~., data = swiss, method = "rvmPoly",
               trControl = train.control)

# RVM with radial basis kernel function
model_rad <- train(Fertility ~., data = swiss, method = "rvmRadial",
               trControl = train.control)

# Summarize the results
print(model_lin)
print(model_pol)
print(model_rad)

# 10-fold cross  - our data ----------------------------
dfx <- readRDS("dfx.Rds") %>% 
  dplyr::select(h1, T1_cl_wo_novel_size, m2)

# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# RVM with linear kernel
# Train the model
model_lin <- train(m2 ~., data = dfx, method = "rvmLinear",
               trControl = train.control)

# RVM with polynomial kernel
# model_pol <- train(m2 ~., data = dfx, method = "rvmPoly",
#                trControl = train.control)

# RVM with radial basis kernel function
# model_rad <- train(m2 ~., data = dfx, method = "rvmRadial",
#                trControl = train.control)

# Summarize the results
print(model_lin)
# print(model_pol)
# print(model_rad)

# 10-fold cross ----------------------------------------
dfx <- readRDS("dfx.Rds") %>% 
  dplyr::select(h1, T1_cl_wo_novel_size, m2, Label, prop)

# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# RVM with linear kernel
# Train the model
model_lin <- train(Label ~., data = dfx, method = "svmLinearWeights2",
               trControl = train.control)

# Summarize the results
print(model_lin)


