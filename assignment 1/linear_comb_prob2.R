library(xlsx)

read_file<-function()
{
  data <- read.xlsx("data1.xls",1,header = TRUE)
  return(data)
}

linear_combination_mean <- function(mean_vector,coefficient_vector)
{
  mean_value = t(coefficient_vector) %*% mean_vector
  return(mean_value)
}

linear_combination_variance <- function(covariance_matrix,coefficient_vector)
{
  variance_value = t(coefficient_vector)%*% covariance_matrix %*% (coefficient_vector)
  return(variance_value)
}

mean_and_variance<-function(coefficient_vector,mean_vector,covariance_matrix)
{
  mean_value <- linear_combination_mean(mean_vector,coefficient_vector)
  variance_value <- linear_combination_variance(covariance_matrix,coefficient_vector)
  result <- c(mean_value,variance_value)
  return(result)
}

get_mean_vector<-function(data)
{
  mean_y1 = mean(data$Y1)
  mean_y2 = mean(data$Y2)
  mean_y3 = mean(data$Y3)
  mean_y4 = mean(data$Y4)
  mean_y5 = mean(data$Y5)
  mean_vector = matrix(c(mean_y1,mean_y2,mean_y3,mean_y4,mean_y5),nrow = 5,ncol = 1)
}

get_variance_matrix<- function(data,mean_vector)
{
  data_matrix <- matrix(c(data$Y1,data$Y2,data$Y3,data$Y4,data$Y5),nrow = 252,ncol = 5)
  unit_matrix <- matrix(1,nrow = 252,ncol = 252)
  product <- (unit_matrix %*% data_matrix)/252
  data_matrix <- data_matrix - product
  data_matrix <- t(data_matrix) %*% (data_matrix)
  data_matrix <- data_matrix/252
  return(data_matrix)
}

linear_combination <- function()
{
  data <- read_file()
  mean_vector <- get_mean_vector(data)
  covariance_matrix <- get_variance_matrix(data,mean_vector)
  
  z1 <- matrix(c(3,1,-3,4,7),nrow = 5,ncol =1)
  results1 <- mean_and_variance(z1,mean_vector,covariance_matrix)
  View(results1)
  z2 <- matrix(c(-1,1,-1,2,9),nrow = 5,ncol =1)
  results2 <- mean_and_variance(z2,mean_vector,covariance_matrix)
  View(results2)
  z3 <- matrix(c(3,1,0,0,5),nrow = 5,ncol =1)
  results3 <- mean_and_variance(z3,mean_vector,covariance_matrix)
  View(results3)
  z4 <- matrix(c(6,3,1,11,10),nrow = 5,ncol =1)
  results4 <- mean_and_variance(z4,mean_vector,covariance_matrix)
  View(results4)
}

linear_combination()