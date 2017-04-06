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

mean_and_variance<-function(coefficient_vector)
{
  mean_vector <- c(26,18,51)
  covariance_matrix <- matrix(c(70,35,50,35,92,30,50,30,85),nrow = 3, ncol = 3)
  mean_value <- linear_combination_mean(mean_vector,coefficient_vector)
  variance_value <- linear_combination_variance(covariance_matrix,coefficient_vector)
  result <- c(mean_value,variance_value)
  return(result)
}

linear_combination <- function()
{
  
  z1 <- matrix(c(2,4,-1),nrow = 3,ncol =1)
  results1 <- mean_and_variance(z1)
  View(results1)
  z2 <- matrix(c(1,1,-3),nrow = 3,ncol =1)
  results2 <- mean_and_variance(z2)
  View(results2)
  z3 <- matrix(c(6,-2,1),nrow = 3,ncol =1)
  results3 <- mean_and_variance(z3)
  View(results3)
  z4 <- matrix(c(1,-4,-5),nrow = 3,ncol =1)
  results4 <- mean_and_variance(z4)
  View(results4)
}

linear_combination()