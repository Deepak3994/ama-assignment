library(xlsx)
library(MASS)

read_file<-function()
{
  data <- read.xlsx("football data.xls",1,header = TRUE)
  return(data)
}

discriminant_function<-function(data)
{
  r <- lda(formula = Group ~ WDIM+CIRCUM+FBEYE +EYEHD+EARHD,data=data,CV = TRUE)
  return(r)
}

data <- read_file()
fit <- discriminant_function(data)
table(fit$class,data[,1])
r1 <-lda(formula = Group ~ WDIM+CIRCUM+FBEYE +EYEHD+EARHD,data=data)
prop = r1$svd^2/sum(r1$svd^2)
prop