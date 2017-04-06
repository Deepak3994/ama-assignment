library(xlsx)

read_file<-function()
{
 data <- read.xlsx("iris data.xls",1,header = TRUE)
 return(data)
}

Manova<-function(data)
{
  Y <- cbind(data$V2,data$V3,data$V4,data$V5)
  iris_fit <- manova(Y ~ data$V1,data = data)
  summary(iris_fit, test="Pillai")
  summary(iris_fit,test = "Roy")
  summary(iris_fit,test = "Wilks")
  summary(iris_fit,test = "Hotelling-Lawley")
  
  summary.aov(iris_fit)
}

iris_manova<-function()
{
  data<-read_file()
  Manova(data)
}
iris_manova()
