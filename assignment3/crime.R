library(xlsx)

read_file<-function()
{
  data <- read.xlsx("crime rates in India 2011.xls",1,header= TRUE)
  data<- na.omit(data)
}

Hclust<-function(data)
{
  Clust1 <- hclust(dist(model.matrix(~-1 + Burglary_rate+Dacoity_rate+Mruder_rate+
        Other_crimes_rate+Riots_rate+Robbery_rate+Theft_rate,data)) , method= "ward.D2")
  
  plot(Clust1, main= "Cluster Dendrogram for Solution HClust.14", xlab= 
          "Observation Number in Data Set data", 
        sub="Method=ward.D2; Distance=euclidian")
  
  Clust2 <- hclust(dist(model.matrix(~-1 + Burglary_rate+Dacoity_rate+Mruder_rate+
                                        Other_crimes_rate+Riots_rate+Robbery_rate+Theft_rate,data)) , method= "single")
  
  plot(Clust2, main= "Cluster Dendrogram for Solution HClust.14", xlab= 
         "Observation Number in Data Set data", 
       sub="Method=single; Distance=euclidian")
  
  Clust3 <- hclust(dist(model.matrix(~-1 + Burglary_rate+Dacoity_rate+Mruder_rate+
                                        Other_crimes_rate+Riots_rate+Robbery_rate+Theft_rate,data)) , method= "complete")
  
  plot(Clust3, main= "Cluster Dendrogram for Solution HClust.14", xlab= 
         "Observation Number in Data Set data", 
       sub="Method=complete; Distance=euclidian")
  
  Clust4 <- hclust(dist(model.matrix(~-1 + Burglary_rate+Dacoity_rate+Mruder_rate+
                                        Other_crimes_rate+Riots_rate+Robbery_rate+Theft_rate,data)) , method= "average")
  
  plot(Clust4, main= "Cluster Dendrogram for Solution HClust.14", xlab= 
         "Observation Number in Data Set data", 
       sub="Method=average; Distance=euclidian")
  
  Clust5 <- hclust(dist(model.matrix(~-1 + Burglary_rate+Dacoity_rate+Mruder_rate+
                                        Other_crimes_rate+Riots_rate+Robbery_rate+Theft_rate,data)) , method= "median")
  
  plot(Clust5, main= "Cluster Dendrogram for Solution HClust.14", xlab= 
         "Observation Number in Data Set data", 
       sub="Method=median; Distance=euclidian")
  
  Clust6 <- hclust(dist(model.matrix(~-1 + Burglary_rate+Dacoity_rate+Mruder_rate+
                                        Other_crimes_rate+Riots_rate+Robbery_rate+Theft_rate,data)) , method= "centroid")
  plot(Clust6, main= "Cluster Dendrogram for Solution HClust.14", xlab= 
         "Observation Number in Data Set data", 
       sub="Method=centroid; Distance=euclidian")
  
}

K_means<-function(data)
{
  cluster <-  KMeans(model.matrix(~-1 + Burglary_rate + Dacoity_rate + Mruder_rate + Other_crimes_rate + 
                                    Riots_rate + Robbery_rate + Theft_rate, data), centers = 4, iter.max = 20, num.seeds = 5) 
}


clustering<-function()
{
data<-read_file()
Hclust(data)
K_means(data)
}

clustering()
