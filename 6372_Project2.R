bc<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",header=F,sep=",")
names(bc)<- c('id_number', 'diagnosis', 'radius_mean', 
              'texture_mean', 'perimeter_mean', 'area_mean', 
              'smoothness_mean', 'compactness_mean', 
              'concavity_mean','concave_points_mean', 
              'symmetry_mean', 'fractal_dimension_mean',
              'radius_se', 'texture_se', 'perimeter_se', 
              'area_se', 'smoothness_se', 'compactness_se', 
              'concavity_se', 'concave_points_se', 
              'symmetry_se', 'fractal_dimension_se', 
              'radius_worst', 'texture_worst', 
              'perimeter_worst', 'area_worst', 
              'smoothness_worst', 'compactness_worst', 
              'concavity_worst', 'concave_points_worst', 
              'symmetry_worst', 'fractal_dimension_worst')

#Getting a look at the distribution
table(bc$diagnosis)

#Scatter plots color coded by response for just the first few variables
pairs(bc[,3:6],col=bc$diagnosis)

#Conduct PCA
pc.bc<-prcomp(bc[,-c(1,2)],scale.=TRUE)
pc.bc.scores<-pc.bc$x

#Adding the response column to the PC's data frame
pc.bc.scores<-data.frame(pc.bc.scores)
pc.bc.scores$Diagnosis<-bc$diagnosis

#Use ggplot2 to plot the first few pc's
library(ggplot2)
ggplot(data = pc.bc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=Diagnosis), size=1)+
  ggtitle("PCA of Breast Cancer Tumor Biopsies")

ggplot(data = pc.bc.scores, aes(x = PC2, y = PC3)) +
  geom_point(aes(col=Diagnosis), size=1)+
  ggtitle("PCA of Breast Cancer Tumor Biopsies")
