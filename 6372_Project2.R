# Include Libraries
library(tidyverse)
library(caret)
library(ggcorrplot)
library(kernlab)
library(ggplot2)

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

# Data Summary
summary(bc)

# Normalize Data
bc.clean <- bc[,-c(1)]
normalize <- function(x){
  return (( x - min(x))/(max(x) -min(x)))
}  
bc.clean.normalized <- as.data.frame(
  lapply(bc.clean[,2:31],normalize)
)  
bc.clean.normalized <- cbind(
  bc.clean[,1],
  bc.clean.normalized
)
names(bc.clean.normalized)[1] <- "diagnosis"

summary(bc.clean.normalized)

#Getting a look at the distribution
table(bc$diagnosis)

# Malignant and Benign Distribution
m_and_b <- bc.clean %>% 
  group_by(diagnosis) %>%
  summarise(n = n()) %>%
  mutate(percentage = signif((100 * n/sum(n)),2))

ggplot(data = m_and_b) +
  geom_bar(
    mapping = aes(x = "",y = percentage, fill = diagnosis), 
    stat = "identity", 
    width = 1) +
  geom_text(
    mapping = aes(x = c(1,1), y = c(69,18), 
                  label = paste(percentage,"%")), 
    size = 3) +
  coord_polar("y")

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
