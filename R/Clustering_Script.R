# Libraries
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(caret)
library(psych)
library(pastecs)
library(mice)
library("corrplot")
library(factoextra)
library(ggrepel)
library(clustertend)
library("colorspace")
library(cluster)

# Data Import
data <- read.csv("wdbc.data", header = TRUE, sep=",")
data <- data[1:12]
colnames <- c("ID", "Diagnosis", "radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave.points", "symmetry", "fractal.dimension" )
names(data) <- colnames

# Final version for clustering.
wdbc <- data[3:12]

head(wdbc)

# Descriptive Statistics 
summary(wdbc)
apply(wdbc,2,sd)

# Missing Value Handling
md.pattern(wdbc, plot= FALSE)

#Box Plots
boxplot(wdbc$radius, main = "radius",col=sequential_hcl(4, palette = "BurgYl"))
boxplot(wdbc$texture, main = "texture",col=sequential_hcl(4, palette = "BurgYl"))
boxplot(wdbc$perimeter, main = "perimeter",col=sequential_hcl(4, palette = "BurgYl"))
boxplot(wdbc$area, main = "area",col=sequential_hcl(4, palette = "BurgYl"))
boxplot(wdbc$smoothness, main = "smoothness",col=sequential_hcl(4, palette = "BurgYl"))
boxplot(wdbc$compactness, main = "compactness",col=sequential_hcl(4, palette = "BurgYl"))
boxplot(wdbc$concavity, main = "concavity",col=sequential_hcl(4, palette = "BurgYl"))
boxplot(wdbc$concave.points, main = "concave.points",col=sequential_hcl(4, palette = "BurgYl"))
boxplot(wdbc$symmetry, main = "symmetry",col=sequential_hcl(4, palette = "BurgYl"))
boxplot(wdbc$fractal.dimension, main = "fractal.dimension",col=sequential_hcl(4, palette = "BurgYl"))

# Correlation Between Variables
corr <- cor((wdbc), method = "pearson")
corr

corrplot.mixed(corr, lower="pie",upper="number")  

# Principal Component Analysis (PCA)
df.pca <- prcomp(wdbc, center = TRUE, scale. = TRUE)  
summary(df.pca)

(df.pca$sdev)^2

# Upon examining the output, it can be observed that the eigenvalues of the two principal components are greater than 1. 
# It indicates that with these two principal components, 80% of the variability in the data is explained.

fviz_eig(df.pca)

x <- fa.parallel(wdbc, fm="pa", fa="both", n.iter=1)

df_new <- df.pca$x[,1:2] #The new dataset corresponding to the two components. 

df.pca$rotation[,1:2] 


fviz_contrib(df.pca, choice = "var", axes = 1,top = 10)
fviz_contrib(df.pca, choice = "var", axes = 2,top = 10)



fviz_pca_ind(df.pca, axes = c(1,2),
             col.ind = "cos2",gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE )

# When examining the contributions of observations in the PC1 and PC2 plot, a clustering on the right side can be observed.

wdbc[c(461,101,504,237),]
summary(wdbc)

wdbc[c(461,101,504,237),]
# The values 461., 101., 504., and 237. have been examined.

fviz_pca_var(df.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


cor(df.pca$x[,1],df.pca$x[,2])


# Distance/Similarity Measures
dist.euc=get_dist(df_new, method="euclidean")
fviz_dist(dist.euc)


# Hopkins Test
set.seed(123)
hopkins.data <- hopkins(wdbc,n=nrow(wdbc)-1)
hopkins.data

# Clustering Analysis

# K-Means

# Elbow Method

set.seed(123)
fviz_nbclust(df_new,kmeans,method = "wss",nstart = 25)


# Silhouette Method

set.seed(123)
fviz_nbclust(df_new,kmeans,method = "silhouette") #for average silhouette width


# Gap Statistics Method

set.seed(123)
fviz_nbclust(df_new,kmeans,method = "gap_stat", nboot=100)



# Clusters Created with K-Means Method

set.seed(123)
km_res <- kmeans(df_new, 2, nstart=25) 
print(km_res)

fviz_cluster(km_res, data = df_new,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


set.seed(123)
km_res3 <- kmeans(df_new, 3, nstart=25) 
print(km_res)


fviz_cluster(km_res3, data = df_new,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



set.seed(123)
km_res5 <- kmeans(df_new, 5, nstart=25) 
print(km_res)


fviz_cluster(km_res5, data = df_new,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)




# K-medoids

# Elbow Method

set.seed(123)
fviz_nbclust(df_new,pam,method = "wss")


# Silhouette Method

set.seed(123)
fviz_nbclust(df_new,pam, method = "silhouette")


# Gap Statistics Method

set.seed(123)
fviz_nbclust(df_new,kmeans,method = "gap_stat", nboot=100)


# Clusters Created with K-Medoids Method


set.seed(123)
pam_df_2 <- pam(df_new,2)
#print(pam_df_2)


fviz_cluster(pam_df_2,
             ellipse.type = "convex"
             , repel = TRUE
             , axes = c(1,2)
             
)




set.seed(123)
pam_df_3 <- pam(df_new,3)
#print(pam_df_2)



fviz_cluster(pam_df_3,
             ellipse.type = "convex"
             , repel = TRUE
             
             
)



set.seed(123)
pam_df_2 <- pam(df_new,5)
#print(pam_df_2)
fviz_cluster(pam_df_2,
             ellipse.type = "convex"
             , repel = TRUE
             
             
)

# Upon examining the clusters, it was observed that selecting 5 clusters in our dataset could not prevent heterogeneity. 
# When 2 clusters were used, one of the clusters had a significantly larger variance. In the case of 3 clusters, 
# there were some overlapping values, albeit not as much in the k-medoids method. It was decided that the best 
# separation was achieved with 3 clusters using the k-means algorithm.


# Kümeleme Sonuçları

aggregate(wdbc, by=list(cluster=km_res3$cluster), mean)
summary(wdbc)

# If we compare the cluster means with the initial means of the variables:
# In cluster 1, the means of texture and fractal dimension variables are the same. The means of the other variables are lower than their actual values.
# In cluster 2, the means of radius, perimeter, and area variables are lower. The mean of texture is the same. The means of the other variables are higher within the cluster.
# In cluster 3, except for fractal dimension, the means of all variables are higher. The mean of fractal dimension is the same.


diag <- (as.factor(data$Diagnosis))

data_d[data_d$Diagnosis == 'M',]
data_d[data_d$Diagnosis == 'B',]

data_d <- data[2:12]
summary(data_d)

# We can say that the second and third clusters represent malignant tumors.
# The first cluster represents benign tumors.
