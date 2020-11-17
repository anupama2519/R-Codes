
#PCA Analysis
#Q. Perform Principal component analysis and perform clustering using first 3 principal 
#   component scores (both heirarchial and k mean clustering(scree plot or elbow curve) and 
#   obtain optimum number of clusters and check whether we have obtained same number of 
#   clusters with the original data

#Read a csv file:

wine<-read.csv(file.choose())
View(wine)   #All the variables are continuous

#Delete column 1(No of clusters) :

wine_data<-wine[,-1]
View(wine_data)
summary(wine_data)
dim(wine_data)   #178  13

#Check for NULL Values:

unique(is.na(wine_data))  #False

#Graphical Representation :

hist(wine_data$Alcohol)
hist(wine_data$Malic)    
hist(wine_data$Ash)
hist(wine_data$Alcalinity)
hist(wine_data$Magnesium)  
hist(wine_data$Phenols)
hist(wine_data$Flavanoids)
hist(wine_data$Nonflavanoids)
hist(wine_data$Proanthocyanins)
hist(wine_data$Color)
hist(wine_data$Hue)
hist(wine_data$Dilution)
hist(wine_data$Proline)

#Data doesnt follows Normal Distribution

attach(wine_data)

#Check the correlation between variables:

cor(wine_data)

pairs(wine_data)

#No variable is highly corelated to each other.
#Some are having moderate corelation

#Building PCA Components for wine data set:
wine_pca<-princomp(wine_data,cor=TRUE,scores = TRUE,covmat = NULL)

#?princomp
summary(wine_pca)
wine_pca$loadings

#As per summary,first seven PCA components contains approx 90% of Information.

#Graphical Representation:

plot(wine_pca)
#from plot, we can make out that first three PCA components contains maximum information
#(Component 1,2,3 are having high variances)

biplot(wine_pca)

#Check for the PCA scores:

View(wine_pca$scores)

#Combined the PCA Scores of first three components in wine dataset:

wine_data<-cbind(wine_data,wine_pca$scores[,1:3])
View(wine_data)
summary(wine_data)

#Cluster analysis on first three PCA components :

wine_clust<-wine_pca$scores[,1:3]
#wine_clust<-wine_standard[,14:16]
View(wine_clust)
summary(wine_clust)

#Hierarchical clustering on first three components :

#Standardisation of data :
wine_standard_clust<-scale(wine_clust)

#Calculate Distance Matrix(Euclidean Method)

wine_dist<-dist(wine_standard_clust,method="euclidean")
str(wine_dist)
class(wine_dist)

#Calculate Clusters from complete linkage method :
clust_wine<-hclust(wine_dist,method = "complete")

#Plot a Diagram(Dendrogram):

plot(clust_wine)
plot(clust_wine,hang = -1) #hang=-1 is used to show the labels clearly below the plot 

#Draw rectangle around clusters for clear view:

rect.hclust(clust_wine,k=6,border="red")  #k=No. of clusters
# Size of each cluster is almost same.

#Cut the tree :
groups_wine<-cutree(clust_wine,k=6)
class(groups_wine)  #integer

#Add the groups_wine into wine_data data set:

wine_data<-cbind(wine_data,groups_wine)
View(wine_data)

#Change the groups_wine name to class :

colnames(wine_data)[17]<-"Class"
View(wine_data)

#Sort the wine data set by Class :
wine_data_final<-aggregate(wine_data,by=list(wine_data$Class),FUN=mean)

View(wine_data_final)

# K Means Clustering on first three PCA components :

#Elbow curve or scree plot to find the number of clusters:

twss = c()
for (i in 2:15) twss[i] = sum(kmeans(wine_standard_clust, centers=i)$withinss)

plot(1:15, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
# Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)

#Result: From Elbow curve, after 4 the drop is constant.. so we can say that No of clusters are 4 (k=4) 


#Apply K Means  for k=4
wine_kmeans<-kmeans(wine_standard_clust,4)
str(wine_kmeans)
wine_kmeans$cluster

#Append Cluster into Wine Original Data set:

wine_final_kmeans<-as.data.frame(cbind(wine,"Groups"=wine_kmeans$cluster))
View(wine_final_kmeans)

#Sort by Groups: 

wine_final_kmeans<-aggregate(wine_final_kmeans,by=list(wine_final_kmeans$Groups),FUN=mean)
wine_final_kmeans


#Result : PCA Analysis is used to reduce the dimensionality of data set.As per the original 
#wine dataset, Type variable represents the no of clusters are 3. But when we do cluster 
#analysis(Both Hiererichal and KMeans clustering) on first 3 PCA components, No of clusters
#are different. Hence, it shows that optimum number of clusters from original data set and
# PCA transformed data set are different.

  
