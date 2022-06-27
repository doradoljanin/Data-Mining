# authors: Dora Doljanin, Filip Pavicic
# EDAMI Task3: Clustering
# The aim of the task is to determine the best clustering according to the evaluation methods presented during the laboratory.
# Some methods include: Partitioning algorithms (K-means, K-medoids), Hierarchical clustering, etc.
# For this task we will be using the Cardiotocography Data Set, which consists of measurements of fetal heart rate (FHR) and 
# uterine contraction (UC) features on cardiotocograms classified by expert obstetricians.
# Also, we will be using the Wine Quality Data Set, which includes data related to red and white vinho verde wine samples 
# from the north of Portugal. Specifically, we will be using the red wine dataset.

library(Rcpp)
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)


accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}



#data set 1 for the laboratory task
#http://archive.ics.uci.edu/ml/datasets/Cardiotocography 

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_noClass_corr.csv','cardioto_noClass_corr.csv')
ctg_noClass <- read.csv("cardioto_noClass_corr.csv",row.names = 1)
head(ctg_noClass)
# we do not use s the fields Class and NSP during the analysis

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_all_corr.csv','cardioto_all_corr.csv')
ctg_all <- read.csv("cardioto_all_corr.csv",row.names = 1)

set.seed(7777)

#example
distC = dist(ctg_noClass)
card.kmeans = kmeans(distC,10)
res3 = table(ctg_all$CLASS,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

# The obtained accuracy is 0.4081729
# However, the kmeans algotithm might give different results each time it is executed
# because kmeans picks up random initial points to cluster the data every time.
# Our goal is to achieve results better than the results obtained by using k-means algorithm
# for 10 groups with default values of the parameters and without data pre-processing. (40.81%)

# We will try to find the best clustering according to the evaluation methods 
# presented during the laboratory.

##### kmeans ##################

# Rand index
# The corrected Rand index provides a measure for assessing the similarity between
# two partitions, adjusted for chance. Its range is -1 (no agreement) to 1 (perfect agreement).
# The reference grouping is defined by the attribute Class
classes <- as.numeric(ctg_all$CLASS)
clust_stats<-cluster.stats(d=distCS, classes, card.kmeans$cluster)
clust_stats$corrected.rand
# R = 0.1210153
# This R is very low, so we want to try to get better results than that.

summary(ctg_noClass)
#we can see that scales for different attributes vary a lot so
# to archive better results we need to scale the data in each column

#### data scaling

ctg_scale = scale(ctg_noClass, center = FALSE)
summary(ctg_scale)

# now we can try now to repeat the kmeans algorithm to see if we
# can obtain better results

distCS = dist(ctg_scale)
card.kmeans = kmeans(ctg_scale,10)
res3 = table(ctg_all$CLASS,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

# now we obtained an accuracy of 0.4720526, which is a little better, but still not good enough


###############################################################
# finding the optimal number of groups with the "elbow" method
###############################################################

# we try to find the optimal number of groups with the "elbow" method
wss <- vector(mode = "integer" ,length = 15)
# Maximum number of groups to consider: 15 (the reference grouping has 10 groups).
#  1 to 15 clusters
for (i in 1:15) {
  kmeans.group <- kmeans(distCS, centers = i, nstart=20)
  # total within-cluster sum of squares
  wss[i] <- kmeans.group$tot.withinss
}

# total within-cluster sum of squares per number of groups
plot(1:15, wss, type = "b", 
     xlab = "number of groups", 
     ylab = "total within-cluster sum of squares")

# The "elbow" method suggests that the best number of clusters is 6 or 10, but it is hard to tell 
# for sure because the fall of the graph is pretty gradual.

# To be more sure, we will also try the silhouette method for finding the optimal number of clusters.
# A silhouette is a measurement that considers how closely related objects are within the cluster 
# and how clusters are separated from each other.
# The silhouette value usually ranges from 0 to 1; a value closer to 1 suggests
# the data is better clustered.

?eclust
# n_clusters = 2
km_alt<-eclust(ctg_scale, "kmeans", k=2, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")
# The silhouette plot shows that the n_cluster value of 2 is not a good choice because all the points in
# the cluster with the cluster_label=1 are below-average silhouette scores (red dotted line).

# n_clusters = 3
km_alt<-eclust(ctg_scale, "kmeans", k=3, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")
# This silhouette plot shows us that the n_cluster value of 3 might also not be the optimal choice,
# because almost all the points in the cluster with cluster_label=3 are below-average silhouette scores.

# n_clusters = 4
km_alt<-eclust(ctg_scale, "kmeans", k=4, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")
# For n_clusters=4, the silhouette method shows that it is not the best choice because almost all the points
# in the cluster with cluster_label=3 are below-average silhouette scores.

# n_clusters = 5
km_alt<-eclust(ctg_scale, "kmeans", k=5, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")
# For n_clusters=5, none of the clusters has all of its points below the average silhouette scores line,
# but the plots are not of very similar thickness and sizes as we would like them to be.

# n_clusters = 6
km_alt<-eclust(ctg_scale, "kmeans", k=6, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")
# For n_clusters=6, the situation is not much better because the silhouettes still vary in size a lot.

# n_clusters = 7
km_alt<-eclust(ctg_scale, "kmeans", k=7, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 8
km_alt<-eclust(ctg_scale, "kmeans", k=8, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 9
km_alt<-eclust(ctg_scale, "kmeans", k=9, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 10
km_alt<-eclust(ctg_scale, "kmeans", k=10, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 11
km_alt<-eclust(ctg_scale, "kmeans", k=11, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# We want to find the number of clusters which has the least differences between thicknesses of silhouettes.
# For n_clusters=7, 8, 9 the differences between thicknesses of the silhouettes are smaller and smaller.
# For n_clusters=10 the differences between thicknesses of the silhouettes is even smaller.
# For n_clusters>10 we get a warning message:
# This manual palette can handle a maximum of 10 values. You have supplied 11.

# The silhouette method suggests that the best number of clusters might be 10.

####################################
# PAM - partitioning around medoids#
####################################

# deciding on the optimal number of clusters
fviz_nbclust(ctg_scale, pam, method = "silhouette")+theme_classic()

# Based on the silhouettes average width, the best cluster number might be 2 or 7 because 
# for those values we have peaks. For that reason, we will try clustering for 
# n_clusters = 2 and n_clusters = 7

# division into 2 clusters
pam.res <- pam(ctg_scale, 2)
res3 = table(ctg_all$CLASS,pam.res$cluster)
accuracyCalc(res3,1) 
# accuracy = 0.339 
#clustering visualization
fviz_cluster(pam.res,
             palette="jco"
)

# division into 7 clusters
pam.res <- pam(ctg_scale, 7)
res3 = table(ctg_all$CLASS,pam.res$cluster )
accuracyCalc(res3,1) 
# accuracy =  0.4387036
#clustering visualization
fviz_cluster(pam.res,
             palette="jco"
)

# We are not very satisfied with the obtained accuracies for n_clusters = 2 and n_clusters = 7,
# so we will try the value of n_clusters suggested by the "elbow" method and the silhouette method, 
# which is n_clusters = 10.

# division into 10 clusters
pam.res <- pam(ctg_scale, 10)
res3 = table(ctg_all$CLASS,pam.res$cluster )
accuracyCalc(res3,1) 
# accuracy =  0.5129169 
#clustering visualization
fviz_cluster(pam.res,
             palette="jco"
)

#### hierarchical clustering

?hclust
#calculation of a distance matrix
?dist
distM = dist(ctg_scale)
distT = as.matrix(distM)
dim(distT)
distT[1:5,1:5]

#hierarchical clustering for different linkage methods
ctg_scale.hc_complete <- hclust(distM, method="complete")
ctg_scale.hc_single <- hclust(distM, method="single")
ctg_scale.hc <- hclust(distM, method="average")
ctg_scale.hc_centroid <- hclust(distM, method="centroid")

# generate clusters
?cutree
ctg_scale.hc.groups <- cutree(ctg_scale.hc, k=10)
ctg_scale.hc.groups
# compare clusters with original class labels
res3 = table(ctg_all$CLASS,ctg_scale.hc.groups)
accuracyCalc(res3,1) 
# accuracy = 0.2935651

# We observe that the accuracy obtained by hierarchical clustering is pretty low.

##### dbscan algorithm ##########################

?dbscan

# MinPts parameter estimation:
# The idea is to calculate the average of the distances of every point to its k nearest neighbors. 
# The value of k will be specified by the user and corresponds to MinPts.
# Next, these k-distances are plotted in an ascending order.
# The aim is to determine the “knee”, which corresponds to the optimal eps parameter.
# A knee corresponds to a threshold where a sharp change occurs along the k-distance curve.

dbscan::kNNdistplot(ctg_scale, k=10)
abline(h=5, lty="dashed")

# dbscan alg. execution
ctg_scale.dbscan <- dbscan(ctg_scale, eps=3, MinPts=5)

#compare clusters with original class labels
#cluster 0 means noise
res3 = table(ctg_all$CLASS,ctg_scale.dbscan$cluster )
accuracyCalc(res3,1) 
# accuracy = 0.2799436

# plot clusters
plot(ctg_scale.dbscan, ctg_scale)
plot(ctg_scale.dbscan, ctg_scale[c(1,4)])

# We are not satisfied with the obtained accuracy, so we change the parameter MinPts to the
# number of clusters suggested by previous methods (n_clusters=10)

# dbscan alg. execution
ctg_scale.dbscan <- dbscan(ctg_scale, eps=3, MinPts=10)

#compare clusters with original class labels
#cluster 0 means noise
res3 = table(ctg_all$CLASS,ctg_scale.dbscan$cluster )
accuracyCalc(res3,1) 
# accuracy = 0.2837013

# plot clusters
plot(ctg_scale.dbscan, ctg_scale)
plot(ctg_scale.dbscan, ctg_scale[c(1,4)])

# However, the accuracy did not improve much.

# We try changing the eps parameter

# dbscan alg. execution
ctg_scale.dbscan <- dbscan(ctg_scale, eps=0.5, MinPts=10)

#compare clusters with original class labels
#cluster 0 means noise
res3 = table(ctg_all$CLASS,ctg_scale.dbscan$cluster )
accuracyCalc(res3,1) 
# accuracy = 0.3250352

# plot clusters
plot(ctg_scale.dbscan, ctg_scale)
plot(ctg_scale.dbscan, ctg_scale[c(1,4)])

0.3250352 - 0.2837013
# We managed to raise the accuracy for 4.13%

# Using the "elbow" method and the silhouette method, we determined that the best clustering for 
# the Cardiotocography Data Set was n_clusters = 10
# Overall, the best accuracy for the Cardiotocography Data Set that we managed to achieve was 
# 51.29% using the PAM algorithm (partitioning around medoids).
# We expected that the best obtained accuracy would be higher.
# However, our goal was to achieve results better than the results obtained by using k-means algorithm
# for 10 groups with default values of the parameters and without data pre-processing. (40.81%)
# Thus, we may conclude that we have reached our goal, as we obtained an accuracy that is 10.48% 
# higher than the starting accuracy.


# data set 2 for the laboratory task
# http://archive.ics.uci.edu/ml/datasets/Wine+Quality

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');

wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")
head(wineRed_ds)
#During the analysis (clustering) the field quality should 
#not be taken into consideration, so we remove it
wineRed_dsC <- wineRed_ds[,-12]
head(wineRed_dsC)

#example - wines
distC = dist(wineRed_dsC)
card.kmeans = kmeans(distC,6)
res3 = table(wineRed_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)
# The obtained accuracy = 0.4865541

# Our goal is to achieve results better than the results obtained by using k-means algorithm
# for 6 groups with default values of the parameters and without data pre-processing. (48.65%)

# We will try to find the best clustering according to the evaluation methods 
# presented during the laboratory.

##### kmeans ##################

# Rand index
# The corrected Rand index provides a measure for assessing the similarity between
# two partitions, adjusted for chance. Its range is -1 (no agreement) to 1 (perfect agreement).
classes <- as.numeric(wineRed_ds$quality)
clust_stats<-cluster.stats(d=distC, classes, card.kmeans$cluster)
clust_stats$corrected.rand
# R = 0.002888379
# This R is very low, so we want to try to get better results than that.

summary(wineRed_dsC)
#we can see that scales for different attributes vary a lot so
# to archive better results we need to scale the data in each column

#### data scaling

wine_scale = scale(wineRed_dsC, center = FALSE)
summary(wine_scale)

# now we can try now to repeat the kmeans algorithm to see if we
# can obtain better results

distCS = dist(wine_scale)
card.kmeans = kmeans(wine_scale,10)
res3 = table(wineRed_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

# now we obtained an accuracy of 0.4965604, which is a little better, but still not good enough

###############################################################
# finding the optimal number of groups with the "elbow" method
###############################################################

# we try to find the optimal number of groups with the "elbow" method
wss <- vector(mode = "integer" ,length = 15)
#  1 to 15 clusters
for (i in 1:15) {
  kmeans.group <- kmeans(distCS, centers = i, nstart=20)
  # total within-cluster sum of squares
  wss[i] <- kmeans.group$tot.withinss
}

# total within-cluster sum of squares per number of groups
plot(1:15, wss, type = "b", 
     xlab = "number of groups", 
     ylab = "total within-cluster sum of squares")

# It is very hard to tell which number of clusters is optimal using the "elbow" method
# because the fall of the graph is gradual.
# To be more sure, we will also try the silhouette method for finding the optimal number of clusters.
# we will try some clustering options for n_clusters = 2, 3, ..., 10
# n_clusters = 2
km_alt<-eclust(wine_scale, "kmeans", k=2, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 3
km_alt<-eclust(wine_scale, "kmeans", k=3, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 4
km_alt<-eclust(wine_scale, "kmeans", k=4, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 5
km_alt<-eclust(wine_scale, "kmeans", k=5, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 6
km_alt<-eclust(wine_scale, "kmeans", k=6, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 7
km_alt<-eclust(wine_scale, "kmeans", k=7, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 8
km_alt<-eclust(wine_scale, "kmeans", k=8, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 9
km_alt<-eclust(wine_scale, "kmeans", k=9, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# n_clusters = 10
km_alt<-eclust(wine_scale, "kmeans", k=10, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")


####################################
# PAM - partitioning around medoids#
####################################

# deciding on the optimal number of clusters
fviz_nbclust(wine_scale, pam, method = "silhouette")+theme_classic()

# Based on the silhouettes average width, the best cluster number might be 3 or 7 because 
# for those values we have peaks. For that reason, we will try clustering for 
# n_clusters = 3 and n_clusters = 7

# division into 3 clusters
pam.res <- pam(wine_scale, 3)
res3 = table(wineRed_ds$quality,card.kmeans$cluster )
accuracyCalc(res3,1) 
# accuracy = 0.4965604  
#clustering visualization
fviz_cluster(pam.res,
             palette="jco"
)

# division into 7 clusters
pam.res <- pam(wine_scale, 7)
res3 = table(wineRed_ds$quality,card.kmeans$cluster )
accuracyCalc(res3,1) 
# accuracy =  0.4965604
#clustering visualization
fviz_cluster(pam.res,
             palette="jco"
)

# We are not very satisfied with the obtained accuracies for n_clusters = 3 and n_clusters = 7,
# so we will try with n_clusters = 6.

# division into 6 clusters
pam.res <- pam(wine_scale, 6)
res3 = table(wineRed_ds$quality,card.kmeans$cluster )
accuracyCalc(res3,1) 
# accuracy =  0.4965604
#clustering visualization
fviz_cluster(pam.res,
             palette="jco"
)
# However, the accuracy did not improve at all.

#### hierarchical clustering

?hclust
#calculation of a distance matrix
?dist
distM = dist(wine_scale)
distT = as.matrix(distM)
dim(distT)
distT[1:5,1:5]

#hierarchical clustering for different linkage methods
wine_scale.hc_complete <- hclust(distM, method="complete")
wine_scale.hc_single <- hclust(distM, method="single")
wine_scale.hc <- hclust(distM, method="average")
wine_scale.hc_centroid <- hclust(distM, method="centroid")

# generate clusters
?cutree
wine_scale.hc.groups <- cutree(wine_scale.hc, k=6)
wine_scale.hc.groups
# compare clusters with original class labels
res3 = table(wineRed_ds$quality,wine_scale.hc.groups)
accuracyCalc(res3,1) 
# accuracy = 0.4277674

##### dbscan algorithm ##########################

?dbscan

# MinPts parameter estimation:
# The idea is to calculate the average of the distances of every point to its k nearest neighbors. 
# The value of k will be specified by the user and corresponds to MinPts.
# Next, these k-distances are plotted in an ascending order.
# The aim is to determine the “knee”, which corresponds to the optimal eps parameter.
# A knee corresponds to a threshold where a sharp change occurs along the k-distance curve.

# dbscan alg. execution
wine_scale.dbscan <- dbscan(wine_scale, eps=3, MinPts=5)

#compare clusters with original class labels
#cluster 0 means noise
res3 = table(wineRed_ds$quality,ctg_scale.dbscan$cluster )
accuracyCalc(res3,1) 
# accuracy = 0.4258912

# plot clusters
plot(wine_scale.dbscan, wine_scale)
plot(wine_scale.dbscan, wine_scale[c(1,4)])

# Using the "elbow" method we could not unambiguously determine which n_clusters
# would be the best clustering for the Wine Quality Data Set, while the 
# silhouette method led us to incorrect number of clusters (3 or 7).
# Overall, the best accuracy for the Wine Quality Data Set that we managed to achieve was 
# 49.65% using the kmeans algorithm and the PAM algorithm.
# We expected that the best obtained accuracy would be higher. Our goal was to achieve results 
# better than the results obtained by using k-means algorithm
# for 6 groups with default values of the parameters and without data pre-processing. (48.65%)
# Although we did obtain an accuracy that is 1% higher than the starting accuracy, we are not
# very satisfied with the results since the accuracy did not show a significant improvement.
