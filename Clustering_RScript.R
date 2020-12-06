#import dataset
seeds_dataset <- read.delim("~/DSC441/week10/seeds_dataset.txt", header=FALSE)
View(seeds_dataset)

#Rename Variables
colnames(seeds_dataset) = c("Area","Perimeter","Compactness","Kernel_Length",
    "Kernel_Width","Asymmetry_Coefficient","kernel_Groove_Length","Class_label")
seeds_dataset$Class_label[Dat]


#Get a summary of the Data
summary(seeds_dataset)

#install factoextra package for visualization

#Perform Kmeans Clustering
set.seed(20)
three_clusters<-kmeans(seeds_dataset[,1:7],3)
str(three_clusters)
print(three_clusters)
fviz_cluster(three_clusters,data = seeds_dataset)


four_clusters<-kmeans(seeds_dataset[,1:7],4)
str(four_clusters)
print(four_clusters)
fviz_cluster(four_clusters,data = seeds_dataset)

five_clusters<-kmeans(seeds_dataset[,1:7],5)
str(five_clusters)
print(five_clusters)
fviz_cluster(five_clusters,data = seeds_dataset)

six_clusters<-kmeans(seeds_dataset[,1:7],6)
str(six_clusters)
print(six_clusters)
fviz_cluster(six_clusters,data = seeds_dataset)

#table to see class distribution
table(seeds_dataset$Class_label,three_clusters$cluster)
table(seeds_dataset$Class_label,four_clusters$cluster)
table(seeds_dataset$Class_label,five_clusters$cluster)
table(seeds_dataset$Class_label,six_clusters$cluster)

#compare Kmeans Plots

k3Plot<-fviz_cluster(three_clusters,geom = "point", data = seeds_dataset) + ggtitle("K=3")
k4Plot<-fviz_cluster(four_clusters,geom = "point", data = seeds_dataset) + ggtitle("K=4")
k5Plot<-fviz_cluster(five_clusters,geom = "point", data = seeds_dataset) + ggtitle("K=5")
k6Plot<-fviz_cluster(six_clusters,geom = "point", data = seeds_dataset) + ggtitle("K=6")

library(gridExtra)
grid.arrange(k3Plot,k4Plot, k5Plot, k6Plot, nrow = 2)

#Determine which K should be selected using the elbow method
set.seed(125)

#function to compute the within-cluster sum of square
within_Clus_SumSq <-function(k)
{
  kmeans(seeds_dataset[,1:7], k, nstart = 10)$tot.withinss
}

#Use elbow method to find the optimal K value
fviz_nbclust(seeds_dataset[,1:7],kmeans,method = "wss")

#Part IV: normalize the data
norm_seeds_dataset <- scale(seeds_dataset[,1:7])
head(norm_seeds_dataset)

#Perform Kmeans on normalized data
set.seed(20)
norm_three_clusters<-kmeans(norm_seeds_dataset[,1:7],3)
str(norm_three_clusters)
print(norm_three_clusters)
fviz_cluster(norm_three_clusters,data = norm_seeds_dataset)

set.seed(20)
norm_six_clusters<-kmeans(norm_seeds_dataset[,1:7],6)
str(norm_six_clusters)
print(norm_six_clusters)
fviz_cluster(norm_six_clusters,data = norm_seeds_dataset)

set.seed(20)
norm_four_clusters<-kmeans(norm_seeds_dataset[,1:7],4)
str(norm_four_clusters)
print(norm_four_clusters)
fviz_cluster(norm_four_clusters,data = norm_seeds_dataset)


 
#compare normalized Kmeans Plots

norm_k3Plot<-fviz_cluster(norm_three_clusters,geom = "point", data = norm_seeds_dataset) + ggtitle("K=3")
norm_k4Plot<-fviz_cluster(norm_four_clusters,geom = "point", data = norm_seeds_dataset) + ggtitle("K=4")
norm_k5Plot<-fviz_cluster(norm_five_clusters,geom = "point", data = norm_seeds_dataset) + ggtitle("K=5")
norm_k6Plot<-fviz_cluster(norm_six_clusters,geom = "point", data = norm_seeds_dataset) + ggtitle("K=6")

library(gridExtra)
grid.arrange(norm_k3Plot,norm_k4Plot, norm_k5Plot, norm_k6Plot, nrow = 2)

#Use elbow method to find the optimal K value with normalized data
fviz_nbclust(norm_seeds_dataset[,1:7],kmeans,method = "wss")

#Part II
#Single linkage
norm_seed_dataset_df<-as.data.frame(norm_seeds_dataset)
summary(norm_seed_dataset_df)

set.seed(800)
seeds_eucl_dist<- dist(norm_seed_dataset_df, method = "euclidean")
hclust_single<- hclust(seeds_eucl_dist, method = "single")
plot(hclust_single)

cut_single<-cutree(hclust_single, h=3, K=3)

suppressPackageStartupMessages(library(dendextend))
single_dend<- as.dendrogram(hclust_single)

suppressPackageStartupMessages(library(dplyr))
seeds_single_dend <-mutate(seeds_dataset,cluster = cut_single)
count(seeds_single_dend,cluster)
table(seeds_single_dend$cluster,seeds_dataset$Class_label)


#apply complete linkage

set.seed(800)
seeds_eucl_dist<- dist(norm_seed_dataset_df, method = "euclidean")
hclust_complete<- hclust(seeds_eucl_dist, method = 'complete')
plot(hclust_complete)

cut_complete<-cutree(hclust_complete, k=4, h=3)

suppressPackageStartupMessages(library(dendextend))
single_dend<- as.dendrogram(hclust_complete)

suppressPackageStartupMessages(library(dplyr))
seeds_complete_dend <-mutate(seeds_dataset,cluster = cut_complete)
count(seeds_complete_dend,cluster)
table(seeds_complete_dend$cluster,seeds_dataset$Class_label)


