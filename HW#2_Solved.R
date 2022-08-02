rm(list=ls())
#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install(version = "3.14")
#BiocManager::install("multtest")
#BiocManager::install("cluster")
install.packages("fpc")
install.packages("bootcluster")
install.packages("ggfortify")
pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
install.packages("kohonen")
install.packages("recommenderlab")
library(kohonen)
library("multtest")
library("fpc")
library("cluster")
library("bootcluster")
library("fossil")
library("ggfortify")
library(factoextra)
library(NbClust)
library(recommenderlab)

### Question 3###########
#########################

#### Loading Iris data####
head(iris)
data("iris")
df= iris[,1:4] ## Storing the iris data with out species label 
##### Obtaining the first two principal components #####
Proj_1<- prcomp(df, center=TRUE, scale=TRUE)
summary(Proj_1)
names(Proj_1)
PC1= Proj_1$x[,1]
PC2=Proj_1$x[,2]
Proj_2=cbind(PC1,PC2)
## Plotting PCA#####

pca_plot <- autoplot(Proj_1,data = iris,colour = 'Species')
pca_plot
###### Applying K means to the Two principal Components######
km_pc <- kmeans(Proj_2, centers = 3, nstart = 10)
table(km_pc$cluster,iris$Species)

km_pc2 <- kmeans(Proj_2, centers = 2, nstart = 10)
table(km_pc2$cluster,iris$Species)


##### Plotting the clusters####
##### Providing Separate symbol to each cluster#####
plot(Proj_2, col = km_pc$cluster, main = "Clustering on PC1 and PC2 ",pch= km_pc$cluster,cex= 1)

plot(Proj_2, col = km_pc2$cluster, main = "Clustering on PC1 and PC2 ",pch= km_pc2$cluster,cex= 1)


###### Calculating rand and adj rand index with 3 clusters ######
Rand_clust= rand.index(km_pc$cluster, as.numeric(iris$Species))
Adj_rand_clust=adj.rand.index(km_pc$cluster, as.numeric(iris$Species))

###### Calculating rand and adj rand index with 2 clusters ######

Rand_clust2= rand.index(km_pc2$cluster, as.numeric(iris$Species))
Adj_rand_clust2=adj.rand.index(km_pc2$cluster, as.numeric(iris$Species))



#### Using Silhouette and Gap statistics to determine optimal number of cluster#####
fviz_nbclust(Proj_2, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
##### Gap Statistics #####
fviz_nbclust(Proj_2, kmeans, nstart = 25,  method = "gap_stat", nboot = 100)+
  labs(subtitle = "Gap statistic method")


####### Question 2#############
#############################
data(state)
head(state.x77)
df1= scale(state.x77)
rownames(df1)= NULL 
head(df1)
dim(df1)
###### Calculating the euclidean distance for cluster formation #####
d=dist(df1)
dim(as.matrix(d))
hir_clust= hclust(d,method = 'ave')
names(hir_clust)
summary(hir_clust)
plot(hir_clust, hang=-1, labels= hir_clust$labels, main= "Clustered Dendogram", ylab= "height",
     cex= 0.6)

#### Determining the optimal cluster #####
fviz_nbclust(df1, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(df1, hcut, nstart = 25,  method = "gap_stat", nboot = 100)+
  labs(subtitle = "Gap statistic method")

#### Clustering using SOM ########
set.seed(123)
som_grid= somgrid(xdim= 4 ,ydim= 4, topo = "hexagonal")
df1.som= som(df1,grid= som_grid, rlen = 500)
plot(df1.som)
names(df1.som)
df1.som$unit.classif
#### Plotting the SOM mapping in different format####
plot(df1.som, type = "mapping")
plot(df1.som, type= "changes")

######## Question 1#########
##############################
data("MovieLense")
head(MovieLense)
class(MovieLense)
rm_trix= MovieLense
class(rm_trix)
getRatingMatrix(rm_trix[1:20,1:20]) # Displaying the sparse rating matrix 
##### Normalizing the data ######
Norm_rm_trix= normalize(rm_trix)
##### Creating a recommender system #####
##############################################
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
#### Creating Top 10 recommendation for 3 users####

recommender_pop <- Recommender(rm_trix, method = "POPULAR")
RE_model <- predict(recommender_pop, rm_trix[940:942], n=10, type="ratings") ### Predicting top 10 movies of 3 users with its predicted rating 
as(RE_model, "list")
RE_model_top <- predict(recommender_pop, rm_trix[940:942], type="topNList")###### Predicting top 10 Movies of 3 users 
as(RE_model_top, "list")#### Displayed in list format. 






