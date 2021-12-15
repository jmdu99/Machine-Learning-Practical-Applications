ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) -
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr", "RWeka","FactoMineR", "ggpubr","EMCluster")
ipak(packages)

df <- read.arff("normalized_manual_binary_clust.arff")

# PCA 
pca <- PCA(X = df, graph = FALSE)

# PCA variables plot
pca0 = fviz_pca_var(pca)

# % variance in each dimension 
pca1 = fviz_eig(pca, addlabels=TRUE, hjust = -0.3)

# Variables that contribute the most
pca2 = fviz_contrib(pca, choice = "var", axes = 1)
pca3 = fviz_contrib(pca, choice = "var", axes = 2)

# Multiple plots in 1 figure
ggpubr::ggarrange(pca0,pca1,pca2,pca3)

#Estimate number of clusters for ward method
ward_num_clusters<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "ward.D2", index = "alllong")
fviz_nbclust(ward_num_clusters)

#Estimate number of clusters for complete method
complete_num_clusters<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "complete", index = "alllong")
fviz_nbclust(complete_num_clusters)

#Estimate number of clusters for centroid method
centroid_num_clusters<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "centroid", index = "alllong")
fviz_nbclust(centroid_num_clusters)

#Clustering jerárquico método complete k=2
jerq_complete <- hcut(df, hc_method= "complete", k = 2)

#Cluster plots
jerq_comp1 = fviz_cluster(jerq_complete, data = df)
fviz_cluster(jerq_complete, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(jerq_complete, data = df, ellipse.type = "norm")
fviz_cluster(jerq_complete, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

jerq_comp2 = fviz_dend(jerq_complete, rect = TRUE, cex = 0.5,
                  k_colors = c("#2E9FDF","red"))

# Multiple plots in 1 figure
ggpubr::ggarrange(jerq_comp1,jerq_comp2)

#Clustering jerárquico método centroid k=2
jerq_centroid <- hcut(df, hc_method= "centroid", k = 2)

#Cluster plots
jerq_cent1 = fviz_cluster(jerq_centroid, data = df)
jerq_cent2 = fviz_dend(jerq_centroid, rect = TRUE, cex = 0.5,
                       k_colors = c("#2E9FDF","red"))
# Multiple plots in 1 figure
ggpubr::ggarrange(jerq_cent1,jerq_cent2)

#Clustering jerárquico método ward k=2
jerq_ward2 <- hcut(df, hc_method= "ward.D2", k = 2)

#Clustering jerárquico método ward k=3
jerq_ward3 <- hcut(df, hc_method= "ward.D2", k = 3)

#Clustering jerárquico método ward k=4
jerq_ward4 <- hcut(df, hc_method= "ward.D2", k = 4)

#Cluster plots
ward0 = fviz_cluster(jerq_ward2, data = df)
ward1 = fviz_cluster(jerq_ward3, data = df)
ward2 = fviz_dend(jerq_ward2, rect = TRUE, cex = 0.5,
                       k_colors = c("#2E9FDF","red"))
ward3 = fviz_dend(jerq_ward3, rect = TRUE, cex = 0.5,
                  k_colors = c("#2E9FDF","green","red"))
ward4 = fviz_cluster(jerq_ward4, data = df)
ward5 = fviz_dend(jerq_ward4, rect = TRUE, cex = 0.5,
                  k_colors = c("purple","green","#2E9FDF","red"))
# Multiple plots in 1 figure
ggpubr::ggarrange(nrow = 3,ncol = 2,ward0,ward2,ward1,ward3,ward4,ward5)


#---------------------------------------------------------------------------------------

#Estimate number of clusters for kmeans method
kmeans_num_clusters<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(kmeans_num_clusters)

#Clustering kmeans with k= 2
kmeans2 <- kmeans(df, centers = 2, nstart = 25)

#Clustering kmeans with k= 3
kmeans3 <- kmeans(df, centers = 3, nstart = 25)

# Cluster plots
k2 = fviz_cluster(kmeans2, data = df)
k3 = fviz_cluster(kmeans3, data = df)
ggpubr::ggarrange(k2,k3)
fviz_cluster(kmeans, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(kmeans, data = df, ellipse.type = "norm")
fviz_cluster(kmeans, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

#---------------------------------------------------------------------------------------
# Data about clusters
df %>%
  mutate(Cluster = jerq$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Add cluster to dataframe and transform
df$Cluster<-as.factor(kmeans3$cluster)
df_long <- gather(df, Feature, Mean, Polyuria:Obesity, factor_key=TRUE)

# Plot
ggplot(df_long, aes(x = Feature, y = Mean,group=Cluster, colour = Cluster)) + 
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")




