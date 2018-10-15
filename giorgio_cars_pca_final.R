library(dplyr)
cars_ds = read.table("cars-PCA.txt",
                     col.names = c("mpg", "cylinders", "disp", "hp", "wt", "acc", "yr", "og", "name"))
cars_ds
# we have duplicated cars names and it seems thats because the year is not included in the picture.
# so what we do is to create another column combining the two of those
# and abbreviate the name to try to get better graphs later
cars_ds[duplicated(cars_ds$name),]$name

rownames(cars_ds) <- paste(abbreviate(cars_ds$name),cars_ds$yr, sep="_")
head(cars_ds)
glimpse(cars_ds)
#take only what we need
cars.pc = select(cars_ds,  mpg, disp, hp, wt, acc, og, cylinders)
cars.pc$og = factor(cars.pc$og)
cars.pc$cylinders = factor(cars.pc$cylinders)
#most of the cars are from the US
table(cars.pc$og)

#the values on the diagonal are really too different
cov(cars.pc[,1:5])
cor(cars.pc[,1:5])

library(ggplot2)
library(GGally)
ggpairs(cars.pc, lower = list(continuous="points",combo="facetdensity",mapping=aes(color=og)))
#there is a lot of linear combination going on
#also all the distributions seems to have two peaks, do we have two populations?
cars.pc
eigen(cor(cars.pc[,1:5]))
#in fact the last two eigenvalues are pretty low
#they seem to be between mpg, disp, mpg
#let's go for the pca
library(FactoMineR)
cars.pca = PCA(cars.pc, quali.sup = c(6,7),scale.unit = TRUE, graph = FALSE)
cars.pca$eig
dimdesc(cars.pca)
cars.pca$var$contrib

#seems that with the first 2 PC we get 94% of the total variance. Not bad...
cars.pca$var$cor

#plotting
library(factoextra)
fviz_pca_ind(cars.pca,  label="none")
#there is a clear separation of the observations in the first dimension.
#while the second dimension seems to mainly identify outliers in upper side.
#Plot also the alignment of the individuals using the color scale
fviz_pca_ind(cars.pca, col.ind="cos2", repel=TRUE, label="none"  ) +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50)

#circle of correlation
fviz_pca_var(cars.pca, label="var", col.var="contrib")
#biplots
#this one for the paper
fviz_pca_biplot(cars.pca, label="var", col.ind="cos2")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50)

fviz_pca_biplot(cars.pca, label="var", habillage = "og")
fviz_pca_biplot(cars.pca, label="var", habillage = "cylinders")

plotellipses(cars.pca,7, label="none")

library(ggbiplot)
#With this library the groups are shown cleaner
ggbiplot(cars.pca, groups=cars.pc$cylinders, ellipse=TRUE) +
  theme(legend.direction = 'horizontal', legend.position = 'top')+
  scale_color_discrete(name = 'Cylinders') 

ggbiplot(cars.pca, groups=cars.pc$og, ellipse=TRUE, var.scale = 1) +
  theme(legend.direction = 'horizontal', legend.position = 'top')+
  scale_color_discrete(name = 'Origin') 

#for the clustering we need to do it by hand...
cars.clust = kmeans(cars.pca$ind$coord,3)
cars.clust
cars.pc$kcluster <- factor(cars.clust$cluster)
cars.pc
cars.pc$origin <- factor(cars_ds$og)

cars.pca = PCA(cars.pc, scale.unit = TRUE,quali.sup = c(6,7,8))
cars.pca$ind$coord[,1]
cars.pc$origin


library(ggplot2)
cars_pca_obs = data.frame(C1=cars.pca$ind$coord[,1],C2=cars.pca$ind$coord[,2], origin = factor(cars_ds$og), cluster=cars.pc$kcluster, cylinders=cars.pc$cylinders)
cars_pca_obs

#some cars on the right are american
#all non-US producers are on the left
#only some US cars are on the left, maybe trying to imitate 
#EU's/Jp's efficiency
#should be better to biplot with the arrows
ggplot(cars_pca_obs, aes(x=C1,y=C2, label=rownames(cars_pca_obs)))+
  geom_hline(yintercept=0, color="gray70")+
  geom_vline(xintercept=0,color="gray70")+
  geom_point(aes(color=cluster, shape=origin), alpha=0.55, size=4)+
  geom_text(aes(color=cluster),alpha=0.55)

ggplot(cars_pca_obs, aes(x=C1,y=C2))+
  geom_hline(yintercept=0, color="gray70")+
  geom_vline(xintercept=0,color="gray70")+
  geom_point(aes(color=cluster, shape=origin), alpha=0.55, size=4)

ggplot(cars_pca_obs, aes(x=C1,y=C2))+
  geom_hline(yintercept=0, color="gray70")+
  geom_vline(xintercept=0,color="gray70")+
  geom_point(aes(color=cylinders, shape=cluster), alpha=0.55, size=4)


#it is a good idea to make biplots with the cluster

#PC plot with clustering partition
#cereal_pca_obs=data.frame(C1=cereal_pca_r$ind$coord[,1],C2=cereal_pca_r$ind$coord[,2], cluster=factor(cereal_clusters))
#ggplot(cereal_pca_obs, aes(x=C1,y=C2, label=rownames(cereal.pc)))+
#  geom_hline(yintercept=0, color="gray70")+
#  geom_vline(xintercept=0,color="gray70")+
#  geom_point(aes(color=cluster), alpha=0.55, size=2)+
#  geom_text(aes(color=cluster),alpha=0.55)
