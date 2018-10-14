library(dplyr)
cars_ds = read.table("/home/giorgio/Uni/IntelligentDataAnalysys/Homerworks/1_4/me/cars-PCA.txt",
                     col.names = c("mpg", "cylinders", "disp", "hp", "wt", "acc", "yr", "og", "name"))
cars_ds
#we have duplicated cars names and it seems thats because the year is not included in the picture.
#so what we do is to create another column combining the two of those
#and abbreviate the name to try to get better graphs later
cars_ds[duplicated(cars_ds$name),]$name

rownames(cars_ds) <- paste(abbreviate(cars_ds$name),cars_ds$yr, sep="_")
head(cars_ds)


glimpse(cars_ds)
#take only what we need
cars.pc = select(cars_ds,  mpg, disp, hp, wt, acc, og, cylinders)
cars.pc$og = factor(cars.pc$og)
cars.pc$cylinders = factor(cars.pc$cylinders)
table(cars.pc$og)
#nicer when plotting
head(cars.pc)

#the values on the diagonal are really too different
cov(cars.pc[,1:5])

cor(cars.pc[,1:5])

library(ggplot2)
library(GGally)
ggpairs(cars.pc, lower = list(continuous="points",combo="facetdensity",mapping=aes(color=og)))
#there is a lot of liner combination going on, mpg
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
#let's see what we have
summary(cars.pca)
#seems that with the first 2 PC we get 94% of the total variance. Not bad...
#The variables contributions to the PCA are similarly distributed between mpg, disp, hp, wt
#Like PCA1 is a measure of "americanness" of a car
#While the accelleration seems to contribute most to the second PCA
#like PCA2 is a metric for the "prestazioni"
cars.pca$var$cor

#plotting
plot(cars.pca, cex=0.7)
#the names are a mess

library(factoextra)
fviz_pca_ind(cars.pca,  label="none")
#now its better-> there is a clear separation of the observations in the first dimension.
#while the second dimension seems to mainly identify outliers in upper side.
fviz_pca_ind(cars.pca, col.ind="cos2", repel=TRUE, label="none"  ) +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50)

fviz_pca_var(cars.pca, label="var", col.var="contrib")
#biplots
#this one for the paper
fviz_pca_biplot(cars.pca, label="var", col.ind="cos2")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50)

fviz_pca_biplot(cars.pca, label="var", habillage = "og")
fviz_pca_biplot(cars.pca, label="var", habillage = "cylinders")

barplot(cars.pca$eig[,2], names.arg=rownames(cars.pca$eig))

plotellipses(cars.pca,7, label="none")

library(ggbiplot)
dimdesc(cars.pca)
ggbiplot(cars.pca, groups=cars.pc$cylinders, ellipse=TRUE) +
  theme(legend.direction = 'horizontal', legend.position = 'top')+
  scale_color_discrete(name = 'Cylinders') 

ggbiplot(cars.pca, groups=cars.pc$og, ellipse=TRUE, var.scale = 1) +
  theme(legend.direction = 'horizontal', legend.position = 'top')+
  scale_color_discrete(name = 'Origin') 

#it is clear that weight disp and hp goes toghether, if the rise they rise togheter and if thy rise usually mpg lowers
#big cars with need a lot of power and displacement but the are less fuel effiient
#the fact that the accelleration is better rappresented by second PCA means that accelleration is more independendent from
#this other variables, so I would say that there are less powerfull cars that are still performant
fviz_pca_var(cars.pca)
cars.pca$var$cor
cars_ds

#now let's consider origin in the picture, by the way american cars are all heavy and so in need of more hp and engine displ

cars.pc$origin <- factor(cars_ds$og)
#vast majority are americans cars
table(cars.pc$origin)

ggpairs(cars.pc, lower = list(continuous="points", combo="facetdensity",mapping =aes(color=origin)))


fviz_pca_ind(cars.pca,  label="none", habillage = c("origin"))

#for the clustering we need to do it by hand...
#il cluster lo devi fare sulle PC!
cars.clust = kmeans(cars.pca$ind$coord,3)
cars.clust
cars.pc$kcluster <- factor(cars.clust$cluster)
cars.pc

cars.pca = PCA(cars.pc, scale.unit = TRUE,quali.sup = c(6,7))
cars.pca
library(ggplot2)
cars_pca_obs = data.frame(C1=cars.pca$ind$coord[,1],C2=cars.pca$ind$coord[,2], origin = cars.pc$origin, cluster=cars.pc$kcluster)
cars_pca_obs

#tutte le macchine a destra sono americane
#tutti i produttori non americani sono a sinistra
#ci sono alcune macchine americane a sinistra, probabilmente cercavano di 
#copiare l'efficienza dei modelli europei
#should be better to biplot with the arrows
ggplot(cars_pca_obs, aes(x=C1,y=C2, label=rownames(cars_pca_obs)))+
  geom_hline(yintercept=0, color="gray70")+
  geom_vline(xintercept=0,color="gray70")+
  geom_point(aes(color=cluster, shape=origin), alpha=0.55, size=4)+
  geom_text(aes(color=cluster),alpha=0.55)

#it is a good idea to make biplots with the cluster

#PC plot with clustering partition
#cereal_pca_obs=data.frame(C1=cereal_pca_r$ind$coord[,1],C2=cereal_pca_r$ind$coord[,2], cluster=factor(cereal_clusters))
#ggplot(cereal_pca_obs, aes(x=C1,y=C2, label=rownames(cereal.pc)))+
#  geom_hline(yintercept=0, color="gray70")+
#  geom_vline(xintercept=0,color="gray70")+
#  geom_point(aes(color=cluster), alpha=0.55, size=2)+
#  geom_text(aes(color=cluster),alpha=0.55)
