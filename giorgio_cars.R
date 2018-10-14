library(dplyr)
cars_ds = read.csv("/home/giorgio/Uni/IntelligentDataAnalysys/Homerworks/1_4/me/cars.csv", row.names = 1, header=TRUE)
glimpse(cars_ds)
cars.pc = select(cars_ds,  mpg, disp, hp, drat, wt, qsec)
cars.names.abbrev.map = abbreviate(rownames(cars.pc))

#nicer when plotting
rownames(cars.pc) = abbreviate(rownames(cars.pc))
head(cars.pc)
cars.names.abbrev.map

#the values on the diagonal are really too different
cov(cars.pc)

cor(cars.pc)
library(GGally)
ggpairs(cars.pc)
#there are some very important linear correlations
eigen(cor(cars.pc))
#they seem to be between mpg, disp, wt
#let's go for the pca
library(FactoMineR)
cars.pca = PCA(cars.pc,graph = FALSE)
#let's see what we have
summary(cars.pca)
#seems that with the first 2 PC we get 88%, and 94% with the first 3. Not bad
#the highest contributions to the first PCA are due to mpg, disp, hp and wt
#the second component is monopolized by qsec and partially by qsec abd drat
