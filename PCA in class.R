#In class work on 3/26/24 on PCA


data("iris")
head(iris)

irisdata1 <- iris[,1:4]
head(irisdata1)

pc <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(pc)

plot(pc)
plot(pc, type = 'l')

biplot(pc)
dim(irisdata1)

#_______________________________________________________________________________

library(rpart)
data(Titanic)


titanic_df <- as.data.frame(Titanic)


model <- rpart(Survived ~ ., data = titanic_df, method = "class")


print(model)


plot(model)
text(model)

#_______________________________________________________________________________

install.packages("MASS")
library(MASS)

data(Boston, package = "MASS")
head(Boston)

pca_out <- prcomp(Boston,scale. = T)
pca_out
plot(pca_out)

biplot(pca_out, scale = 0)

boston_pc <- pca_out$x
boston_pc
head(boston_pc)
summary(boston_pc)

#_______________________________________________________________________________

data("USArrests")
states = row.names(USArrests)
states

apply(USArrests , 2, mean)
apply(USArrests , 2, var)

pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale = 0)
pr.out$sdev

pr.var = pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)
pve

#_______________________________________________________________________________





