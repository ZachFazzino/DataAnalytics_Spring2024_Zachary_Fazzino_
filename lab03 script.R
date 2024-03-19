set.seed(12345)
par(mar = rep(0.2, 4))
data_matrix = matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_matrix)[,nrow(data_matrix):1])

heatmap(data_matrix)


set.seed(678910)

for(i in 1:40){
  coin_flip = rbinom(1, size = 1, prob = 0.5)
  if(coin_flip){
    data_matrix[i,] = data_matrix[i,]+rep(c(0, 3), each = 5)
  }
}

par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_matrix)[, nrow(data_matrix):1])
par(mar=rep(0.2, 4))
heatmap(data_matrix)

hh <- hclust(dist(data_matrix))
data_matrix_Ordered <- data_matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_matrix_Ordered)[, nrow(data_matrix_Ordered):1])
plot(rowMeans(data_matrix_Ordered), 40:1, xlab = "The Row Mean", ylab = "Row", pch = 19)
plot(colMeans(data_matrix_Ordered), xlab = "Column", ylab = "Column Mean", pch = 19)


#Exercise 2: Classification
library(class)
abalone = read.csv('abalone.csv')

abalone$Rings <- as.numeric(abalone$Rings)
abalone$Rings <- cut(abalone$Rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$Rings <- as.factor(abalone$Rings)
summary(abalone$Rings)
#Young => 1407
#Adult => 1810
#Old => 960

str(abalone)
abalone$Sex <- NULL
str(abalone)



normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
abalone[1:7] <- as.data.frame(lapply(abalone[1:7], normalize))
summary(abalone$shucked_wieght)

aba = abalone

ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)

KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$Rings, k = 55)
KNNpred
table(KNNpred)
#Young => 388
#Adult => 725
#Old => 133




#Exercise 3: Clustering
library(e1071)
data("iris")
summary(iris)

iris_clean = iris[,-5]

set.seed(2345)
kmeans <- kmeans(iris_clean, centers = 3, iter.max = 1000, nstart = 20)

comparison <- table(iris[, 5], kmeans$cluster)
print(comparison)
# 1 2 3
# 0 0 50
# 2 48 0
# 36 14 0

#Code Snippets
#_______________________________________________________________________________
library(gdata) 


library("xlsx")
bronx1 <- read.csv('rollingsales_bronx (3).csv')

attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
bronx1$SALE.PRICE<-sub("\\$","",SALE.PRICE) 
bronx1$SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
bronx1$GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
bronx1$LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
na.omit(SALE.PRICE)
na.omit(GROSS.SQUARE.FEET)


bronx1 <- bronx1[which(bronx1$GROSS.SQUARE.FEET > 0 & bronx1$LAND.SQUARE.FEET > 0 & bronx1$SALE.PRICE > 0), ]



m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))


# Model 2

m2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))

bronx1$SALE.PRICE<-sub("\\$","",bronx1$SALE.PRICE) 
bronx1$SALE.PRICE<-as.numeric(gsub(",","", bronx1$SALE.PRICE)) 
bronx1$GROSS.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$GROSS.SQUARE.FEET)) 
bronx1$LAND.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$LAND.SQUARE.FEET)) 
bronx1$SALE.DATE<- as.Date(gsub("[^]:digit:]]","",bronx1$SALE.DATE)) 
bronx1$YEAR.BUILT<- as.numeric(gsub("[^]:digit:]]","",bronx1$YEAR.BUILT)) 
bronx1$ZIP.CODE<- as.character(gsub("[^]:digit:]]","",bronx1$ZIP.CODE)) 

minprice<-10000
bronx1<-bronx1[which(bronx1$SALE.PRICE>=minprice),]
nval<-dim(bronx1)[1]

bronx1$ADDRESSONLY <- gsub(",[[:print:]]*", "", bronx1$ADDRESS)
bronxadd <- unique(data.frame(ADDRESSONLY = bronx1$ADDRESSONLY, ZIP.CODE = bronx1$ZIP.CODE, stringsAsFactors = FALSE))
bronxadd <- bronxadd[order(bronxadd$ADDRESSONLY),]
duplicates <- duplicated(bronx1$ADDRESSONLY)

for(i in 1:2345) {
  if(duplicates[i]==FALSE) dupadd<-bronxadd[bronxadd$duplicates,1]
}#what are we doing with dupadd?

nsample=450

addsample<-bronxadd[sample.int(dim(bronxadd),size=nsample),]#I use nval here 
# may need to install this package
install.packages('ggmap')
library(ggmap)

addrlist<-paste(addsample$ADDRESSONLY, "NY", addsample$ZIP.CODE, "US", sep=" ") 
querylist<-geocode(addrlist) #This is cool. Take a break.

matched<-(querylist$lat!=0 &&querylist$lon!=0) addsample<-cbind(addsample,querylist$lat,querylist$lon) 
names(addsample)<-c("ADDRESSONLY","ZIPCODE","Latitude","Longitude")# correct the column na adduse<-merge(bronx1,addsample)

adduse<-adduse[!is.na(adduse$Latitude),]
mapcoord<-adduse[,c(2,3,24,25)]

table(mapcoord$NEIGHBORHOOD)

mapcoord$NEIGHBORHOOD <- as.factor(mapcoord$NEIGHBORHOOD)
map <- get_map(location = 'Bronx', zoom = 12)#Zoom 11 or 12
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapcoord$NEIGHBORHOOD), data = mapcoord) +theme(legend.position = "none") 

#It would be perfect if I can decrease the size of points 

mapmeans<-cbind(adduse,as.numeric(mapcoord$NEIGHBORHOOD))
colnames(mapmeans)[26] <- "NEIGHBORHOOD" #This is the right way of renaming.

keeps <- c("ZIP.CODE","NEIGHBORHOOD","TOTAL.UNITS","LAND.SQUARE.FEET","GROSS.SQUARE.FEET","SALE.PRICE","Latitude","Longitude") 
mapmeans<-mapmeans[keeps]#Dropping others
mapmeans$NEIGHBORHOOD<-as.numeric(mapcoord$NEIGHBORHOOD) 

for(i in 1:8){
  mapmeans[,i]=as.numeric(mapmeans[,i]) 
}#Now done for conversion to numeric

#Classification
mapcoord$class<as.numeric(mapcoord$NEIGHBORHOOD)
nclass<-dim(mapcoord)[1]
split<-0.8
trainid<-sample.int(nclass,floor(split*nclass))
testid<-(1:nclass)[-trainid]

##mappred<-mapcoord[testid,] # What would you use this for?
##mappred$class<as.numeric(mappred$NEIGHBORHOOD) 

kmax<-10
knnpred<-matrix(NA,ncol=kmax,nrow=length(testid))
knntesterr<-rep(NA,times=kmax)
for (i in 1:kmax){		# loop over k
  knnpred[,i]<-knn(mapcoord[trainid,3:4],mapcoord[testid,3:4],cl=mapcoord[trainid,2],k=i)
  knntesterr[i]<-sum(knnpred[,i]!=mapcoord[testid,2])/length(testid)
} 
knntesterr

#Clustering
mapobj<-kmeans(mapmeans,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobj,method=c("centers","classes"))
mapobj$centers
#
library(cluster)
clusplot(mapmeans, mapobj$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
#
library(fpc)#May need to install.packages("fpc")
plotcluster(mapmeans, mapobj$cluster)
#
mapmeans1<-mapmeans[,-c(1,3,4)]
mapobjnew<-kmeans(mapmeans1,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobjnew,method=c("centers","classes"))
clusplot(mapmeans1, mapobjnew$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
plotcluster(mapmeans1, mapobjnew$cluster)
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapobjnew$cluster), data = mapcoord)#How to change colors?



#continues the imported code(lab01_bronx1...etc) in lab04.