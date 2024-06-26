library(tidyverse)
library(e1071)

data('iris')
head(iris)
str(iris)

qplot(Petal.Length, Petal.Width, data = iris, color = Species)


svm_model1 <- svm(Species~., data = iris)
svm_model1
summary(svm_model1)


plot(svm_model1, data = iris,
     Petal.Width~Petal.Length, slice =
       list(Sepal.Width = 3, Sepal.Length = 4))

pred1 <- predict(svm_model1, iris)
pred1

table1 <- table(Predicted = pred1, Actual = iris$Species)
table1

Model1_accuracyRate = sum(diag(table1))/sum(table1)
Model1_accuracyRate

Model1_MissClassificationRate = 1 - Model1_accuracyRate
Model1_MissClassificationRate


svm_model2 <- svm(Species~., data = iris, kernel = "linear")
summary(svm_model2)

plot(svm_model2, data = iris,
     Petal.Width~Petal.Length, slice = list(Sepal.Width = 3,
                                            Sepal.Length = 4))

pred2 <- predict(svm_model2, iris)
pred2


table2 <- table(Predicted = pred2, Actual = iris$Species)
table2

Model2_accuracyRate = sum(diag(table2))/sum(table2)
Model2_accuracyRate

Model2_MissClassificationRate = 1 - Model2_accuracyRate
Model2_MissClassificationRate


svm_model3 <- svm(Species~., data = iris, kernel = "polynomial")
summary(svm_model3)

plot(svm_model3, data = iris,
     Petal.Width~Petal.Length, slice = list(Sepal.Width = 3,
                                            Sepal.Length = 4))

pred3 <- predict(svm_model3, iris)
pred3


table3 <- table(Predicted = pred3, Actual = iris$Species)
table3

Model3_accuracyRate = sum(diag(table3))/sum(table3)
Model3_accuracyRate

Model3_MissClassificationRate = 1 - Model3_accuracyRate
Model3_MissClassificationRate

#Accuracy: ~95.33%
#Miss Classification Rate: ~4.67%
