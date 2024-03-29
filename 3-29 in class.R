library(tidyverse)





wine <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ',')
head(wine)
#wine data has no col names

colnames(wine) <- c("Cvs", "Alcohol",
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash",
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine",
                         "Proline")

#the above code adds labels to the columns in wine.
head(wine)
summary(wine)
#seen here
help(heatmap)
heatmap(cor(wine), Rowv = NA, Colv = NA)
#creates a heatmap w the 13 cols


#New way to make heatmap with different scaling properties
library(corrplot)
corr_matrix <- cor(wine[, -1]) # Assuming the first column is a factor or identifier
corrplot(corr_matrix, method = "color", diag = TRUE)
#

cultivar_classes <- factor(wine$Cvs)
cultivar_classes

wine_PSA <- prcomp(scale(wine[,-1]))
summary(wine_PSA)

#PC(1-3) -> ~66% of variance
#PC(1-7) -> ~90% of variance
#PC(1-11)-> ~98% of variance

nrow(wine)
#nrow = 178



