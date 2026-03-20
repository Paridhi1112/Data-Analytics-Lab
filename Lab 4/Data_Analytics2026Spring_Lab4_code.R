##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("C:/Users/parid/Documents/RPI/Spring 2026/Data Analytics/Lab/Lab 4")



## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type


#### Principal Component Analysis ####

## perform PCA on all variables except Type
wine.pca <- prcomp(wine[,-1], scale. = TRUE)

## summary of PCA
summary(wine.pca)

## view the principal components
wine.pca$rotation

## scree plot
screeplot(wine.pca, type="lines")

## plot using first 2 PCs
autoplot(wine.pca, data = wine, colour = 'Type', 
         loadings = TRUE, loadings.label = TRUE,
         loadings.label.size = 3) +
  labs(title = "Wine Dataset - PCA Plot (PC1 vs PC2)")


#### Identify variables contributing most to PC1 and PC2 ####

## PC1 loadings
cat("\n=== PC1 Loadings ===\n")
pc1.loadings <- sort(abs(wine.pca$rotation[,1]), decreasing = TRUE)
print(pc1.loadings)

cat("\nVariables contributing most to PC1:\n")
print(head(pc1.loadings, 3))

## PC2 loadings
cat("\n=== PC2 Loadings ===\n")
pc2.loadings <- sort(abs(wine.pca$rotation[,2]), decreasing = TRUE)
print(pc2.loadings)

cat("\nVariables contributing most to PC2:\n")
print(head(pc2.loadings, 3))


#### Split data into training and testing sets ####

set.seed(123)  # for reproducibility
sample.size <- floor(0.75 * nrow(wine))
train.indices <- sample(seq_len(nrow(wine)), size = sample.size)

## training and testing sets for original data
wine.train <- wine[train.indices, ]
wine.test <- wine[-train.indices, ]


#### Model 1: kNN with all original variables ####

## prepare data - remove Type column for predictors
train.data <- wine.train[,-1]
test.data <- wine.test[,-1]
train.labels <- wine.train$Type
test.labels <- wine.test$Type

## train kNN classifier with k=3
knn.pred.original <- knn(train = train.data, 
                         test = test.data, 
                         cl = train.labels, 
                         k = 3)

## contingency table
cat("\n=== Model 1: kNN with All Original Variables ===\n")
confusion.original <- table(Predicted = knn.pred.original, Actual = test.labels)
print(confusion.original)

## calculate accuracy
accuracy.original <- sum(diag(confusion.original)) / sum(confusion.original)
cat("\nAccuracy:", accuracy.original, "\n")

## calculate precision, recall, F1 for each class
calculate.metrics <- function(confusion.matrix) {
  n.classes <- nrow(confusion.matrix)
  precision <- numeric(n.classes)
  recall <- numeric(n.classes)
  f1 <- numeric(n.classes)
  
  for(i in 1:n.classes) {
    tp <- confusion.matrix[i,i]
    fp <- sum(confusion.matrix[i,]) - tp
    fn <- sum(confusion.matrix[,i]) - tp
    
    precision[i] <- tp / (tp + fp)
    recall[i] <- tp / (tp + fn)
    f1[i] <- 2 * (precision[i] * recall[i]) / (precision[i] + recall[i])
  }
  
  return(data.frame(
    Class = rownames(confusion.matrix),
    Precision = precision,
    Recall = recall,
    F1 = f1
  ))
}

metrics.original <- calculate.metrics(confusion.original)
print(metrics.original)


#### Model 2: kNN with first 2 PCs ####

## get PC scores for training and testing data
train.pca.scores <- predict(wine.pca, newdata = wine.train[,-1])[, 1:2]
test.pca.scores <- predict(wine.pca, newdata = wine.test[,-1])[, 1:2]

## train kNN classifier with k=3 on PC scores
knn.pred.pca <- knn(train = train.pca.scores, 
                    test = test.pca.scores, 
                    cl = train.labels, 
                    k = 3)

## contingency table
cat("\n=== Model 2: kNN with First 2 PCs ===\n")
confusion.pca <- table(Predicted = knn.pred.pca, Actual = test.labels)
print(confusion.pca)

## calculate accuracy
accuracy.pca <- sum(diag(confusion.pca)) / sum(confusion.pca)
cat("\nAccuracy:", accuracy.pca, "\n")

## calculate precision, recall, F1 for each class
metrics.pca <- calculate.metrics(confusion.pca)
print(metrics.pca)


#### Compare the two models ####

cat("\n=== Model Comparison ===\n")
cat("Model 1 (All Variables) Accuracy:", accuracy.original, "\n")
cat("Model 2 (First 2 PCs) Accuracy:", accuracy.pca, "\n")

cat("\n--- Model 1 Metrics (All Variables) ---\n")
print(metrics.original)

cat("\n--- Model 2 Metrics (First 2 PCs) ---\n")
print(metrics.pca)

## visualize classification results with PCA
pca.results <- data.frame(
  PC1 = test.pca.scores[,1],
  PC2 = test.pca.scores[,2],
  Actual = test.labels,
  Predicted = knn.pred.pca
)

ggplot(pca.results, aes(x = PC1, y = PC2, color = Actual, shape = Predicted)) +
  geom_point(size = 3) +
  labs(title = "kNN Classification Results on First 2 PCs",
       subtitle = "Color = Actual Class, Shape = Predicted Class") +
  theme_minimal()
###