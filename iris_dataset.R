# install.packages("caret")
# install.packages("ellipse")
# install.packages('e1071', dependencies=TRUE)
# install.packages("kernlab")

library("kernlab")
library("caret")
library("ellipse")
library("e1071")

setwd("~/Desktop/info201/project")

file_name <- "iris.data.csv"
data_iris <- read.csv(file_name, sep = ",", stringsAsFactors = T, header = F)
colnames(data_iris) <- c("Sepal.Length","Sepal.Width","Petal.Length"
                         ,"Petal.Width","Species")

val_ind <- createDataPartition(data_iris$Species, p = 0.60, list = FALSE)
train <- data_iris[val_ind,] 
test <- data_iris[-val_ind,]

dim(data_iris)
sapply(data_iris, class)
data_iris$Species <- as.factor(data_iris$Species)
levels(data_iris$Species)

perc <- prop.table(table(data_iris$Species)) * 100
cbind(freq = table(data_iris$Species), perc)

# Now to analyse the graph we will start with make some vizualization to 
# get a better feel of the data we are using
summary(data_iris)

# First we will analyse some univariate plots

# Spliting our input and output
x <- data_iris[,1:4]
y <- data_iris[,5]

# boxplot 
par(mar = rep(2, 4))
boxplot(x[,1], main=names(iris)[1])
boxplot(x[,2], main=names(iris)[2])
boxplot(x[,3], main=names(iris)[3])
boxplot(x[,4], main=names(iris)[4])
plot(y)

# The univariate plot did not turn out that informative
# since the all were pretty even.(Can't say much about long term trends too)
# So now we will see some
# Multivariate plots to hopefully find something useful
featurePlot(x, y, plot="box")
featurePlot(x, y, plot="ellipse")
scale <- list(list(relation="free"),list(relation="free"))
featurePlot(x = x, y = y, plot = "density", scales = scale)

# By running the above code we see the different in distribution
# amougst each of the given variables
# In the last example particularly we see the Gaussian Distribution
# of each of the attributes. (The bell-curve)

# Now that we have a decent understanding of the data we would
# now start using some basic Machine Learning algorithms to predict 
# the species of the flower based on the other 4 variables

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

# Linear Discriminant Analysis (A simple linear)
set.seed(7)
LDA <- train(Species ~ ., data = data_iris, method = "lda",
             metric = metric, trControl = control)

LDA_results <- LDA$results

# Classification and Regression Trees (non-linear model)
rpart <- train(Species ~ ., data = data_iris, method = "rpart",
             metric = metric, trControl = control)

rpart_results <- rpart$results

# k-Nearest Neighbors  (non-linear model)
knn <- train(Species ~ ., data = data_iris, method = "knn",
               metric = metric, trControl = control)
knn_results <- knn$results

# Support Vector Machines with a linear kernel (complex non-linear)
svm <- train(Species ~ ., data = data_iris, method = "svmRadial",
               metric = metric, trControl = control)
svm_results <- svm$results

# Random Forest (complex non-linear)
rf <- train(Species ~ ., data = data_iris, method = "rf",
               metric = metric, trControl = control)
rf_results <- rf$results

# find the best solution
results <- resamples(list(rf, LDA, svm, rpart, knn))
summary(results)
dotplot(results)

# Finally now we can use our validation dataset to test whether
# this model (LDA) actually works or if we have overfitted the data
my_predictions <- function(test_type) {
  my_pred <- predict(test_type, test)
  con_mat <- confusionMatrix(my_pred, test$Species)
}

# Checking to find the most accurate model
Accuracy_LDA <- my_predictions(LDA)
Accuracy_rpart <- my_predictions(rpart)
Accuracy_knn <- my_predictions(knn)
Accuracy_rf <- my_predictions(rf)
Accuracy_svm <- my_predictions(svm)
