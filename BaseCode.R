##Libraries
library(ggplot2)
library(lubridate)
library(caret)
library(dplyr)
library(ggpubr)
###Downloading Data
if(!file.exists("./data")){dir.create("./data")}
GeneralFiles <- list.files("./data", full.names = TRUE)
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileUrl2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(!file.exists("./data/training.csv")){
        download.file(fileUrl1, destfile = "./data/training.csv", method = "curl")
}
if(!file.exists("./data/testing.csv")){
        download.file(fileUrl2, destfile = "./data/testing.csv", method = "curl")
}
##Reading Data
training <- read.csv("./data/training.csv", header = TRUE)
testing <- read.csv("./data/testing.csv", header = TRUE)
#Pre-Processing
NAbyCol <- apply(training, 2, function(x) sum(is.na(x))) 
NACol <- names(subset(NAbyCol, NAbyCol == 19216))
training <- select(training, !NACol) ##Remove Columns with NAs
training <- select(training, !names(training)[1:7]) ##Remove Columns with out usefull info
vars2 <- grepl("kurtosis|skewness|amplitude|min|max", names(training))
training <- select(training, c(names(training[,!vars2]), classe)) ##Removes the clomuns with character classes (Its almost an empty column)
##CV
set.seed(2549)
inTrain <- createDataPartition(y=training$classe,
                               p = 0.75, list = FALSE)
training <- training[inTrain,]
TEST <- training[-inTrain,]
dim(training)
dim(TEST)
##Exploration Analysis
plot1 <- ggplot(training, aes(total_accel_arm, total_accel_dumbbell)) + 
        geom_point(aes(color = classe) , alpha = 0.3) + 
        scale_color_manual("Classe", 
                           values = c("A" = "red", 
                                      "B" = "blue",
                                      "C" = "yellow",
                                      "D" = "green",
                                      "E" = "purple"))

plot3 <- ggplot(training, aes(total_accel_arm, total_accel_forearm)) + 
        geom_point(aes(color = classe) , alpha = 0.3) + 
        scale_color_manual("Classe", 
                           values = c("A" = "red", 
                                      "B" = "blue",
                                      "C" = "yellow",
                                      "D" = "green",
                                      "E" = "purple"))

plot2 <- ggplot(training, aes(total_accel_forearm, total_accel_dumbbell)) + 
        geom_point(aes(color = classe) , alpha = 0.3) + 
        scale_color_manual("Classe", 
                           values = c("A" = "red", 
                                      "B" = "blue",
                                      "C" = "yellow",
                                      "D" = "green",
                                      "E" = "purple"))
plot4 <- ggplot(training, aes(magnet_arm_x, magnet_arm_y)) + 
        geom_point(aes(color = classe) , alpha = 0.3) + 
        scale_color_manual("Classe", 
                           values = c("A" = "red", 
                                      "B" = "blue",
                                      "C" = "yellow",
                                      "D" = "green",
                                      "E" = "purple"))

figure <- ggarrange(plot1, plot2, plot3, plot4, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
annotate_figure(figure,
                top = text_grob("Total Accelerations Interactions", color = "black", face = "bold", size = 14))
##ML
IteControl <- trainControl(method='cv', number = 2) #Control the iteration of the train function to speed up this function 
fit1 <- train(classe ~ ., method = "rpart", data = training)
fit1_1 <- train(classe ~ ., method = "rpart", preProcess = "knnImpute",data = training)
fit2 <- train(classe ~ ., method = "rf", trControl=IteControl, data = training, verbose=FALSE)
fit3 <- train(classe ~ ., method = "treebag", trControl=IteControl, data = training, verbose=FALSE)
##Model Testing
confusionMatrix(as.factor(TEST$classe),predict(fit1, TEST))
confusionMatrix(as.factor(TEST$classe),predict(fit1_1, TEST))
confusionMatrix(as.factor(TEST$classe),predict(fit2, TEST))
confusionMatrix(as.factor(TEST$classe),predict(fit3, TEST))
##Predictions
pred1 <- predict(fit1, testing)
pred1_1 <- predict(fit1_1, testing)
pred2 <- predict(fit2, testing)
pred3 <- predict(fit3, testing)
data.frame(Trees = pred1, Trees_kmeans = pred1_1, Random_Forest = pred2, Boosting_Trees = pred3)
pred2 ## Most accurate answer 