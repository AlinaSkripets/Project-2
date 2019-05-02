getwd()
setwd("/Users/Alina/Desktop/project2/image_data")

library(ggplot2)
library(pastecs)
library(skimr)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(MASS)
library(plotROC)
library(caret)
library(rpart)
library(LiblineaR)
library(tree)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(gbm)
library(MLmetrics)
library(boot)
library(readr)
library(cvTools) #run the above line if you don't have this library
library(ROCR)
library(InformationValue)



head <- c("y", "x", "Label", "NDAI", "SD", "CORR","DF", "CF", "BF", "AF", "AN")
data1 <- read.delim("image1.txt", sep = "", col.names = head)
data2 <- read.delim("image2.txt", sep = "", col.names = head)
data3 <- read.delim("image3.txt", sep = "", col.names = head)

##################################################################################
##################################################################################
#SUMMARY
##################################################################################
##################################################################################

sum <- stat.desc(data1)
sum <- t(sum)[,c(1,3,4,5,6,8,9,10,11,12)]
sum <- sum[ ,c(3,6,7,4,10)]
sum
#sink("Summary", append=FALSE, split=FALSE)
skim(data3, c(Label, NDAI, SD, CORR, DF, CF, BF, AF, AN))
#sink()
t1 <- table(data1$Label)/length(data1$Label)
t2 <- table(data2$Label)/length(data2$Label)
t3 <- table(data3$Label)/length(data3$Label)
rbind(t1,t2,t3)

sum <- stat.desc(data1)
sum <- t(sum)[,c(1,3,4,5,6,8,9,10,11,12)]
sum <- sum[ ,c(3,6,7,4,10)]
sum
#sink("Summary", append=FALSE, split=FALSE)
skim(data3, c(Label, NDAI, SD, CORR, DF, CF, BF, AF, AN))
#sink()
t1 <- table(data1$Label)/length(data1$Label)
t2 <- table(data2$Label)/length(data2$Label)
t3 <- table(data3$Label)/length(data3$Label)
rbind(t1,t2,t3)

##################################################################################
##################################################################################
#FEATURE CREATION
##################################################################################
##################################################################################


data1$FBDF <- c()
for (i in 1:length(data1$y)){
  data1$FBDF[i] = abs(data1$AF[i]-data1$AN[i])/(data1$AF[i])
}

data2$FBDF <- c()
for (i in 1:length(data2$y)){
  data2$FBDF[i] = abs(data2$AF[i]-data2$AN[i])/(data2$AF[i])
}

data3$FBDF <- c()
for (i in 1:length(data3$y)){
  data3$FBDF[i] = abs(data3$AF[i]-data3$AN[i])/(data3$AF[i])
}


data1 <- data1[, c(1:5, 12)]
data2 <- data2[, c(1:5, 12)]
data3 <- data3[, c(1:5, 12)]

data1$image <- 1
data2$image <- 2
data3$image <- 3

data1$Label <- as.factor(as.character(data1$Label))
levels(data1$Label) <- c("0","NA", "1")
data1 <- data1[which(data1$Label!="NA"), ]
data1$Label <- as.factor(as.numeric(as.character(data1$Label)))

data2$Label <- as.factor(as.character(data2$Label))
levels(data2$Label) <- c("0", "NA", "1")
data2 <- data2[which(data2$Label!="NA"), ]
data2$Label <- as.factor(as.numeric(as.character(data2$Label)))

data3$Label <- as.factor(as.character(data3$Label))
levels(data3$Label) <- c("0",  "NA", "1")
data3 <- data3[which(data3$Label!="NA"), ]
data3$Label <- as.factor(as.numeric(as.character(data3$Label)))
sum(is.na(data3$Label))

data1$SD <- log(data1$SD)
data2$SD <- log(data2$SD)
data3$SD <- log(data3$SD)

##################################################################################
##################################################################################
#EDA
##################################################################################
##################################################################################
# data1$Label <- as.numeric(data1$Label)
# M <-cor(data1)
# corrplot(M, type="upper", order="hclust",
#          col=brewer.pal(n=8, name="RdYlBu"))
library("PerformanceAnalytics")
#my_data <- data1[, c(4,5,6)]
#chart.Correlation(my_data, histogram=TRUE, pch=19)
# ggplot(data1, aes(x,y, col = as.factor(Label))) +
#   geom_point() +
#   scale_color_manual(values=c("lightgreen", "grey", "lightblue"),
#                      labels= c("Cloud free","Unsure","Clouds")) +
#   labs(col="Presence of clouds") +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_blank(),
#         legend.position="bottom") +
#   ylab("")+
#   xlab("")

# kmn <- kmeans(data1[ , c(4:6)], 3, nstart = 20)
# kmn$centers

# ggplot(data1, aes(x,y, col = factor(kmn$cluster))) +
#   geom_point() +
#   scale_color_manual(values=c("lightblue", "grey","lightgreen"),
#                      labels= c("Cloud free","Unsure","Clouds")) +
#   labs(col="Presence of clouds") +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_blank(),
#         legend.position="bottom") +
#   ylab("")+
#   xlab("")+
#   ggtitle("K-mean analysis (3 covariates)")

# kmn <- kmeans(data1[ , c(4)], 3, nstart = 20)
# kmn$centers

# ggplot(data1, aes(x,y, col = factor(kmn$cluster))) +
#   geom_point() +
#   scale_color_manual(values=c("lightblue", "grey","lightgreen"),
#                      labels= c("Cloud free","Unsure","Clouds")) +
#   labs(col="Presence of clouds") +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_blank(),
#         legend.position="bottom") +
#   ylab("")+
#   xlab("")+
#   ggtitle("K-mean analysis (3 covariates)")
# 
# kmn <- kmeans(data1[ , c(5)], 3, nstart = 20)
# kmn$centers

# ggplot(data1, aes(x,y, col = factor(kmn$cluster))) +
#   geom_point() +
#   scale_color_manual(values=c("lightblue", "grey","lightgreen"),
#                      labels= c("Cloud free","Unsure","Clouds")) +
#   labs(col="Presence of clouds") +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_blank(),
#         legend.position="bottom") +
#   ylab("")+
#   xlab("")+
#   ggtitle("K-mean analysis (3 covariates)")


# kmn <- kmeans(data1[ , c(6)], 3, nstart = 20)
# kmn$centers

# ggplot(data1, aes(x,y, col = factor(kmn$cluster))) +
#   geom_point() +
#   scale_color_manual(values=c("lightblue", "grey","lightgreen"),
#                      labels= c("Cloud free","Unsure","Clouds")) +
#   labs(col="Presence of clouds") +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_blank(),
#         legend.position="bottom") +
#   ylab("")+
#   xlab("")+
#   ggtitle("K-mean analysis (3 covariates)")



# kmn <- kmeans(data1[ , c(7:11)], 2, nstart = 20)
# kmn$centers

# ggplot(data1, aes(x,y, col = factor(kmn$cluster))) +
#   geom_point() +
#   scale_color_manual(values=c("lightblue", "grey","lightgreen"),
#                      labels= c("Cloud free","Unsure","Clouds")) +
#   labs(col="Presence of clouds") +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_blank(),
#         legend.position="bottom") +
#   ylab("")+
#   xlab("")+
#   ggtitle("K-mean analysis (3 covariates)")

##################################################################################
##################################################################################
#TRAINING VS VAL VS TEST 1
##################################################################################
##################################################################################
data_all <- rbind(data1, data2, data3)
write.csv(data_all, "data_all.csv", row.names = FALSE)
data_all <- read.table("data_all.csv", header=TRUE, sep=",")
data_all$Label <- as.factor(as.character(data_all$Label))
data_all$squar1 <- c(0)
k=0
stepsize = 40
i = stepsize
while (i<=max(data_all$x)+stepsize){
  data_all$squar1[ which( data_all$x>=(i - stepsize) & (data_all$x<i) ) ] <- k
  k=k+1
  i = i + stepsize
}

data_all$squar2 <- c(0)
k=0.1
stepsize = 40
i = stepsize
while (i<=max(data_all$y)+stepsize){
  data_all$squar2[ which(data_all$y>=(i - stepsize) & (data_all$y<i) ) ] <- k
  k=k+0.1
  i = i + stepsize
}
data_all$sort1 <- data_all$squar1+data_all$squar2 

data_all$sort1 <- as.factor(data_all$sort1)
sample <- sample(levels(data_all$sort1))
test <- sample[1:ceiling(0.1*length(sample))]
train <- sample[ceiling(0.1*length(sample))+1:ceiling(0.7*length(sample))]
val <- sample[(ceiling(length(sample)-(0.2*length(sample)))+1):length(sample)]

test_data_1 <- data_all %>%
  filter(data_all$sort1%in%test)
train_data_1 <- data_all %>% 
  filter(data_all$sort1%in%train)
val_data_1 <- data_all %>% 
  filter(data_all$sort1%in%val)


# data_all$status1 <- c(0)
# for (i in 1:length(data_all$x)){
#   if (data_all$sort1[i] %in% test) data_all$status1[i] <- 1
#   if (data_all$sort1[i] %in% val) data_all$status1[i] <- 2
# }


# ggplot(data1, aes(x,y, col = factor(status1))) +
#   geom_point() +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_blank(),
#         legend.position="bottom") +
#   ylab("")+
#   xlab("")

##################################################################################
##################################################################################
#WAY 2
##################################################################################
##################################################################################

vec <- rep(c(0,0,0,1,2,0,0,0,0,1,2), length = length(levels(data_all$sort1)))
test2 <- levels(data_all$sort1)[which(vec == 1)]
train2 <- levels(data_all$sort1)[which(vec == 0)]
val2 <- levels(data_all$sort1)[which(vec == 2)]

test_data_2 <- data_all %>%
  filter(data_all$sort1%in%test2)
train_data_2 <- data_all %>% 
  filter(data_all$sort1%in%train2)
val_data_2 <- data_all %>% 
  filter(data_all$sort1%in%val2)

# data_all$status2 <- c(0)
# for (i in 1:length(data1$x)){
#   if (data1$sort1[i] %in% test2) data1$status2[i] <- 1
#   if (data1$sort1[i] %in% val2) data1$status2[i] <- 2
# }

##################################################################################
##################################################################################
#WAY 3
##################################################################################
##################################################################################

levels(data1$sort1)
test3 <- data1[which(data1$x<=200&data1$y<200), ]
train3_1 <- data1[which(data1$x>200), ]
train3_2 <- data1[which(data1$y>=200), ]
train3_3 <- data1[which((data1$x<200&data1$y>=200)|(data1$x>200&data1$y<200)), ]

val3_1 <- data1[which(data1$x<=200&data1$y>=200), ]
val3_2 <- data1[which(data1$x>200&data1$y<200), ]
val3_3 <- data1[which(data1$x>=200&data1$y>=200), ]



data1$status3_1 <- c(0)
data1$status3_2 <- c(0)
data1$status3_3 <- c(0)

data1$status3_1[which(data1$x<=200&data1$y<200)] <- 1
data1$status3_2[which(data1$x<=200&data1$y<200)] <- 1
data1$status3_3[which(data1$x<=200&data1$y<200)] <- 1

data1$status3_1[which(data1$x<=200&data1$y>=200)] <- 2
data1$status3_2[which(data1$x>200&data1$y<200)] <- 2
data1$status3_3[which(data1$x>=200&data1$y>=200)] <- 2

##################################################################################
##################################################################################
#TRIVIAL
##################################################################################
##################################################################################
# ggplot(data_with_values, aes(x,y, col = as.factor(Label))) +
#   geom_point() +
#   scale_color_manual(values=c("lightgreen", "grey", "lightblue"),
#                      labels= c("Cloud free","Unsure","Clouds")) +
#   labs(col="Presence of clouds") +
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(), axis.line = element_blank(),
#       legend.position="bottom") +
#   ylab("")+
#   xlab("")+
#   ggtitle("Image №1")

x <- train_data_1$Label
x <- as.numeric(as.character(x))
predx <- rep(0, length(x))
roc_trivial <- data.frame(x, predx)
ggplot(roc_trivial, aes(d = x, m = predx)) + 
  geom_roc()
plotROC(x, predx, Show.labels = 1)
freq <- (length(predx)-sum(abs(predx-x)))/length(predx)

#the fraction of instances that are correctly classified
freq
#[1] 0.6248471
##################################################################################
##################################################################################
#FEATURE SELECTION
##################################################################################
##################################################################################


par(mfrow=c(1,3))

cdplot(factor(Label)~FBDF, data = data_all[which(data_all$FBDF<0.3), ], ylab = " ")
cdplot(factor(Label)~NDAI, data = data_all)
cdplot(factor(Label)~log(SD), data = data_all[which(data_all$FBDF<0.3), ], xlab = "Log of SD")

par(mfrow=c(1,1))


par(mfrow=c(1,3))
hist(log(data_all$SD), main = "Log of SD", xlab = " ")
hist((data1$NDAI), main = "NDAI index", xlab = " ")
hist(data1[which(data1$FBDF<0.25), ]$FBDF, main = "Front-Back Difference Factor (FBDF)", xlab = " ")
#0.006621654 is over 0.25
par(mfrow=c(1,1))

##################################################################################
##################################################################################
#CV GENERIC
##################################################################################
##################################################################################
# Write a generic cross validation (CV) function CVgeneric in R that takes a generic
# classiffier, training features, training labels, number of folds K and a loss function (at 
#least classication accuracy should be there) as inputs and outputs the K-fold CV loss
# on the training set. 

#' Generic CV function
#' @param classifier Type of classifier: glm, lda, qda, tree, rf, boost
#' @param features An array of feature values
#' @param labels A numeric vector of true labels of the training set
#' @param K A number.
#' @param loss Type of loss function: L1, L2, 
#' Accuracy AIC LogLoss ZeroOne AUC Precision Sensitivity Specificity
#' @return K-fold CV on the training dataset

CVgeneric <- function(train_data_1 = train_data_1, classifier = "glm", features, labels, k=10, loss = "Accuracy"){
  #the K-fold CV loss on the training set

      
      x <-train_data_1[features]
      data <- data.frame(NDAI = x[ ,1], SD = x[ ,2], FBDF = x[ ,3], Label = labels)
      Accuracy = c(0)
      AIC = c(0)
      LogLoss = c(0)
      ZeroOne = c(0)
      AUC = c(0)
      Precision = c(0)
      Sensitivity = c(0)
      Specificity = c(0)
      MAE = c(0)
      MSE = c(0)
      folds <- cvFolds(NROW(data), K=k)
      

      for(i in 1:k){
        train <- data[folds$subsets[folds$which != i], ] #Set the training set
        val <- data[folds$subsets[folds$which == i], ]  #Set the validation set

        
############################################################################################
#GLM
############################################################################################
        if (classifier == "glm"){
        train$Label <- as.numeric(as.character(train$Label))
        newglm <- glm(as.numeric(as.character(Label))~NDAI+SD+FBDF,data=train) #Get new model 
        newpred <- predict(newglm, val) #Get the predicitons for the validation set
        newpred <- ifelse(newpred > 0.5, 1, 0)
        }
############################################################################################
#LDA
############################################################################################
        if (classifier == "lda"){
        newlda <- lda(Label ~ NDAI + SD + FBDF, data=train)
        newpred <- predict(newlda, val)$class} #Get the predicitons for the validation set
############################################################################################
#QDA
############################################################################################ 
        if (classifier == "qda"){
        newqda <- qda(Label ~ NDAI + SD + FBDF, data=train)
        newpred <- predict(newqda, val)$class} #Get the predicitons for the validation set
############################################################################################
#tree
############################################################################################ 
        if (classifier == "tree"){
        newtree <- tree(Label~NDAI + SD + FBDF, data=train)
        newpred = predict(newtree, val, type="class")}
############################################################################################
#forest
############################################################################################ 
        if (classifier == "rf"){
        newforest = randomForest(Label~NDAI + SD + FBDF, data = train, maxnodes = 10)
        newpred <- predict(newforest, val)}
############################################################################################
#boost
############################################################################################ 
        if (classifier == "boost"){
        train$Label <- as.numeric(as.character(train$Label))
        newboost = gbm(as.character(as.numeric(Label))~NDAI + SD + FBDF, data = train, shrinkage = 0.01, interaction.depth = 4, n.trees = 100)
        newpred = predict(newboost, newdata = val, n.trees = 100)
        newpred <- ifelse(newpred > 0, 1, 0)
        }
############################################################################################
#output
############################################################################################ 
        
        if (loss == "Accuracy") {Accuracy[i] <- Accuracy(newpred, val$Label)}
        if (loss == "AIC") {AIC[i] = newglm$aic}
        if (loss == "LogLoss") {LogLoss[i] = LogLoss(newpred, as.numeric(as.character(val$Label)))}
        if (loss == "ZeroOne") {ZeroOne[i] = ZeroOneLoss(newpred, val$Label)}
        if (loss == "AUC") {AUC[i] = Area_Under_Curve(newpred, val$Label)}
        if (loss == "Precision") {Precision[i] = Precision(newpred, val$Label)}
        if (loss == "Sensitivity") {Sensitivity[i] = Sensitivity(newpred, val$Label)}
        if (loss == "Specificity") {Specificity[i] = Specificity(newpred, val$Label)}
        if (loss == "L1"){MAE[i] = MLmetrics::MAE(newpred, val$Label)}
        if (loss == "L2"){MSE[i] = MLmetrics::MSE(newpred, val$Label)}
        
}
        
        if (loss == "Accuracy") {return(Accuracy)}
        if (loss == "AIC") {return(AIC)}
        if (loss == "LogLoss") {return(LogLoss)}
        if (loss == "ZeroOne") {return(ZeroOne)}
        if (loss == "AUC") {return(AUC)}
        if (loss == "Precision") {return(Precision)}
        if (loss == "Sensitivity") {return(Sensitivity)}
        if (loss == "Specificity") {return(Specificity)}
        if (loss == "L1") {return(MAE)}
        if (loss == "L2") {return(MSE)}
}



##################################################################################
##################################################################################
#MODELS
##################################################################################
##################################################################################
#UNSUCCESSFUL
#fit.rf <- train(Label~ NDAI + SD + FBDF, data=train_data_1, method="rf")
#ctree.fit <- system.time(train(Label ~ NDAI + SD + FBDF, data=train_data_1, method="ctree", metric=metric, trControl=control))
#fit.svm <- train(Label~NDAI + SD + FBDF, data=train_data_1, method="svmLinear", metric=metric, trControl=control)
# tree.fit <- rpart(Label ~ NDAI + SD + FBDF, data = train_data_1, method = "class", minsplit = 2, 
#                   minbucket = 1, cp = -1)
# tree.fit <- prune(tree.fit, cp = 0.0002)
# fancyRpartPlot(fit.tree)
# plot mytree
# fancyRpartPlot(tree.fit, caption = NULL)
# tree.fit$variable.importance
# printcp(tree.fit
# svm.fit <- LiblineaR(train_data_1[,4:6], train_data_1[,3], type = 2, cost = 1, 
#           cross = 0, verbose = F)
# predict.LiblineaR(svm.fit, val_data_1)

#METHOD 1 GLM
data_cv <- rbind(train_data_1, val_data_1)
glm.fit <- glm(Label ~ NDAI + SD + FBDF, data = data_cv, family = binomial)
summary(glm.fit)
y_pred <- as.numeric(predict(glm.fit, val_data_1))
y_true <- as.numeric(val_data_1$Label)
LogLoss(y_pred, y_true)

#METHOD 2 LDA
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
lda.fit <- train(Label ~ NDAI + SD + FBDF, data=train_data_1, method="lda")

#METHOD 3 QDA
qda.fit <- train(Label ~ NDAI + SD + FBDF, data=train_data_1, method="qda")
results <- resamples(list(lda=lda.fit, qda=qda.fit, svm = fit.svm))
summary(results)

#METHOD 4 TREE
fit.tree <- tree(Label~NDAI + SD + FBDF, data=train_data_1)
tree.pred = predict(fit.tree, val_data_1, type="class")
with(val_data_1, table(tree.pred, Label))
cv.tree = cv.tree(fit.tree)
plot(cv.tree)

(13971+16623)/(13971+4998+1471+16623)
(503+8042)/(503+8042+27928+11297)
plot(fit.tree)
text(fit.tree)

#METHOD 5 FOREST
rf.fit = randomForest(Label~NDAI + SD + FBDF, data = train_data_1)
plot(rf.fit)
rf.fit
pred_forest <- predict(rf.fit, val_data_1)
(4372+5044)/(4372+5044+88887+77159)

#COMPARE TREE AND FOREST
oob.err = double(3)
test.err = double(3)
for(mtry in 1:3){
  fit = randomForest(Label~NDAI + SD + FBDF, data = train_data_1, mtry=mtry, ntree = 10)
  pred = predict(fit, val_data_1)
  test.err[mtry] = with(val_data_1, 1-Accuracy(pred,val_data_1$Label))
}


matplot(1:mtry, test.err, pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = "Test", pch = 23, col = "red")

#METHOS 6 BOOST
boost.fit = gbm(as.numeric(as.character(Label))~NDAI + SD + FBDF, data = train, n.trees = 1000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.fit)
plot(boost.fit,i="NDAI")
plot(boost.fit,i="SD")
plot(boost.fit,i="FBDF")
Accuracy(newpred, val_data_1$Label)
newpred <- ifelse(predmat > 0, 1, 0)




##################################################################################
##################################################################################
#CV
##################################################################################
##################################################################################


# Try several classification methods and assess their fit using cross-validation CV. 
# Report the accuracies across folds (and not just the average across folds) and the test accuracy. 
# CV-results for both the ways of creating folds (as answered in part 2(a)) should be reported.



##################################################################################
##################################################################################
#ROC
##################################################################################
##################################################################################
glm.fit <- glm(as.numeric(as.character(Label)) ~ NDAI + SD + FBDF, 
               data = train_data_1, family = binomial)
boost.fit = gbm(as.numeric(as.character(Label))~NDAI + SD + FBDF, 
                data = train_data_1, n.trees = 1000, shrinkage = 0.01, interaction.depth = 4)
lda.fit <- lda(Label ~ NDAI + SD + FBDF, data = train_data_1)
qda.fit <- qda(Label ~ NDAI + SD + FBDF, data = train_data_1)
tree.fit <- tree(Label~NDAI + SD + FBDF, data = train_data_1)
forest.fit <- randomForest(Label~NDAI + SD + FBDF, data = train_data_1, maxnodes = 10)

###predictions
glm.pred <- as.numeric(ifelse(unname(predict(glm.fit, val_data_1[, 4:6]))>0, 1, 0))
boost.pred <- as.numeric(ifelse(predict(boost.fit, newdata = val_data_1, n.trees = 1000) >0, 1, 0))
lda.pred <- as.numeric(as.character(predict(lda.fit, val_data_1)$class))
qda.pred <- as.numeric(as.character(predict(qda.fit, val_data_1)$class))
tree.pred <- as.numeric(as.character(predict(tree.fit, val_data_1, type="class")))
forest.pred <- as.numeric(as.character(predict(forest.fit, val_data_1)))

Accuracy(glm.pred, as.numeric(as.character(val_data_1$Label)))
Accuracy(boost.pred, val_data_1$Label)
Accuracy(lda.pred, val_data_1$Label)
Accuracy(qda.pred, val_data_1$Label)
Accuracy(tree.pred, val_data_1$Label)
Accuracy(forest.pred, val_data_1$Label)



# List of predictions
preds_list <- list(glm.pred, boost.pred, lda.pred, qda.pred, tree.pred, forest.pred)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(val_data_1$Label), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Logit", "Boosting", "LDA", "QDA", "Tree", "RF"),
       fill = 1:m)

##################################################################################
##################################################################################
#Other Revelant Metrics
##################################################################################
##################################################################################


labels <- train_data_1$Label
k = 10
features <- c("NDAI", "SD", "FBDF")

##################################################################################
##################################################################################
#Precision
##################################################################################
##################################################################################

Pre_glm <- CVgeneric(train_data_1 = train_data_1, features = features, 
                   labels = labels, classifier = "glm", k=10, loss = "Precision")

Pre_lda <- CVgeneric(train_data_1 = train_data_1, features = features, 
                   labels = labels, classifier = "lda", k=10, loss = "Precision")
Pre_qda <- CVgeneric(train_data_1 = train_data_1, features = features, 
                   labels = labels, classifier = "lda", k=10, loss = "Precision")
Pre_tree <- CVgeneric(train_data_1 = train_data_1, features = features, 
                    labels = labels, classifier = "tree", k=10, loss = "Precision")
Pre_rf <- CVgeneric(train_data_1 = train_data_1, features = features, 
                  labels = labels, classifier = "rf", k=10, loss = "Precision")
Pre_boost <- CVgeneric(train_data_1 = train_data_1, features = features, 
                     labels = labels, classifier = "boost", k=10, loss = "Precision")

Pre_table <- cbind(Pre_glm, Pre_lda, Pre_qda, Pre_tree, Pre_rf, Pre_boost)
Precision <- round(c(GLM = mean(Pre_glm), 
        LDA = mean(Pre_lda),
        QDA = mean(Pre_qda),
        Tree = mean(Pre_tree),
        Forest = mean(Pre_rf), 
        Boosting = mean(Pre_boost)), 3)

##################################################################################
##################################################################################
#Sensitivity
##################################################################################
##################################################################################
Sens_glm <- CVgeneric(train_data_1 = train_data_1, features = features, 
                     labels = labels, classifier = "glm", k=10, loss = "Sensitivity")
Sens_lda <- CVgeneric(train_data_1 = train_data_1, features = features, 
                     labels = labels, classifier = "lda", k=10, loss = "Sensitivity")
Sens_qda <- CVgeneric(train_data_1 = train_data_1, features = features, 
                     labels = labels, classifier = "lda", k=10, loss = "Sensitivity")
Sens_tree <- CVgeneric(train_data_1 = train_data_1, features = features, 
                      labels = labels, classifier = "tree", k=10, loss = "Sensitivity")
Sens_rf <- CVgeneric(train_data_1 = train_data_1, features = features, 
                    labels = labels, classifier = "rf", k=10, loss = "Sensitivity")
Sens_boost <- CVgeneric(train_data_1 = train_data_1, features = features, 
                       labels = labels, classifier = "boost", k=10, loss = "Sensitivity")

Sens_table <- cbind(Sens_glm, Sens_lda, Sens_qda, Sens_tree, Sens_rf, Sens_boost)
Sensitivity <- round(c(GLM = mean(Sens_glm), 
        LDA = mean(Sens_lda),
        QDA = mean(Sens_qda),
        Tree = mean(Sens_tree),
        Forest = mean(Sens_rf), 
        Boosting = mean(Sens_boost)), 3)

##################################################################################
##################################################################################
#Specificity
##################################################################################
##################################################################################
Spec_glm <- CVgeneric(train_data_1 = train_data_1, features = features, 
                      labels = labels, classifier = "glm", k=10, loss = "Specificity")
Spec_lda <- CVgeneric(train_data_1 = train_data_1, features = features, 
                      labels = labels, classifier = "lda", k=10, loss = "Specificity")
Spec_qda <- CVgeneric(train_data_1 = train_data_1, features = features, 
                      labels = labels, classifier = "lda", k=10, loss = "Specificity")
Spec_tree <- CVgeneric(train_data_1 = train_data_1, features = features, 
                       labels = labels, classifier = "tree", k=10, loss = "Specificity")
Spec_rf <- CVgeneric(train_data_1 = train_data_1, features = features, 
                     labels = labels, classifier = "rf", k=10, loss = "Specificity")
Spec_boost <- CVgeneric(train_data_1 = train_data_1, features = features, 
                        labels = labels, classifier = "boost", k=10, loss = "Specificity")

Spec_table <- cbind(Spec_glm, Spec_lda, Spec_qda, Spec_tree, Spec_rf, Spec_boost)
Specificity <- round(c(GLM = mean(Spec_glm), 
        LDA = mean(Spec_lda),
        QDA = mean(Spec_qda),
        Tree = mean(Spec_tree),
        Forest = mean(Spec_rf), 
        Boosting = mean(Spec_boost)), 3)

##################################################################################
##################################################################################
#AC
##################################################################################
##################################################################################
labels <- train_data_1$Label
k = 10
features <- c("NDAI", "SD", "FBDF")

Ac_glm <- CVgeneric(train_data_1 = train_data_1, features = features,
                      labels = labels, classifier = "glm", k=10, loss = "Accuracy")
Ac_lda <- CVgeneric(train_data_1 = train_data_1, features = features,
                      labels = labels, classifier = "lda", k=10, loss = "Accuracy")
Ac_qda <- CVgeneric(train_data_1 = train_data_1, features = features,
                      labels = labels, classifier = "lda", k=10, loss = "Accuracy")
Ac_tree <- CVgeneric(train_data_1 = train_data_1, features = features,
                       labels = labels, classifier = "tree", k=10, loss = "Accuracy")
Ac_rf <- CVgeneric(train_data_1 = train_data_1, features = features,
                     labels = labels, classifier = "rf", k=10, loss = "Accuracy")
Ac_boost <- CVgeneric(train_data_1 = train_data_1, features = features,
                        labels = labels, classifier = "boost", k=10, loss = "Accuracy")

Acc_1_table <- cbind(Ac_glm, Ac_lda, Ac_qda, Ac_tree, Ac_rf, Ac_boost)
Acc_1 <- round(c(GLM = mean(Ac_glm),
        LDA = mean(Ac_lda),
        QDA = mean(Ac_qda),
        Tree = mean(Ac_tree),
        Forest = mean(Ac_rf),
        Boosting = mean(Ac_boost)), 3)

labels <- train_data_2$Label
k = 10
features <- c("NDAI", "SD", "FBDF")
Ac_glm2 <- CVgeneric(train_data_1 = train_data_2, features = features,
                    labels = labels, classifier = "glm", k=10, loss = "Accuracy")
Ac_lda2 <- CVgeneric(train_data_1 = train_data_2, features = features,
                    labels = labels, classifier = "lda", k=10, loss = "Accuracy")
Ac_qda2 <- CVgeneric(train_data_1 = train_data_2, features = features,
                    labels = labels, classifier = "lda", k=10, loss = "Accuracy")
Ac_tree2 <- CVgeneric(train_data_1 = train_data_2, features = features,
                     labels = labels, classifier = "tree", k=10, loss = "Accuracy")
Ac_rf2 <- CVgeneric(train_data_1 = train_data_2, features = features,
                   labels = labels, classifier = "rf", k=10, loss = "Accuracy")
Ac_boost2 <- CVgeneric(train_data_1 = train_data_2, features = features,
                      labels = labels, classifier = "boost", k=10, loss = "Accuracy")

Acc_2_table <- cbind(Ac_glm2, Ac_lda2, Ac_qda2, Ac_tree2, Ac_rf2, Ac_boost2)
Acc_2 <- round(c(GLM = mean(Ac_glm2),
                 LDA = mean(Ac_lda2),
                 QDA = mean(Ac_qda2),
                 Tree = mean(Ac_tree2),
                 Forest = mean(Ac_rf2),
                 Boosting = mean(Ac_boost2)), 3)
labels <- train_data_1$Label
k = 10
features <- c("NDAI", "SD", "FBDF")

##################################################################################
##################################################################################
#AREA UNDER CURVE
##################################################################################
##################################################################################

plotROC(val_data_1$Label, glm.pred, Show.labels = 1)
plotROC(val_data_1$Label, lda.pred, Show.labels = 1)
plotROC(val_data_1$Label, qda.pred, Show.labels = 1)
plotROC(val_data_1$Label, forest.pred, Show.labels = 1)
plotROC(val_data_1$Label, tree.pred, Show.labels = 1)
plotROC(val_data_1$Label, boost.pred, Show.labels = 1)



##################################################################################
##################################################################################
#HISTOGRAMS OF O.M.
##################################################################################
##################################################################################

Sensitivity <- round(c(GLM = mean(Spec_glm), 
        LDA = mean(Spec_lda),
        QDA = mean(Spec_qda),
        Tree = mean(Spec_tree),
        Forest = mean(Spec_rf), 
        Boosting = mean(Spec_boost)), 3)
Specificity <- round(c(GLM = mean(Sens_glm), 
        LDA = mean(Sens_lda),
        QDA = mean(Sens_qda),
        Tree = mean(Sens_tree),
        Forest = mean(Sens_rf), 
        Boosting = mean(Sens_boost)), 3)

Precision <- round(c(GLM = mean(Pre_glm), 
                     LDA = mean(Pre_lda),
                     QDA = mean(Pre_qda),
                     Tree = mean(Pre_tree),
                     Forest = mean(Pre_rf), 
                     Boosting = mean(Pre_boost)), 3)

O_m <- t(rbind(Sensitivity, Specificity, Precision))

barplot(O_m,beside=TRUE, main = "Other metrics", ylim = c(0.8, 1),
        names.arg = c(" ", " ", " "),
        legend.text = c("GLM", "LDA", "QDA", "Tree", "RF", "Boost"),
        ylab = "Quality"
        )


##################################################################################
##################################################################################
#Diagnostics
##################################################################################
##################################################################################
#A gradient boosted model with bernoulli loss function.

boost.fit = gbm(as.numeric(as.character(Label))~NDAI + SD + FBDF, 
                data = train_data_1, n.trees =1000, shrinkage = 0.01, interaction.depth = 4)
boost.pred <- as.numeric(ifelse(predict(boost.fit, newdata = val_data_1, n.trees = 1000) >0, 1, 0))

boost_imp <- t(varImp(boost.fit, numTrees = 1000,scale = 1))
pie(boost_imp, labels = c("NDAI", "SD", "FBDF"), col = c("lightgreen", "lightblue", "grey"),
    main = "Variable Importance")

optCutOff <- optimalCutoff(val_data_1$Label, boost.pred)[1] 

#http://r-statistics.co/Information-Value-With-R.html
misClassError(val_data_1$Label, boost.pred, threshold = optCutOff)
# plotROC(val_data_1$Label, boost.pred)
Coord <- Concordance(val_data_1$Label, boost.pred)
confusionMatrix(val_data_1$Label, boost.pred, threshold = optCutOff)
youdensIndex(val_data_1$Label, boost.pred)
ks_plot(val_data_1$Label, boost.pred)
#Interpretation in the link above


n.trees = seq(from = 100, to = 1000, by = 50)
predmat = ifelse(predict(boost.fit, newdata = val_data_1, n.trees = n.trees)>0, 1, 0)
dim(predmat)

boost.acc <- c(0)
boost.zol <- c(0)
for (i in 1:19){
  boost.acc[i] = Accuracy(predmat[ ,i], val_data_1$Label)
  boost.zol[i] = ZeroOneLoss(predmat[ ,i], val_data_1$Label)
}

par(mfrow=c(1, 2))

plot(n.trees, boost.acc, pch = 23, ylab = "Accuracy", xlab = "# Trees", 
     col = "blue", main = "Boosting Test Error", type = "o")
abline(h = 0.8856373, col = "red")

plot(n.trees, boost.zol, pch = 23, ylab = "Zero-One Loss", xlab = "# Trees", 
     col = "green", main = "Boosting Test Error", type = "o")
abline(h = 0.1143627, col = "red")

par(mfrow=c(1,1))


#shrinkage = 0.01, interaction.depth = 4

n.trees = 300
shrinkage = seq(0, 0.05, 0.01)
boost.acc <- c(0)
boost.zol <- c(0)
for (i in 1:6){
boost.fit = gbm(as.numeric(as.character(Label))~NDAI + SD + FBDF, 
                data = train_data_1, n.trees =300, shrinkage = shrinkage[i], interaction.depth = 4)
predmat = ifelse(predict(boost.fit, newdata = val_data_1, n.trees = n.trees)>0, 1, 0)
  boost.acc[i] = Accuracy(predmat, val_data_1$Label)
  boost.zol[i] = ZeroOneLoss(predmat, val_data_1$Label)
}

par(mfrow=c(1, 2))

plot(shrinkage, boost.acc, pch = 23, ylab = "Accuracy", xlab = "Shrinkage Parameter", 
     col = "blue", main = "Boosting Test Error", type = "o")
abline(h = 0.8776508, col = "red")

plot(shrinkage, boost.zol, pch = 23, ylab = "Zero-One Loss", xlab = "Shrinkage Parameter", 
     col = "green", main = "Boosting Test Error", type = "o")
abline(h = 0.1223492, col = "red")

par(mfrow=c(1,1))

n.trees = 300
shrinkage = 0.01
interaction.depth = c(3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40)

boost.acc <- c(0)
boost.zol <- c(0)
for (i in 1:length(interaction.depth)){
  boost.fit = gbm(as.numeric(as.character(Label))~NDAI + SD + FBDF, 
                  data = train_data_1, n.trees =300, shrinkage = shrinkage, interaction.depth = interaction.depth[i])
  predmat = ifelse(predict(boost.fit, newdata = val_data_1, n.trees = n.trees)>0, 1, 0)
  boost.acc[i] = Accuracy(predmat, val_data_1$Label)
  boost.zol[i] = ZeroOneLoss(predmat, val_data_1$Label)
}

par(mfrow=c(1, 2))

plot(interaction.depth, boost.acc, pch = 23, ylab = "Accuracy", xlab = "Interaction Depth", 
     col = "blue", main = "Boosting Test Error", type = "o")
abline(h =0.8856116, col = "red")

plot(interaction.depth, boost.zol, pch = 23, ylab = "Zero-One Loss", xlab = "Interaction Depth", 
     col = "green", main = "Boosting Test Error", type = "o")
abline(h = 0.1143884, col = "red")

par(mfrow=c(1,1))
##################################################################################
##################################################################################
#RESIDUAL
##################################################################################
##################################################################################


boost.fit = gbm(as.numeric(as.character(Label))~NDAI + SD + FBDF, 
                data = train_data_1, n.trees =400, shrinkage = 0.01, interaction.depth = 4)


data1 <- read.csv("image1.txt", sep = "", header = T, col.names = head)
data1$FBDF <- c()
for (i in 1:length(data1$y)){
  data1$FBDF[i] = abs(data1$AF[i]-data1$AN[i])/(data1$AF[i])
}

# data1$Label <- as.factor(as.character(data1$Label))
# levels(data1$Label) <- c("0", "NA", "1")
# data1 <- data1[which(data1$Label!="NA"), ]
# data1$Label <- as.factor(as.numeric(as.character(data1$Label)))

boost.pred1 = ifelse(predict(boost.fit, newdata  = data1, n.trees = 400, shrinkage = 0.01, 
                            interaction.depth = 4)>0, 1, 0)
boost.pred2 = ifelse(predict(boost.fit, newdata  = data2, n.trees = 400, shrinkage = 0.01, 
                            interaction.depth = 4)>0, 1, 0)
boost.pred3 = ifelse(predict(boost.fit, newdata  = data3, n.trees = 400, shrinkage = 0.01, 
                            interaction.depth = 4)>0, 1, 0)
Accuracy(boost.pred1, data1$Label)
Accuracy(boost.pred2, data2$Label)
Accuracy(boost.pred3, data3$Label)

# glm.pred = as.numeric(ifelse(unname(predict(glm.fit, data1[, 4:6]))>0, 1, 0))
# lda.pred <- as.numeric(as.character(predict(lda.fit, data1)$class))
# qda.pred <- as.numeric(as.character(predict(qda.fit, data2)$class))
# tree.pred <- as.numeric(as.character(predict(tree.fit, data1, type="class")))
# forest.pred <- as.numeric(as.character(predict(forest.fit, data2)))

TrueLabels1 <- ggplot(data1, aes(x,y, col = as.factor(Label))) +
  geom_point() +
  scale_color_manual(values=c("lightgreen", "lightblue"),
                     labels= c("Cloud free","Clouds")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_blank(),
      legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("True Labeling Image 1")

boost.misclass1 <- ifelse(boost.pred1 == data1$Label, 0, 1)

PredLabels1 <- ggplot(data1, aes(x,y, col = as.factor(boost.pred1))) +
  geom_point() +
    scale_color_manual(values=c("lightgreen", "lightblue"),
                       labels= c("Cloud Free", "Cloud")) +
    labs(col="Presence of clouds") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_blank(),
          legend.position="bottom") +
    ylab("")+
    xlab("")+
    ggtitle("Gradient Boosting Image 1")

Misclass1 <- ggplot(data1, aes(x,y, col = as.factor(boost.misclass1))) +
  geom_point() +
  scale_color_manual(values=c("grey", "red"),
                     labels= c("Correct", "Incorrect")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("Misclassified Image 1")

TrueLabels2 <- ggplot(data2, aes(x,y, col = as.factor(Label))) +
  geom_point() +
  scale_color_manual(values=c("lightgreen", "lightblue"),
                     labels= c("Cloud free","Clouds")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("True Labeling Image 2")

boost.misclass2 <- ifelse(boost.pred2 == data2$Label, 0, 1)

PredLabels2 <- ggplot(data2, aes(x,y, col = as.factor(boost.pred2))) +
  geom_point() +
  scale_color_manual(values=c("lightgreen", "lightblue"),
                     labels= c("Cloud Free", "Clouds")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("Gradient Boosting Image 2")

Misclass2 <- ggplot(data2, aes(x,y, col = as.factor(boost.misclass2))) +
  geom_point() +
  scale_color_manual(values=c("grey", "red"),
                     labels= c("Correct", "Incorrect")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("Misclassified Image 2")

TrueLabels3 <- ggplot(data3, aes(x,y, col = as.factor(Label))) +
  geom_point() +
  scale_color_manual(values=c("lightgreen", "lightblue", "gray"),
                     labels= c("Cloud free","Clouds")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("True Labeling Image 3")

boost.misclass3 <- ifelse(boost.pred3 == data3$Label, 0, 1)

PredLabels3 <- ggplot(data3, aes(x,y, col = as.factor(boost.pred3))) +
  geom_point() +
  scale_color_manual(values=c("lightgreen", "lightblue"),
                     labels= c("Cloud feee", "Clouds")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("Gradient Boosting Image 3")

Misclass3 <- ggplot(data3, aes(x,y, col = as.factor(boost.misclass3))) +
  geom_point() +
  scale_color_manual(values=c("grey", "red"),
                     labels= c("Correct", "Incorrect")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("Misclassified Image 3")

grid.arrange(TrueLabels1, PredLabels1, Misclass1, TrueLabels2, PredLabels2, Misclass2,TrueLabels3, PredLabels3, Misclass3, nrow = 3)

data1$misclass <- boost.misclass1
data2$misclass <- boost.misclass2
data3$misclass <- boost.misclass3

resid1 <- data1[which(data1$misclass == 1), ]
resid2 <- data2[which(data2$misclass == 1), ]
resid3 <- data3[which(data3$misclass == 1), ]

# r1 <- ggplot(resid1, aes(x = Label, y = FBDF, col = Label))+
#   geom_boxplot() +
#   scale_color_manual(values=c("lightgreen", "lightblue"),
#                      labels= c("Cloud free","Clouds"))
# r2 <- ggplot(resid1, aes(x = Label, y = NDAI, col = Label))+
#   geom_boxplot() +
#   scale_color_manual(values=c("lightgreen", "lightblue"),
#                      labels= c("Cloud free","Clouds"))
# r3 <- ggplot(resid1, aes(x = Label, y = SD, col = Label))+
#   geom_boxplot() +
#   scale_color_manual(values=c("lightgreen", "lightblue"),
#                      labels= c("Cloud free", "Clouds"))
r1 <- ggplot(resid1, aes(x = Label, y = x, col = Label))+
  geom_boxplot() +
  scale_color_manual(values=c("lightgreen", "lightblue"),
                     labels= c("Cloud free", "Clouds"))

r2 <- ggplot(resid1, aes(x = Label, y =y, col = Label))+
  geom_boxplot() +
  scale_color_manual(values=c("lightgreen", "lightblue"),
                     labels= c("Cloud free", "Clouds"))

grid.arrange(r1, r2, nrow = 1)

par(mfrow=c(1,3))

cdplot(factor(Label)~FBDF, data = resid1[which(resid1$FBDF<0.), ], ylab = " ", xlab = "FBDF", )
cdplot(factor(Label)~NDAI, data = resid1, ylab = " ", xlab = "NDAI", )
cdplot(factor(Label)~log(SD), data = resid1, xlab = "Log of SD", )

par(mfrow=c(1,1))

##################################################################################
##################################################################################
#FINAL
##################################################################################
##################################################################################



head <- c("y", "x", "Label", "NDAI", "SD", "CORR","DF", "CF", "BF", "AF", "AN")
data1 <- read.delim("image1.txt", sep = "", col.names = head)
data2 <- read.delim("image2.txt", sep = "", col.names = head)
data3 <- read.delim("image3.txt", sep = "", col.names = head)

data1$FBDF <- c()
for (i in 1:length(data1$y)){
  data1$FBDF[i] = abs(data1$AF[i]-data1$AN[i])/(data1$AF[i])
}

data2$FBDF <- c()
for (i in 1:length(data2$y)){
  data2$FBDF[i] = abs(data2$AF[i]-data2$AN[i])/(data2$AF[i])
}

data3$FBDF <- c()
for (i in 1:length(data3$y)){
  data3$FBDF[i] = abs(data3$AF[i]-data3$AN[i])/(data3$AF[i])
}


data1 <- data1[, c(1:5, 12)]
data2 <- data2[, c(1:5, 12)]
data3 <- data3[, c(1:5, 12)]

data1$image <- 1
data2$image <- 2
data3$image <- 3

data1$Label <- as.factor(as.character(data1$Label))
levels(data1$Label) <- c("0","NA", "1")
#data1 <- data1[which(data1$Label!="NA"), ]
data1$Label <- as.factor(as.numeric(as.character(data1$Label)))

data2$Label <- as.factor(as.character(data2$Label))
levels(data2$Label) <- c("0", "NA", "1")
data2 <- data2[which(data2$Label!="NA"), ]
data2$Label <- as.factor(as.numeric(as.character(data2$Label)))

data3$Label <- as.factor(as.character(data3$Label))
levels(data3$Label) <- c("0",  "NA", "1")
data3 <- data3[which(data3$Label!="NA"), ]
data3$Label <- as.factor(as.numeric(as.character(data3$Label)))
sum(is.na(data3$Label))

data1$SD <- log(data1$SD)
data2$SD <- log(data2$SD)
data3$SD <- log(data3$SD)

boost.pred1 = ifelse(predict(boost.fit, newdata  = data1, n.trees = 350, shrinkage = 0.01, 
                             interaction.depth = 4)>0, 1, 0)
boost.pred2 = ifelse(predict(boost.fit, newdata  = data2, n.trees = 350, shrinkage = 0.01, 
                             interaction.depth = 4)>0, 1, 0)
boost.pred3 = ifelse(predict(boost.fit, newdata  = data3, n.trees = 350, shrinkage = 0.01, 
                             interaction.depth = 4)>0, 1, 0)

final1 <- ggplot(data1, aes(x,y, col = as.factor(boost.pred1))) +
  geom_point() +
  scale_color_manual(values=c("lightgreen", "lightblue"),
                     labels= c("Correct", "Incorrect")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("Results: Image 1")

final2 <- ggplot(data2, aes(x,y, col = as.factor(boost.pred2))) +
  geom_point() +
  scale_color_manual(values=c("lightgreen", "lightblue"),
                     labels= c("Correct", "Incorrect")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("Results: Image 2")

final3 <- ggplot(data3, aes(x,y, col = as.factor(boost.pred3))) +
  geom_point() +
  scale_color_manual(values=c("lightgreen", "lightblue"),
                     labels= c("Correct", "Incorrect")) +
  labs(col="Presence of clouds") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        legend.position="bottom") +
  ylab("")+
  xlab("")+
  ggtitle("Results: Image 3")

grid.arrange(final1, final2, final3, nrow = 1)





















