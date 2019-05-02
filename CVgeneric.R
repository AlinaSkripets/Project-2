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

