require(AppliedPredictiveModeling)
require(caret)
require(dplyr)
require(ipred)
require(ggplot2)
require(plotly)
require(MASS)
CDAT<-data(twoClassData)
CDAT<-cbind(predictors,classes)
str(CDAT)
#Scatter plot for the predictors with respect to class
ggplot(CDAT,aes(PredictorA,PredictorB,color=classes))+
  geom_point(alpha=0.5)+facet_wrap(aes(theme=classes))+
  labs(title = "Scatter Plot for the Predictors")

ggplot(CDAT,aes(PredictorA,PredictorB,color=classes))+
  geom_point(alpha=0.5)+
  labs(title = "Scatter Plot for the Predictors")

#Data partition using caret
index1<-createDataPartition(classes,p=0.8,list = FALSE)
trainDat<-CDAT[index1,]
#train.class<-classes[index1]
testDat<-CDAT[-index1,]
#test.class<-classes[-index1]
length(test.class)
#Train knn model using train function in caret 

Mfit<-train(classes~.,method="knn",
            data = trainDat,preProc=c("center","scale"))
print(Mfit)

Predfit<-predict(Mfit,testDat)

confusionMatrix(Predfit,testDat$classes)

#Tuning the model by search best k value
Mfit2<-train(classes~.,trainDat,tuneLength=10,
             preProc=c("center","scale"),method="knn")

print(Mfit2)

#10 fold cross validation 
trcl<-trainControl(method = "repeatedcv",repeats = 4)

Mfit3<-train(classes~.,data=trainDat,
             method="knn",trControl=trcl,
             preProc=c("center","scale"),
             tuneLength=10)

print(Mfit3)

plot(Mfit3,main="10 Fold Cross Validationn for knn")
#Model Validation
Pred.fit2<-predict(Mfit3,testDat)
confusionMatrix(Pred.fit2,testDat$classes)

##We can also use knn3 function in caret to fit knn model
knn.predTr<-as.matrix(predictors[index1,])
knn.classTr<-classes[index1]
knn.predTe<-as.matrix(predictors[-index1,])
knn.classTe<-classes[-index1]
#Fit the knn model using 5 nearest neghbours
fit1<-knn3(x=knn.predTr,y=knn.classTr,k=5,prob=T)
fit1

#Get the prediction
predf1<-predict(fit1,knn.predTe,type = "class")
predf1

#Get the confusion matrix
confusionMatrix(predf1,knn.classTe)

data("GermanCredit")
Dat1<-GermanCredit
str(Dat1)
#Vissualization of few predictors

l1<-ggplot(Dat1,aes(Duration,Amount,color=Class))+
  geom_point(aes(frame= Property.Insurance),position = "jitter")+
  facet_wrap(aes(theme=Job.SkilledEmployee))
l1
ggplotly(l1)##Give the interactive vizualization

##More plots can be investigated as above using differents variables and 
#geometrics

Dat11<-Dat1 %>% 
  dplyr::select(-Class) %>% 
  arrange(desc(Age))

head(Dat11)
#Check the correlation
correl<-cor(Dat11[,1:10])
require(corrplot)
require(e1071)
corrplot::corrplot(correl,method = "color")
head(correl)

#Check out the skewness
apply(Dat11,2,FUN = summary)
apply(Dat11,2,skewness)

#Correct skewness

skwnl<-preProcess(Dat11,method = c("BoxCox"))
skwnl

trfDat11<-predict(skwnl,Dat11)
head(trfDat11)

#histogram for any selected predictor

ggplot(trfDat11,aes(Amount),fill=SavingsAccountBonds.gt.1000,
       color=SavingsAccountBonds.gt.1000)+
  geom_histogram(position = "identity")+
  facet_wrap(aes(Job.SkilledEmployee))+
  labs(title = "Histogram for Credit amount")

###Training SVM model
indcred<-createDataPartition(Dat1$Class,p=0.8,list = FALSE)
trainCred<-Dat1[indcred,]
trainCrs<-Dat1$Class[indcred]

testCred<-Dat1[-indcred,]
testCrs<-Dat1$Class[-indcred]

#Training SVM model
svmfit<-train(Class~.,method="svmRadial",
              data = Dat1)
svmfit

#Preprocess and tunning the data 
svmfit2<-train(Class~.,data = Dat1,
               method="svmRadial",tuneLength=10,
               preProc=c("center","scale","BoxCox"))
svmfit2

#To perform 10 fold cross validation with trainControl
set.seed(123)
# svmcvfit<-train(Class~.,data=trainCred,
#                 method="svmRadial",tuneLength=10,
#                 preProc=c('center','scale','BoxCox'),
#                 trainControl(method = "repeatedcv",repeats = 5))

#From the results above we can produce performance plot as

plot(svmcvfit,scales=list(x=list(log=2)),
     main = "Performance profile for SVM with Radial Kernel")

#Predicting new data we do as

classprednew<-predict(svmcvfit,testCred,type="prob")

###Model comparison

#We may fit a logistic regresion model using same specifications as above

logfit<-train(Class~., data=trainCred,
              method="glm",preProc=c("center","scale","BoxCox"),
              trainControl(method = "repeatedcv",repeats = 5))
logfit
#Model comparison using resamples since both SVM and logistic 
#regression used same resamples in their CV

resamp1<-resamples(list(SVM=svmcvfit,Logistic=logfit))
summary(resamp1)

#Assess the significance difference between the models

ModDiff<-diff(resamp1)
aummary(ModDiff)

#To vissualize the difference
bwplot(resamp1,metric="RMSE")

###Work out with Music Genre Dataset####

