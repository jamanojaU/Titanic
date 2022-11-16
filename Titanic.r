library(dplyr)
library(e1071)
data_titanic <- read.csv("C:/Users/jesus/Downloads/train.csv")
data_titanic <- na.omit(data_titanic)
data_titanic <- select(data_titanic, -PassengerId, -Name, -Ticket, -Cabin, -Embarked)

data_titanic<-data_titanic[1:50,]
#data_titanic$Embarked=NULL


#cuenta el numero de na por columna
#sampply(data_titanic, funcion(x) sum(is.na(x)))


#SVM
#validacion cruzada
set.seed(200)
d_size<-dim(data_titanic)[1]
dtest_size <- ceiling(0.2*d_size)
samples<-sample(d_size, d_size, replace=FALSE)
indexes<-samples[1:dtest_size]
dtrain<-data_titanic[-indexes,]
dtest<-data_titanic[indexes,]

svp <- svm(Survived~.,data=dtrain,C = 100, kernel='linear',scaled=c(),cost=1000, cross=2,scale=FALSE, Probabilistic=FALSE)

matrizconfusionSVM<-table(predict(svp,dtest), dtest$Survived , dnn=c("Prediction", "Actual"))
accuracySVM<- sum(diag(matrizconfusionSVM))/sum(matrizconfusionSVM)

#USAMOS TUNE
svm_cvpolinomial <- tune("svm", Survived ~., data = dtrain, kernel = 'polynomial',
                         ranges = list(cost = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                                       degree = c( 1, 2, 3, 4)), gamma=1, coef0=1)

#MOSTRAMOS LOS RESULTADOS 
svm_cvpolinomial$best.parameters
mejorSVM<-svm_cvpolinomial$best.model
matrizconfusionSVMBetter<-table(predict(mejorSVM,dtest), dtest$Survived, dnn=c("Prediction", "Actual"))
accuracySVMBetter<- sum(diag(matrizconfusionSVMBetter))/sum(matrizconfusionSVMBetter)
matrizconfusionSVM
matrizconfusionSVMBetter
accuracySVMBetter
accuracySVM


#RPART

fm <- formula(Survived ~ Pclass +Sex + Age + SibSp + Parch + Fare)
dtrain.rpart <- tune.rpart(fm, data=dtrain, minsplit=seq(10,100,10))
plot(dtrain.rpart, main="Tune rpart on minsplit")




