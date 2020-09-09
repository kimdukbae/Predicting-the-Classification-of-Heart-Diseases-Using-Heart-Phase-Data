library(dplyr)
library(ggplot2)

# 데이터 취즉 및 전처리

heart_disease = read.csv('C:/Sources/heart.csv', header = TRUE)
str(heart_disease)
names(heart_disease) = c("age","sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")
str(heart_disease)
summary(heart_disease)

#결측값 파악
table(is.na(heart_disease))

#이상치 파악
library(reshape2)
heart.melt <- melt(heart_disease, id.var="target")
ggplot(data=heart.melt, aes(x=as.factor(target), y=value)) + geom_boxplot() + facet_wrap(~variable, ncol=2)



# 데이터 시각화
heart_disease %>% ggplot(aes(x=1, y=exang, color=target)) + geom_jitter()
heart_disease %>% ggplot(aes(x=1, y=sex, color=target)) + geom_jitter()
heart_disease %>% ggplot(aes(x=1, y=cp, color=target)) + geom_jitter()
heart_disease %>% ggplot(aes(x=1, y=oldpeak, color=target)) + geom_jitter()
heart_disease %>% ggplot(aes(x=1, y=thalach, color=target)) + geom_jitter()
heart_disease %>% ggplot(aes(x=1, y=ca, color=target)) + geom_jitter()
heart_disease %>% ggplot(aes(x=1, y=slope, color=target)) + geom_jitter()
heart_disease %>% ggplot(aes(x=1, y=thal, color=target)) + geom_jitter()
heart_disease %>% ggplot(aes(x=thalach, y=slope, color=target)) + geom_jitter()


# heart_disease2 데이터

heart_disease2 = read.csv('C:/Sources/heart.csv', header = TRUE)
names(heart_disease2) = c("age","sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
str(heart_disease2)

changeType <-c("numeric", "factor", "factor", "numeric", "numeric", "factor", "factor", "numeric", "factor", "numeric", "factor", "factor", "factor", "factor")

# 구글링 참고 -> 데이터 형 변환

changeFunc <- function(obj,types){
  t1 <- lapply(1:length(obj),FUN = function(i){FUN1 <- 
    switch(types[i],
           character = as.character,
           numeric = as.numeric,
           factor = as.factor); FUN1(obj[,i])})
  names(t1) <- colnames(obj)
  as.data.frame(t1)
}

heart_disease2 <- changeFunc(heart_disease2, changeType)
str(heart_disease2)

levels(heart_disease2$num) = c("No disease", "Disease")
levels(heart_disease2$sex) = c("female", "male")

mosaicplot(heart_disease2$sex ~ heart_disease2$num, color=TRUE, main="성별과 심장병 유무 관계" ,xlab="성별", ylab="심장병 유무")


#데이터 자르기
library(caret)
set.seed(10)
trainRows <- createDataPartition(heart_disease$target, p=0.8, list=FALSE)
train <- heart_disease[trainRows, ]
test <- heart_disease[-trainRows, ]
str(train)
str(test)

nrow(test) + nrow(train)
nrow(heart_disease)


#rpart
library(rpart)
dt = train(as.factor(target) ~ . , data=train, method='rpart')
summary(dt)
p <- predict(dt, newdata = test, type='raw')
table(p, test$target)


#randomForest
library(randomForest)
rf = train(as.factor(target) ~ . , data=train, method='rf')
summary(rf)
p1 <- predict(rf, newdata = test, type='raw')
table(p1, test$target)


#SVM
library(e1071)
sv = svm(as.factor(target) ~ . , data=train, kernel='linear', cost=0.01, scale=TRUE)
summary(sv)
p2 <- predict(sv, newdata = test)
table(p2, test$target)
mean(test$target == p2)


#kNN
library(class)
kn = knn(train=train[ ,c(1:13)], test=test[ ,c(1:13)], cl=train$target, k=10)
summary(kn)
table(kn, test$target)
mean(test$target==kn)

kn20 = knn(train=train[ ,c(1:13)], test=test[ ,c(1:13)], cl=train$target, k=20)
table(kn20, test$target)


kn30 = knn(train=train[ ,c(1:13)], test=test[ ,c(1:13)], cl=train$target, k=30)
table(kn30, test$target)


#일반화 (데이터 셋 전부다 numeric형)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
heart_disease.new<- as.data.frame(lapply(heart_disease[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)],normalize))

str(heart_disease.new)
head(heart_disease.new)

set.seed(10)
trainRows2 <- createDataPartition(heart_disease.new$target, p=0.8, list=FALSE)
train2 <- heart_disease.new[trainRows2, ]
test2 <- heart_disease[-trainRows2, ]
str(train2)
str(test2)
nrow(train2) + nrow(test2)
nrow(heart_disease)
nrow(heart_disease.new)

kn2 = knn(train=train2[ ,c(1:13)], test=test2[ ,c(1:13)], cl=train$target, k=20)
table(kn2, test$target)
mean(test2$target==kn2)

install.packages("gmodels")
library(gmodels)
CrossTable(x=kn2, y=test2[,14], prop.chisq=FALSE, dnn=c('predicted', 'actual'))






# 실험(Factor형 포함 가능?)
set.seed(10)
trainRows2 <- createDataPartition(heart_disease2$target, p=0.8, list=FALSE)
train2 <- heart_disease2[trainRows2, ]
test2 <- heart_disease2[-trainRows2, ]
str(train2)
str(test2)

nrow(train2) + nrow(test2)
nrow(heart_disease)
nrow(heart_disease.new)

kn2 = knn(train=train2[ ,c(1:13)], test=test2[ ,c(1:13)], cl=train$target, k=20)
table(kn2, test$target)
mean(test2$target==kn2)

install.packages("gmodels")
library(gmodels)
CrossTable(x=kn2, y=test2[,14], prop.chisq=FALSE, dnn=c('predicted', 'actual'))


