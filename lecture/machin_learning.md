```r
#주성분 분석법
#상관관계가 있는 기존 변수를 선형결합해 분산이 극대화된 상관관계가 없는 새로운 변수로 축약하는 것.(차원 축소)
#차원 축소란 고차원의 데이터를 저차원의 데이터로 변환하는데 사용
#적은 수의 특징만으로 특정 현상을 설명할 때, 쓸데없는 공간을 줄여 준다.

#여기서 사용할 주성분분석법 PCA는 Feature Extraction이다. 

m <- prcomp(mtcars[, c(1:7, 10, 11)], center = TRUE, scale.=TRUE) 
summary(m)

str(m)

biplot(m)
#말 그대로 주성분이기에 PC1, PC2, PC3 순서대로 중요
#scale=TRUE는 정규화

install.packages('factoextra')
install.packages('devtools')
library(factoextra)
library(devtools)

# 고유벡터를 차트 그려주는 함수
fviz_eig(m)
#주성분 분석은 선형 대수에 대한 이해가 필요
#상관 계수는 벡터의 내적이다.
#선형대수에서 고유값(eigenvalue)와 고유벡터(eigenvector)
#행렬 A를 선형변환으로 봤을 때 선형변환 A에 의한 변환결과과
#자신의 상수배가 되는 0이 아닌 벡터를 고유벡터라고 함
#Ax = λx???? lambda

#간단하게 풀어보기
A = matrix(c(1,2,3,2), nrow=2, ncol=2, byrow=T)
lambda_A = eigen(A)

#손으로 계산한 것과 고유벡터 값이 다르게 보이는 이유는??

#보통 고유벡터를 표현할 때 단위 벡터로 만들어 주기 때문에
#서로 비율은 같음

# Graph of variance
fviz_pca_var(m,
             col.var = "contrib", # Color by the quality of representation . Possible values include also : "cos2", "contrib", "coord", "x" or "y".
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# 옆의 Contribution이 높으면 좋은것 여기서는 가장 진한 주황색이
# 제일 높은 기여도를 나타내고 그에 해당하는 변수는 
# hp, cyl, disp이다. 반대로 qsec은 가장 기여도가 낮다. 

#분류 모형
#의사결정 나무
install.packages("caret")
install.packages("party")
install.packages("ctree")
library(caret)


idx <- createDataPartition(iris$Species, p=0.7)
train <- iris[idx$Resample1,]
nrow(train[train$Species =="versicolor",])
test <- iris[-idx$Resample1,]
table(train$Species)

#트리 그리기
library(ctree)
treemod <- tree(Species~., data = train)
plot(treemod)
text(treemod)

#k-fold Cross Validation 을 이용하여 가치치기(pruning)
cv_tree<-cv.tree(treemod, FUN= prune.misclass)
plot(cv_tree)

#가지치기 이후의 그래프 표시
prune_tree <- prune.misclass(treemod, best=3)
plot(prune_tree)
text(prune_tree)

#Decision Tree의 또다른 패키지
install.packages('rpart')
library(rpart)

rpartmod<- rpart(Species~. , data=train, method="class")
plot(rpartmod)
text(rpartmod)
#
library(party)
partymod<- ctree(Species~., data=train)
plot(partymod)
View(iris)
################################################

#실제 금융데이터 가지고 해보기
install.packages('rpart.plot')
library(rpart.plot)
library(quantmod)

getData <- function(x, Nation) {
  if (missing(Nation)) {
    stock <- getSymbols(paste(x, ".KS", sep = ""), auto.assign = FALSE)
  } else {
    stock <- getSymbols(paste(x, ".KS", sep = ""), from=from, auto.assign = FALSE)
  }
  
  # 모든 분석은 수정 주가 (adjusted)를 이용한다
  stock <- adjustOHLC(stock, use.Adjusted=TRUE)
  
  # 거래량이 0 인 데이터는 제외한다 (공,휴일)
  stock <- stock[Vo(stock) > 0]
  
  # colume 이름을 바꾼다
  colnames(stock) <- c("open", "high", "low", "close", "volume", "adjusted")
  
  getData <- stock
}

LG_Chemistry <- getData('051910')
# SKHynix <- getData('000660')



FeatureSetTA <- function(p, tRate=0.3, cat=2) {
  
  # 일일 수익률을 계산한다
  p$rtn <-ROC(Cl(p))
  
  # 종가 기준 Spread = 5일 이동평균 - 10일 이동평균 계산
  # scale() 함수로 Z-score Normalization을 적용함
  ds <- scale(runMean(Cl(p), n=5) - runMean(Cl(p), n=10))
  colnames(ds) <- c("spread")
  
  # ATR 변동성 지표 계산
  ds$atr <- scale(ATR(scale(HLC(p)), n = 14)[,"atr"])

  # ADX 지표 계산
  ds$adx <- scale(ADX(HLC(p), n = 14)[,"ADX"])
  
  # Bollinger Band 지표 계산
  ds$boll <- scale(BBands(HLC(p), n = 20)[, "pctB"])
  
  # MACD 지표 계산
  ds$macd <- scale(MACD(Cl(p))[, 2])
  
  # OBV 지표 계산
  ds$obv <- scale(OBV(Cl(p), Vo(p)))
  
  # 5일 이동 평균 수익률 계산
  ds$martn <- scale(runMean(p$rtn, n = 5))
  
  # 익일 수익률을 표시한다. 내일의 수익률을 예측하기 위함.
  ds$frtn <- lag(p$rtn, -1)
  
  # Prediction 용 데이터
  pred <- as.data.frame(ds[nrow(ds)])
  pred$frtn <- NULL
  
  # Data Set에서 Prediction 용 데이터 제거
  ds <- ds[-nrow(ds)]
  
  # NA 제거
  ds <- na.omit(ds)
  
  # Data Set에서 frtn을 기준으로 Class를 부여함
  # s = frtn의 표준편차.
  if (cat == 3) {
    # 상승, 하락, 보합 3 가지 경우로 설정함
    # frtn이 -0.2s 이면 하락, -0.2s ~ +0.2s 이면 보합, +0.2s 이상이면 상승
    s <- sd(ds$frtn)
    ds$class <- ifelse(ds$frtn < -0.2 * s, 1, ifelse(ds$frtn > 0.2 * s, 3, 2))
  } else {
    # 상승, 하락 2 가지 경우로 설정함
    # frtn이 음수이면 하락, 양수이면 상승
    ds$class <- ifelse(ds$frtn < 0, 1, 2)
  }
  ds$frtn <- NULL
  
  # test data set 개수
  n <- as.integer(nrow(ds) * tRate)
  
  # training data set
  train <- as.data.frame(ds[1:(nrow(ds)-n),])
  
  # test set
  test <- as.data.frame(ds[(nrow(ds)-n+1):nrow(ds),])
  
  FeatureSetTA <- list("train" = train, "test" = test, "pred" = pred)
}

ds <- FeatureSetTA(LG_Chemistry)

ds$train$class <- ifelse(ds$train$class == 1, "Down", "Up")
ds$test$class <- ifelse(ds$test$class == 1, "Down", "Up")
##############################################################
#복잡도를 작게 설정해서 큰 트리를 우선 먼저 그려본다.
dt <- rpart(class ~ atr + boll + obv + martn, data = ds$train, cp=0.001)

prp(dt, type = 2, extra = 8)
#굉장히 드럽다...
#가지치기 수행

# Cross validation 오차가 최소가 되는 지점의 복잡도 (cp)를 찾는다.
# rpart 패키지는 자체로 CV 기능을 가지고 있으며, default로 10-fold CV를 수행한다.
# k-fold의 값은 rpart.control(xval = 10) 에 default로 설정되어 있음.

printcp(dt) #얘를 통해서 cp 최대 지점 찾기
pruned_data <- prune(dt, cp = 0.0031983)
prp(pruned_data, type = 2, extra = 8)


#베이즈 분류
install.packages("klaR")
library(klaR)
m <- NaiveBayes(Species~., data=train) #위에 caret 패키지로 이미 쪼갠 것
op <- par(mfrow=c(2,2))
plot(m)
par(op)
pred <- predict(m)
table(pred$class, train[,5])

print(paste(round(mean(pred$class ==train[,5])*100,2),
            "%분류 일치", sep=""))

install.packages("e1071")
library(e1071)
m <- naiveBayes(Species~., data=train)
pred <- predict(m, test[ ,-5]) #훈련데이터로 만든 예측모델에 test데이터를 넣어서 검증
table(test[,5], pred)

#KNN
library(class)
library(caret) #데이터 파티셔닝(나누기)

idx <- createDataPartition(iris$Species, p=0.7)
train <- iris[idx$Resample1,]
# nrow(train[train$Species =="versicolor",])
test <- iris[-idx$Resample1,]
table(train$Species)


pred <- knn(train[ , 1:4], test[ , 1:4], train[,5],k=3, prob=TRUE)
tmp <- table(pred, test[ , 5])
tmp
sum(diag(tmp))/sum(tmp)


#ANN 인공신경망 모형
library(nnet)
m <- nnet(Species ~., data=train, size=3)

pred <- predict(m, train, type="class")
table(pred, train[,5])

pred_1 <- predict(m, test, type="class")
table(pred, test[ , 5])
#######################################
ds$train$class <- factor(ifelse(ds$train$class == 1, "Down", "Up"))
ds$test$class <- factor(ifelse(ds$test$class == 1, "Down", "Up"))
ds$train <- na.omit(ds$train)
sum(is.na(ds$train))

ann_ds_train <- nnet(class ~., data=ds$train, size=2)
?predict
pred_ann <- predict(ann_ds_train, ds$train, type='class')
table(pred_ann, ds$train$class)
####################################

# svm
library(e1071)
m1 <- svm(Species~., data=train, kernel='linear')
m1 <- svm(Species~., data=train, kernel='polynomial')
m1 <- svm(Species~., data=train, kernel='radial')

x <- subset(test, select=-Species)
y <- test$Species

pred1 <- predict(m1, x)
pred2 <- predict(m2, x)
pred3 <- predict(m3, x)
table(y, pred1)
table(y, pred2)
table(y, pred3)
```
