# https://www.kaggle.com/alvarob96/spanish-stocks-historical-data

using BBVA dataset

```R
# BBVA의 데이터 셋을 이용하도록 한다.
library(quantmod)
library(rpart)
library(rpart.plot)


bbva <- read.csv("C:\\Users\\lucas\\Main\\spanish-stocks-historical-data\\bbva.csv")

FeatureSet <- function(p, test_ratio=0.3 ){
  p<- bbva
  # 일일 수익률을 계산한다
  p$return <- ROC(p$Close)
  
  # 종가 기준 Spread = 5일 이동평균 - 10일 이동평균 계산
  # scale() 함수로 Z-score Normalization을 적용함
  ds <- as.data.frame(scale(runMean(p$Close, n=5) - runMean(p$Close, n=10)))
  colnames(ds) <- c("spread")
  
  HLC_p <- p[ ,c("High", "Low", "Close")]
  
  # ADX 지표 계산
  ds$adx <- scale(ADX(HLC(p), n = 14)[,"ADX"])
  
  # Bollinger Band 지표 계산
  ds$boll <- scale(BBands(HLC_p, n = 20)[, "pctB"])
  
  # OBV 지표 계산
  ds$obv <- scale(OBV(Cl(p), Vo(p)))
  

  # 10일 이동 평균 수익률 계산
  ds$martn_10 <- scale(runMean(p$return, n = 10))
  
  # 익일 수익률을 표시한다. 내일의 수익률을 예측하기 위함.
  ds$f_return <- lag(p$return, -1)
  
  # Prediction 용 데이터
  
  pred <- as.data.frame(ds[nrow(ds), ])
  pred$f_return <- NULL
  
  # Data Set에서 예측용 데이터 제거
  ds <- ds[-nrow(ds), ]
  
  # NA 제거
  ds <- na.omit(ds)
  
  # Data Set에서 frtn을 기준으로 Class를 부여함
 
  # 상승, 하락 2 가지 경우로 설정함
  # frtn이 음수이면 하락, 양수이면 상승
  # 1이 하락, 2가 상승
  ds$class <- ifelse(ds$f_return < 0, 1, 2)
  
  # 익일 수익률 열 자체가 개입되면 문제가 생기므로 해당 열을 지움
  ds$f_return <- NULL
  
  # test data set 개수
  n <- as.integer(nrow(ds) * test_ratio)
  
  # training data set
  train <- as.data.frame(ds[1:(nrow(ds)-n),])
  
  # test set
  test <- as.data.frame(ds[(nrow(ds)-n+1):nrow(ds),])
  
  Features_dataset <- list("train" = train, "test" = test, "pred" = pred)
}
result <- FeatureSet(bbva, test_ratio= 0.3)
result$train$class <- ifelse(result$train$class == 1, "Down", "Up")
result$test$class <- ifelse(result$test$class == 1, "Down", "Up")


#복잡도를 작게 설정해서 큰 트리를 우선 먼저 그려본다.
dt <- rpart(class ~ adx + boll + obv + martn_10, data = result$train, cp=0.001)

prp(dt, type = 2, extra = 8)
#굉장히 드럽다...
#가지치기 수행

# Cross validation 오차가 최소가 되는 지점의 복잡도 (cp)를 찾는다.
# rpart 패키지는 자체로 CV 기능을 가지고 있으며, default로 10-fold CV를 수행한다.
# k-fold의 값은 rpart.control(xval = 10) 에 default로 설정되어 있음.

printcp(dt) #얘를 통해서 cp 최대 지점 찾기
pruned_data <- prune(dt, cp = 0.0032421)
prp(pruned_data, type = 2, extra = 8)


```
