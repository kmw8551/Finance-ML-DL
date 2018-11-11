
#은닉 마코프 모델
```R

library('depmixS4')
library(dplyr)
#4 State Model, 1 continuous Model, 1 binary response
#ntimes is used to specify the lengths of 3 separate series

#s <- read.csv("경로")
#s <- read.csv("C:\\hynix.csv")   
#s$obv <- (OBV(s$adjusted, s$sigmaDay))
#sum(is.na(hynix))
#s$spread <- (runMean(s$revised, n=10) - runMean(s$revised, n=20))
#sum(is.na(s))
#s$rtn <-ROC(s$adjusted)
#s$class <- ifelse(s$rtn < -0.01 , 1, ifelse(s$rtn > 0.01, 4, ifelse(s$rtn > 0,3,2))) #1%
#s$class <- as.factor(s$class) 
#sum(is.na(s))\
#s <- s[20:1955,]
#hynix <- hynix[1:1955,]
#s$lrClass<-as.factor(s$Class)
#sum(is.na(s))

#은닉 마코프 추론  
hmmInference <- function(s, n,  chart=TRUE) {
  
  hmm <- depmix(s$rtn ~ 1, family = gaussian(), nstates = n, data=s) #가우시안
  #hmm <- depmix(s$rtn ~ 1, family = poisson(), nstates = n, data=s) #포아송
  hmmfit <- fit(hmm, verbose = FALSE)
  summary(hmmfit, which="transition")
  
 
  



if (chart) {
  
  # 주가 및 State 변화 차트
  prob <- posterior(hmmfit)
  print(tail(prob))
  par(mfrow=c(3,1), mar=c(2, 2, 2, 2), mgp=c(2, 0.3, 0))
  #layout(1:n)
  plot(s$adjusted, type='l', main=",수정 주가")     #선형
  plot(prob$state, type='s', main='True Regimes', xlab='', ylab='국면(Regimes)')
  matplot(prob[,-1], type='l', main='후방 국면(전환) 확률', ylab='확률') 
  legend(x='topright', paste("S", 1:n, sep=''), fill=1:n, bty='n') 
  
  # State Density
  layout(1:n) #n=3
  s$state <- prob$state
  
  for (i in 1:n) {
    # State i 에 속한 수익률
    #hynix$state <- as.integer(hynix$state)
    #test <- hynix%>% filter(hynix$state == 1)
    #rtn <- hynix$rtn %>% filter(hynix$state == 1)
    rtn <- s$rtn[which(s$state == i)]
    
    # State i 의 비율
    r <- 100 * length(rtn) / nrow(s)
    
    # State i 에 속한 수익률의 평균, 표준편차 (연간단위로 환산함)
    m <- mean(rtn) * 252 * 100 #영업일 252일로 설정 하고 100을 곱해줌 퍼센트 때문에
    sd <- sd(rtn) * sqrt(252) * 100
    
    plot(density(rtn), xlim=c(-0.1, 0.1), main=sprintf("상태 #%d (μ=%.2f, σ=%.2f, r=%.2f)", i, m, sd, r))
    abline(v=m[1]/25200, col='red') #위에서 252*100 한거 다시 나눠줌
    }
  }
}


#######################################
totalModel 에서 실행


TotalModel.R
기술적 지표는 알아서 
#######################################

#앙상블 기법
#SVM과 HMM 추가해서 모델 생성
library(depmixS4)
library(e1071)
library(foreach)
library(TTR)

source('D:\\Projects\\Workplace\\Models\\SVM1.R')
source('D:\\Projects\\Workplace\\Models\\HMM1.R')

s <- read.csv("C:/SK하이닉스.csv")
s <- s[1:1964, ]
#obv
#s$obv <- scale(OBV(s$adjusted, s$sigmaDay))

#학습 시키는 데이터 구간이 이동해야 함 (학습 구간이 너무 길면 별로임 한 한달 정도로 해봐야)
sum(is.na(s))

#모멘텀
#s$momentum <- scale(momentum(s[,"close"]))

#지수이동평균
#s$macd <-  scale(MACD(s[,"close"], nFast = 12, nSlow = 26, nSig = 9, maType = "EMA"))


#스프레드
#s$spread <- scale(runMean(s$adjusted, n=5) - runMean(s$adjusted, n=10))

#볼린저 밴드
#s$boll <- scale(BBands(HLC(s), n = 20)[, "pctB"])

#atr
#s$atr <- scale(ATR(scale(HLC(s)), n = 14)[,"atr"])

#atr관련 설명
#http://layhope.tistory.com/241

#aroon
#s$aroon <- scale(aroon(s[, c('high','low')], n = 20))

#chaikinAD
#s$clv <- scale(CLV(s[,c('high','low','close')]))

#RSI
#s$rsi <- scale(RSI(s[,'close'], n=5))

#smi
#s$smi <- scale(SMI(HLC(s), n = 13)[,"SMI"])
#s$stoch <- scale(stoch(s[,c('high','low','close')]))

#chande momentum oscilator
#s$cmo <- scale(CMO(s[,"close"]))

#adx
#s$adx <- scale(ADX(HLC(s), n = 14)[,"ADX"])

#chaikin Vol
#s$chaikin <- scale(chaikinVolatility(s[,c('high','low')]))

#SAR
#s$sar <- scale(SAR(s[, c('high','low')]))

#ADX는 DI의 단점을 보완하기 위하여 이용되는 지표로 일반적으로 DMI를 차트상으로 표시할 때,
#세 개의 선을 이용하여 기술적 분석 지표로 사용한다.
#http://blog.naver.com/PostView.nhn?blogId=zoro8055&logNo=80197779176&parentCategoryNo=&categoryNo=48&viewDate=&isShowPopularPosts=false&from=postView

#CMF
#s$cmf <- scale(CMF(s[ , c('high','low', 'close')], s[,'volume']))

#MFI
#s$mfi <- scale(MFI(s[ , c("high","low","close")], s[,"volume"]))


#ROC 1일전 종가 기준!!
s$rtn <-ROC(s$adjusted)

#MACD 20일
#s$macd <- scale(runMean(s$rtn, n = 10))

#lrClass와 roc 중복으로 인한 제외
s<-subset(s,select = -lrClass)
#log(s$adjusted/s$adjusted[-1])

s <- subset(s, select =-logreturn)
sum(is.na(s))

s$class <- ifelse(s$rtn < 0, 1 ,2)#1%


s$class <- as.factor(s$class) 

#예측을 다음날 분류를 예측해야 하므로 1개 당겨준다.
s$class[1:length(s$class)]<-s$class[-1]

s<-s[28:1957,]


sum(is.na(s))
#불필요 변수 제거 해봄(기준은 단계선택법)
s <- subset(s, select =-volume)
s <- subset(s, select =-betaYearDay)
s <- subset(s, select =-pbr)
s <- subset(s, select =-foreigners)
s <- subset(s, select =-institutional)
s <- subset(s, select =-kodexGBond3Year)
#s <- subset(s, select =-vkospi200)
s <- subset(s, select =-shanghai)
#s <- subset(s, select =-wonPerDollar)
s <- subset(s, select =-wonPerYen)
s <- subset(s, select =-goldFuture)
s <- subset(s, select =-wtiNY)
s <- subset(s, select =-kospi200IT)
s <- subset(s, select =-rtn)
#s <- subset(s, select =-kospi)
#s <- subset(s, select =-sigmaDay60)
#s <- subset(s, select =-dowjones)
#s <- subset(s, select =-volumeRTN)
#s <- subset(s, select =-pos_art_rate)
#s <- subset(s, select =-neg_art_rate)
#s <- subset(s, select =-pos_score)
#s <- subset(s, select =-score)

#나머지 변수 scale
#s$pbr <- scale(s$pbr)

#s$institutional <- scale(s$institutional)

s$vkospi200 <- scale(s$vkospi200)

#s$foreigners <- scale(s$foreigners)

#s$shanghai <- scale(s$shanghai)

#s$kospi200IT <- scale(s$kospi200IT)

#s$kodexGBond3Year <- scale(s$kodexGBond3Year)

s$sigmaDay60 <- scale(s$sigmaDay60)

#s$goldFuture <- scale(s$goldFuture)

s$kospi <- scale(s$kospi)

s$dowjones <- as.numeric(s$dowjones)

s$dowjones <- scale(s$dowjones)

#s$volume <- scale(s$volume)

s$volumeRTN <- scale(s$volumeRTN)

s$wonPerDollar <- scale(s$wonPerDollar)

#s$wtiNY <- scale(s$wtiNY)

#s$score <- scale(s$score)
#s$neg_art_rate <- scale(s$neg_art_rate)



#hynix <- hynix[1:1955,]
#s$lrClass <- as.factor(s$lrClass)
sum(is.na(s))


#n <- as.integer(nrow(s) * 0.1 )
sumAccuracy<-c()
sumDirAcc<-c()
t<-table(c(1,2),c(1,2))-table(c(1,2),c(1,2))


#서포트 벡터 머신
svmFunction(s, 0.94, 10, 5, 10)  #svmFunction(데이터 명 ,감마 값, cost값, 나누는 횟수, 반복횟수)

source('D:\\Projects\\Workplace\\Models\\HMM1.R')
#은닉 마코프 모델
#후방 상태 :  n-state model
hmmInference(s, 4 ,  chart=TRUE) #(데이터명, state 갯수, family, chart=TRUE)
#family 는 gaussian() 또는 poisson()

```
