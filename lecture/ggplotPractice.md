``` R


#https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf

# ggplot2 패키지 구조
# 
# 산점도의 구조(예시)
# 관측치를 geom, Scale, Coordinate system으로 표시한다
# geom : 관측치를 점으로 표현한 것
# Scale: x축과 y축의 선형 스케일을 갖는 것.
# Coordinate 직교 좌표계를 사용한다.
# 따라서 우리는 그림을 마치 계층으로 파악하는 것이 중요하다.
# 계속 위에 덧칠하는 느낌으로 접근하면 좋을 것 같다.


# ggplot2의 기본 성분
# 
# Data: 주로 데이터 프레임 객체 형태의 데이터
# Aesthetic Mappings: 데이터를 축, 색상 및 점의 크기 등으로 매핑하는 방법
# Geometric object: 점 선 도형과 같은 기하학적 객체
# Facetting: 조건부 플롯을 위해 패널을 분할하여 표현하는 방법
# Statistical transformation : 통계변환
# Scales: 데이터의 스케일을 동적으로 조정하여 어떤 시각적 요소를 사용할 것인가에 대한 정의
# Coordinate system: 좌표계
# Position adjustment: 위치의 조정

library(ggplot2)

p <- ggplot(mtcars, aes(wt, mpg, colour=cyl))
# Geometric object로 점 정의
p <- p + geom_point()
p

#geom_point() 함수

p_1 <- ggplot(mtcars, aes(wt, mpg))
p_1 + geom_point()
p_2 <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p_2 + geom_point(colour="orange" , size=6)

#geom_abline() 함수
#geom_abline() 플롯에 선을 추가하는 함수 geom_line()과는 다르다.
#일반적인 선그래프가 아닌 선형회귀에서 절편과 기울기에 의해 그려지는 geometric 을 추가하여
#회귀선이나 추세선을 그리는데 사용한다.
#따라서 intercept와 slope를 가지고 있다.

p_3 <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p_3 + geom_abline()

mtcars_coefs <- coef(lm(mpg ~ wt, mtcars))
?lm

mtcars_coefs

p_4 <- ggplot(data=mtcars , aes(x=wt, y=mpg))
p_4 <- p_4 + geom_point()
p_4 + geom_abline(intercept = mtcars_coefs["(Intercept)"],
                  slope=mtcars_coefs["wt"], colour="red")

#회귀선이나 추세선는 stat_smooth()기능으로도 가능
p_4 <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p_4 <- p_4 + geom_point()
p_4 + stat_smooth(method="lm", se=FALSE, colour="red")


#geom_bar()함수
#geom_bar()함수는 이름에서 유추할수 있듯이 막대 그래프를 플로팅 하는 함수.
#막대 그래프, 누적(stacked), 가로 방향등의 막대 그래프를 모두 그릴수 있다.

p_5 <- ggplot(data=mtcars, aes(factor(cyl)))
p_5 + geom_bar()

p_5 + geom_bar(aes(fill=cyl), colour="black")

#가로 막대 그래프
p_5 <- p_5 + geom_bar(aes(fill=factor(gear)), colour="black")
p_5 + coord_flip()

#geom_ribbon() 함수
#geom_ribbon()는 영역을 채우는 플롯을 그리는 함수
#geom_area()는 ribbon함수의 특별한 형태

library(datasets)
huron <- data.frame(year= 1875:1972, level = as.vector(LakeHuron))
p_6 <- ggplot(data=huron, aes(x=year))
p_6 <- p_6 + geom_area(aes(y=level))
p_6 + coord_cartesian(ylim=c(570,590))

library(quantmod)
getSymbols("AMZ", from=as.Date("2014-01-01"), to=as.Date("2018-06-01"))
amazon <- ggplot(AMZ, aes(x=index(AMZ), y=AMZ.Close))
amazon <- amazon + geom_ribbon(aes(min=AMZ.Low, max=AMZ.High), fill="lightgreen",
                               colour="black")
amazon <- amazon + geom_point(aes(y=AMZ.Close), colour="black", size=5)
amazon <- amazon + geom_line(aes(y=AMZ.Close), colour="blue")
amazon <- amazon + stat_smooth(method="loess", se=FALSE, colour="red", lwd=1.2)
amazon
#loess : local regression, 변동성이 존재하는 시계열을 평활화 하기 위한 방법
#se :  신뢰구간 표현할때 사용, 점 없이 선만 표시할때 F, FALSE 처리를 한다.
#lwd: 선 두께 조절

#geom_map() 함수
#지도 그리는 함수
#
install.packages("maps")
crimes <- data.frame(state =tolower(rownames(USArrests)), USArrests)
library(maps)

install.packages("mapproj")
states_map <- map_data("state")
head(states_map)

library(reshape2)
crimesm <-melt(crimes, id=1)
p <- ggplot(crimes, aes(map_id=state))
p <- p + geom_map(aes(fill=Murder), map=states_map)
p <- p + expand_limits(x=states_map$long, y=states_map$lat)

p + coord_map()

#ggplot2의 map_data()함수를 사용해도 된다.
#map:제공되는 지도 이름, 보통 world map을 가장많이 이용한다.
#region: 국가명 입력

#map_data()
#long => longitude 경도를 의미, x축 (x axis)
#lat => latitude 위도를 의미, y축 (y axis)
#group => 권역 , polygon 형태, 국가 또는 아시아 권 대륙 등 area
#order => 매우 중요, 좌표의 순번 개념
NE_Asia <- map_data(map='world', 
                    region = c('Japan', 'South Korea', 'North Korea',
                               'China', 'Mongolia'))

NE_Asia_map <- ggplot(data = NE_Asia,
                      mapping = aes(x = long,
                                    y= lat,
                                    group = group))+
              geom_polygon(fill ='lightgreen',
                           color ='black')
NE_Asia_map


#stat_density() 함수
#밀도곡선을 그리는 함수
library(datasets)
p <- ggplot(diamonds, aes(x=price))
p <- p + stat_density(aes(ymax =..density.., ymin= -..density..),
                      fill="blue", colour="black", alpha=0.50,
                      geom="area", position="identity")
p + facet_grid(.~cut)
#..density..는 주어진 데이터를 밀도로 변환하는 작업을 수행
#area 영역을 통해 플롯의 모양을 결정, 영역 플롯을 그림
#facet_grid는 플롯을 패싯별로 나누어 그리는 역할을 함

#scale_*_brewer()함수
#*에 들어갈 내용으로는 colour, color, fill이 있다.
#http://colorbrewer2.org/에 가면 원하는 색상이 있다.

p_2 <- ggplot(data=diamonds, aes(price, carat, colour=clarity))
p_2 <- p_2 + geom_point()
p_2 + scale_colour_brewer(type="seq", palette = 5)


#연습 예제

library(mapproj)
data(unemp)
data(county.fips)
#미국의 카운티 레벨의 실업률 데이터로 county를 나타내는 fips
#pop(인구 수), unemp(실업률) 정보가 있다.

colors= c("#F1EEF6","#D4B9DA","#C994C7","#DF65B0", "#DD1C77", "#980043")
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0,2,4,6,8,10,100)))
#numeric 숫자형태에서 factor로 변화

leg.txt <- c("<%2", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                    county.fips$polyname)]
colorsmatched <- unemp$colorBuckets[match(cnty.fips, unemp$fips)]

map("county", col = colors[colorsmatched], fill= TRUE, resolution=0,
    lty =0 , projection="polyconic")

#projection 이 polyconic 지도 투영 식별법

map("state", col="white", fill=FALSE, add=TRUE, lty=1, lwd=0.7,
    projection="polyconic")

title("unemployment by county, 2009")
legend("bottomright", leg.txt, fill=colors, bty="o")
#bty가 o 이면 범례 경계선이 생기고 n이면 경계선이 없음

#stat_ecdf()함수
#rnorm => 정규분포 함수 (gaussian distribution)
#rnorm(m, mean =0, sd = 1) 이 기본값 (난수 발생)
#dnorm(m, mean =0, sd = 1) 이 기본값 (확률밀도 함수, pdf)
#pnorm(m, mean =0, sd = 1) 이 기본값 (누적분포함수, cdf)

df_ecdf <- data.frame(x = c(rnorm(100,0,2), rnorm(100,0,4)),
                            g = gl(2,100))
p_ecdf <- ggplot(df_ecdf, aes(x, colour = g))

p_ecdf + stat_ecdf(geom='line', size = 2)

#gl은 factor를 생성하는 함수
#만약 랜덤하게 섞어서 데이터를 생성하고 싶을 경우
#sample 함수를 이용.
#주의할 것은 셔플한 경우, 그 값은 벡터로 리턴
#데이터 프레임으로 변환 따로 해주어야함
#shuffle_df <- df_ecdf[sample(nrow(df_ecdf)), ]
#shuffle_df <- as.data.frame(shuffle_df)
#p_ecdf <- ggplot(shuffle_df, aes(x, colour = g))
#p_ecdf + stat_ecdf(geom='line', size = 2)

#stat_function()함수
#이름에서처럼 y =f(x) 를 plotting 하는 것
#밀도 함수관련 그래프
#alpha 값을 서로 다르게 조정하여 색상의 진하기 정도를 다르게 표현

set.seed(100)
d_f <- data.frame(x = rnorm(100,0,2))
p_f <- ggplot(d_f, aes(x = x)) #난수 추출후 그래프
p_f <- p_f + geom_density(fill = 'blue', alpha =0.4)
p_f + stat_function(fun = pnorm, colour = 'red', fill='green', alpha = 0.1,
                  geom ='area') # 누적밀도함수 그래프
##############################################################################
# scale_alpha:연속형 스케일로 투명도의 범위를 조절
# scale_alpha_continuous: 연속형 스케일로 투명도의 범위를 조절
# scale_alpha_discrete : 이산화된 스케일로 투명도의 범위를 조절

p_scale <- ggplot(data=mtcars, aes(x=mpg, y=cyl, alpha = cyl ))
p_scale <- p_scale + geom_point(size=10)
p_scale + scale_alpha(range=c(0.4, 0.8))

#scale_linetype함수
#scale_linetype 계열의 함수는 선의 종류를 설정
#그냥 사용시 이산형에만 적용가능
#연속형은 scale_linetype_continuous를 사용해야함(잘 안됨, 쓰기도 힘듬)


library(reshape2)
library(plyr)
ecm <- melt(economics, id = "date")
rescale01 <- function(x) (x-min(x)) / diff(range(x)) # R에서의 함수 방식
rescale02 <- function(x) (x-mean(x)) /sd(x)

#단일 식의 경우 Java처럼 중괄호 생략 가능
#rescale01 은 정규화 공식을 사용한것
#rescale02 는 표준화 공식을 사용한것
#diff(range())는 퍼짐의 정도를 보는것
#diff는 차분 함수(현재-직전), returns suitably lagged and iterated differences
str(ecm)

ecm <- ddply(ecm, "variable", transform, value =rescale01(value))
#ddply는 데이터프레임을 입력값으로 받아서 데이터프레임 형식으로 결과를 출력

p_ecm <- ggplot(data = ecm , aes(x=date, y = value, group=variable, linetype=variable,
                                colour=variable))
p_ecm <- p_ecm + geom_line()
p_ecm + scale_linetype_discrete()
################################################################################
#시계열의 경우
p_ecm2 <- ggplot(data=economics, aes(x = date, y=uempmed))

p_ecm2 + geom_path()

############################################################################



```
