# 사전작업
cereals.df<-read.csv("cereals.csv")
library(ggplot2)

#1-(1)번 
#str함수와 summary함수를 사용하여 각 변수들의 data를 탐색한다. 
str(cereals.df)
summary(cereals.df)
# Numerical Variable :: 양적/수치적 변수
#  - Discrete Data
#    -> calories, protein, fat, sodium, fiber, carbo, sugars, shelf, potass, vitamins, weight, cups, rating
#  - Continous Data
#    -> calories, protein, fat, sodium, fiber, carbo, sugars, shelf, potass, vitamins, weight, cups, rating

# Categorical variable ::  data type이 chr로 나타나고 순서형변수(ordinal variable)와 명목형변수(nominal variable)로 구분된다. 
#순서형변수(ordinal variable): shelf
#명목형변수(nominal variable): name, mfr, type


#1-(2)번
#수치형변수들만을 계산하기 위해 범주형변수들을 누락시킨다.
cereal_num.df<-(cereal.df[,-c(1:3)])
#각각의 변수에 대한 평균(mean),중앙값(median),최솟값(min),최댓값(max),표준편차(sd)를 계산
data.frame(mean=sapply(cereal_num.df, mean, na.rm=TRUE),  
           median=sapply(cereal_num.df, median, na.rm=TRUE),
           min=sapply(cereal_num.df, min, na.rm=TRUE),
           max=sapply(cereal_num.df, max, na.rm=TRUE),
           sd=sapply(cereal_num.df, sd, na.rm=TRUE))


#1-(3)번
#수치형 변수 각각에 대한 히스토그램
library(ggplot2)
par(mfrow=c(4,4))
for(i in 4:16){
  hist(cereal.df[,i], main=paste(i,'-',colnames(cereal.df)[i],sep=""))
}
#요약통계량
#먼저 모든 수치형 변수들에 대하여 결측치 없는 data 추출
celeral_nona.df<-na.omit(cereal_num.df)
#분산: 각 변수의 평균값에서 얼마나 떨어져 있는가
#표준편차: 어떤 변수들의 scale이 다른 변수들보다 크면, 표준편차가 크다.
data.frame(var=sapply(celeral_nona, var, na.rm=TRUE),  
           sd=sapply(celeral_nona, sd, na.rm=TRUE))
#(a)어떤 변수의 변동이 가장 큰가? sodium=>분산과 표준편차값이 가장크다.
#(b)어떤 변수가 치우쳐 있는가? fiber => 히스토그램을 보면, 가장 0~4범위에만 데이터가 치우쳐 있음을 알 수 있다.
#boxplot을 통해 극단값 알아보기
for(i in 4:16){
  boxplot(cereal.df[,i], main=paste(i,'-',colnames(cereal.df)[i],sep=""))
}
#(c)극단값으로 보이는 값이 있는가? protein, fiber, potass, weight, cups, rating에 극단값이 존재한다.


#1-(4)번
#calories와 type변수 사용
par(mfrow=c(1,2))
boxplot(cereal.df$calories~cereal.df$type, xlab = "type", ylab = "Calories")
#type이 cold인(type=C) 저온용시리얼의 칼로리는 1사분위에서 3사분위의 값이 100~110에 위치하기 때문에 대부분 100~110에 분포하고, 최솟값은 90칼로리, 최댓값은 120칼로리이다.
#type이 hot인(type=H) 고온용시리얼은 모두 100칼로리이다.
#저온용시리얼은 변동값이 크고 극단값 또한 존재함을 알 수 있다. 


#1-(5)번
#rating과 shelf 변수 사용
par(mfrow=c(1,2))
boxplot(cereal.df$rating~cereal.df$shelf, xlab = "shelf", ylab = "rating")
#1번선반과 3번선반에 위치한 시리얼들의 평점은 비슷하고, 2번 선반에 위치한 시리얼들의 평점은 1번과 3번에 위치한 시리얼들에 비해 낮다.따라서, 진열대 높이로부터 소비자 평점을 예측한다면, 1번선반과 3번선반을 하나로 통합하는 차원축소를 이루어(Reducing Categories) 변수의 개수를 줄이는 것이 좋다. 세가지 범주를 그대로 유지할 필요가 없다. 



#1-(6)번
#상관계수표를 나타내기 위해 먼저, 모든 변수에 결측치 없는 데이터 추출하기
celeral_nona<-na.omit(cereal_num.df)
celeral_nona
#양적변수에 대한 상관계수 표: 수치형변수들만 있는 데이터를 사용하고, 소수점 2째자리까지만 나타내준다.
round(cor(celeral_nona),2)
#전체 변수에 대한 산점도 행렬
plot(celeral_nona)
#(a)어느 변수 쌍이 가장 높은 상관관계를 나타내는가?: fiber와 potass가 0.91로 가장 높은 상관관계를 나타낸다.
#(b)이들 상관관계를 바탕으로 어떻게 변수의 개수를 축소할 수 있는가?: fiber와 potass가 높은 양의 상관관계를 가지므로 둘중 하나의 변수 제거를 통해 변수 개수를 축소할 수 있다.
pcs.cor<-prcomp(celeral_nona,scale.=T)
summary(pcs.cor)
#(c)데이터를 먼저 정규화하였다면 상관관계가 어떻게 변하겠는가?: 상관관계가 더 높아질 것이다.


#1-(7)번
#13개의 수치형 변수에 대한 첫 번째 주성분(principal component): 
#정규화를 하지않은 아침식사용시리얼 데이터의 13개 수치형 변수를 가지고 주성분분석(PCA)을 한 결과이다.정규화를 하지 않았을때는 3개의 변수만 사용해서,즉 3개의 주성분만 있어도 97%가 되기때문에 변수 10개를 제거해도된다. 하지만, 정규화를 한 이후에는 총변동의 90%이상이 되기 위해서는 7개의 주성분이 필요하다. 정규화를 하지 않았을때는, 어떤 변수들의 data들의 scale이 다른 변수들의 값보다 커버리면, 그것들의 표준편차도 커지게 되므로 그들이 대부분의 information을 캐리한다고 착각할 수가 있다. 따라서, mg로 측정된 칼륨과 나트륨의 분산이 g으로 측정된 다른 변수들의 분산보다 훨씬 크게 나타나는 왜곡이 발생하였다. 따라서, 정규화 과정을 통해 같은 단위로 바꿔서 왜곡이 발생하지 않도록 해야한다. 