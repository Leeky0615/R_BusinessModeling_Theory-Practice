# 사전 작업
cereals.df<-read.csv("cereals.csv")
library(ggplot2)

# 1-(1)번
View(cereals.df)
str(cereals.df)
# Numerical Variable :: 양적/수치적 변수
#    -> calories, protein, fat, sodium, fiber, carbo, sugars, potass, weight, cups, vitamins, rating

# Categorical Variable :: 범주형 변수
#  - Nominal Variable :: 명목형 변수
#    -> name, mfr, type, shelf
#  - Ordinal Variable :: 순서형 변수
#    -> rating

# 1-(2)
# 양적 변수.
cereals.numerical.df <- (cereals.df[,-c(1:3, 13)])
str(cereals.numerical.df)

# calculate MEAN, MEDIAN, MIN, MAX, STDEV
data.frame(mean=sapply(cereals.numerical.df, mean, na.rm=TRUE),  
           median=sapply(cereals.numerical.df, median, na.rm=TRUE),
           min=sapply(cereals.numerical.df, min, na.rm=TRUE),
           max=sapply(cereals.numerical.df, max, na.rm=TRUE),
           sd=sapply(cereals.numerical.df, sd, na.rm=TRUE))


# 1-(3)
# histogram for each numerical variables
par(mfrow=c(3,4))
for(i in 1:12){
  hist(cereals.numerical.df[[i]], xlab = colnames(cereals.numerical.df)[i], main = paste('hist_',colnames(cereals.numerical.df)[i]))
}
# (a) : fiber
# -> 표준편차는 sodium이 가장 크지만 동일한 기준으로 비교하기 위해 변동계수를 통해 상쇄시켜서 판단.
# 변동 계수 : Cv = sd / mean
data.frame(mean=sapply(cereals.numerical.df, mean, na.rm=TRUE),  
           sd=sapply(cereals.numerical.df, sd, na.rm=TRUE))
CV <- sapply(na.omit(cereals.numerical.df), sd, na.rm=TRUE) / sapply(na.omit(cereals.numerical.df), mean, na.rm=TRUE)
CV

# (b) : fiber
# 왜도 : 치우쳐진 정도.
# vitamins가 가장 크지만 양적 변수라고 보기 어렵기 때문에 fiber의 치우쳐진 정도가 가장 크다.
install.packages("moments")
library(moments)
skewness(na.omit(cereals.numerical.df))

# (c) : fat, carbo, sugars 제외 모든 변수에 극단값이 존재한다.
par(mfrow=c(3,4))
for(i in 1:12){
  boxplot(cereals.numerical.df[[i]], xlab = colnames(cereals.numerical.df)[i], main = paste('boxplot_',colnames(cereals.numerical.df)[i]))
}

# 1-(4)
par(mfrow=c(1,1))
boxplot(cereals.df$calories ~ cereals.df$type, xlab = "type", ylab = "Calories")
# 저온용 시리얼은 대부분 100~110칼로리 내에 존재한다. 또한, 이상치도 존재한다.
# 고온용 시리얼은 모두 100칼로리이다.


# 1-(5)
boxplot(cereals.df$rating ~ cereals.df$shelf, xlab = "shelf", ylab = "rating")
# 1,3층 선반의 경우 평균값이 비슷하게 분포되어 있고 2층 선반의 경우 앞의 두 경우보다는 평균이 낮다.
# 세 가지 범주를 그데로 유지하는 것에 대해서는 1,3층 선반의 평균이 비슷해서 두개를 하나로 묶는다면 범주의 수가 하나 더 줄어 비교하기가 더 쉬워질 수 있다.


# 1-(6)
# 상관계수 표
cor(na.omit(cereals.numerical.df))
# 산점도 행렬
plot(na.omit(cereals.numerical.df))
# (a) : potass & fiber(0.911503921)
# (b) : 두 변수의 상관관계가 크기 때문에(중복이 많이됨) 둘 중 하나를 제거해 차원을 축소할 수 있다.
# (c) : 정규화를 통해 특정 범위에서만 나오게 한다면 상관관계의 값을 해석하는데 더욱 용이할 것이다.

# 1-(7)
# 첫 번째 주성분(principal component) ::PC1
#  -> 데이터에서 가장 큰 변동을 설명한다.
# 정규화를 하지 않았을 경우 변수들 마다 data scale이 다르기 때문에 이에 따른 표준편차의 차이도 나게 된다. 이렇게 되면 data scale이 나머지 부분을 누락시키고 대부분의 정보를 carry할 수 있기 때문에 정규화를 통해 변수들의 scale을 조정해야한다.
# cereals 데이터의 경우 나트륨의 98%의 지분을 가지고 있지만 나트륨은 단위가 mg이므로 단위가 g인 다른 변수들보다 표준편차가 크게 나타난다.
