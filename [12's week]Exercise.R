housing.df <- read.csv("BostonHousing.csv")
library(ggplot2)

# Rescaling to log scale
# 너무 많은 데이터포인트가 x축에 과민하게 압축이 되어있다.
# 축에 모여 있는 점들을 좀 퍼트려 놓을때 사용됨.
# 어떤 패턴을 직선으로 찾아 내려는 것 -> 선형회귀분석
plot(housing.df$MEDV ~housing.df$CRIM, xlab = "CRIM", ylab = "MEDV")
plot(housing.df$MEDV ~housing.df$CRIM, xlab = "CRIM", ylab = "MEDV", log="xy")

boxplot(housing.df$CRIM ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "CRIM")
boxplot(housing.df$CRIM ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "CRIM",log="y")
