housing.df <- read.csv("BostonHousing.csv")

# Rescaling to log scale
# 너무 많은 데이터포인트가 x축에 과민하게 압축이 되어있다.
plot(housing.df$MEDV ~housing.df$CRIM, xlab = "CRIM", ylab = "MEDV")
