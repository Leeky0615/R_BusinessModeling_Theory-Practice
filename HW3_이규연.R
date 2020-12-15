install.packages("forecast")
library(forecast)
toyota.corolla.df <- read.csv("ToyotaCorolla.csv")
View(toyota.corolla.df)

# 무작위로 600개 골라낸것.
training <- sample(toyota.corolla.df$Id, 600)
# 트레이닝 데이터 셋에서 골라낸 600개는 제외
validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

# 선형모델 : lm / 우리가 예측하고자 하는 variable(price) / 1,2,8,11 컬럼 뺀 데이터(수치적이 아님)
# 
reg <- lm(Price~., data=toyota.corolla.df[,-c(1,2,8,11)], subset = training, na.action = na.exclude)

pred_t <- predict(reg, na.action = na.pass)
pred_v <- predict(reg, newdata = toyota.corolla.df[validation,-c(1,2,8,11)], na.action = na.pass)
pred_t
pred_v
accuracy(pred_t, toyota.corolla.df[training, ]$Price)

# 위랑 비교 했을 때 위에는 트레이닝 셋을 가지고 만든 선형회귀 모델을 다시 그 모델을 만드는데 사용한 트레이닝 셋에 적용을 한 것이 므로 적합성이 뛰어나다.
# 하지만 validation 데이터 셋을 넣는다면 새로뽑은 것이기 때문에 에러가 높을 수 밖에 없다.
accuracy(pred_v, toyota.corolla.df[validation,]$Price)

