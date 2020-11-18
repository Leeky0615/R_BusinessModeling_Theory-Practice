#  사전작업
install.packages("ggplot2")
library(ggplot2)

# 1번 데이터 처리
RidingMowers.df <- read.csv("RidingMowers.csv")
# 1-(1)
par(xpd=TRUE)

plot(RidingMowers.df$Lot_Size ~ RidingMowers.df$Income,
     xlab="LotSize", ylab="Income",
     col=ifelse(RidingMowers.df$Ownership == "Owner","black","red"))

legend("topright", legend = c("Owner","NonOwner"),
       col=c("black", "red"), pch=1, cex=1)

# 1) 가게소득만 보았을때는 Owner는 가게가 모두 50,000달러 이상의 소득을 낸 반면, NonOwner는 대략, 33,000달러~ 85,000달러 사이의 분포한다.
# Income :: Owner(약 $50,000 ~ ) / NonOwner(약 $33,000 ~ $85,000)

# 2) 대지면적만 보았을 때는 Owner는 모두 대략 17,000편방피트 이상의 대지를 소유한 번면, NonOwner는 모두 대략 21,000편방피트 이하의 대지면적을 소유하고 있다.
# Lot_Size :: Owner(약 17,000ft^2 ~ ) / NonOwner(약 ~ $21,000ft^2)

# 3) 전체적으로 가게소득이 높고 대지면적이 넓을수록 Owner(잠재고객)인 경우가 많고 NonOwner(비잠재고객)의 경우 가게소득이 낮고 대지면적이 좁다.


# 2번 데이터 처리
LS_Jan.df<-read.csv("LaptopSalesJanuary2008.csv")

# 2-(1)
LS_Jan_avg <- aggregate(LS_Jan.df$Retail.Price, by=list(LS_Jan.df$Store.Postcode),mean)

names(LS_Jan_avg) <- c("Store.Postcode", "AVG_Retail.Price")

ggplot(LS_Jan_avg) +
  geom_bar(aes(x=Store.Postcode, y=AVG_Retail.Price), stat="identity") +
  coord_cartesian(ylim=c(480, 500)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# 평균소매가격이 가장 높은 매장 : N17 6QA
# 평균소매가격이 가장 낮은 배장 : W4 3PH

# 2-(2)
ggplot(LS_Jan.df) +
  geom_boxplot(aes(Store.Postcode,Retail.Price)) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# 이상치를 비교했을때에 W4 3PH는 평균소매가격이 가장 낮은 매장이지만 모든 매장을 통틀어 대략적으로 가장 높은 소매가격이 있다. 또한, N17 6QA의 이상치의 개수가 더 적고 박스에 상대적으로 가까운 반면 W4 3PH는 이상치의 위치와 박스와의 거리가 멀다

# 최대값과 위치는 비슷하지만 최소값은 N17 6QA가 더 크고 1Q와 3Q가 위치하는 박스는 N17 6QA가 전체적으로 높다. 중앙값 또한 N17 6QA가 더 높다.
