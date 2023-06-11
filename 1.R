install.packages("TTR")  # TTR 패키지 설치
library(TTR)  # TTR 패키지 불러오기


# 주가 데이터
prices <- df$시가

# 5일선 그리기
ma5 <- SMA(prices, n = 5)
plot(prices, type = "l", col = "blue", xlab = "일자", ylab = "주가")
lines(ma5, col = "red")

# 20일선 그리기
ma20 <- SMA(prices, n = 20)
lines(ma20, col = "green")

# 범례 추가
legend("topright", legend = c("주가", "5일선", "20일선"), col = c("blue", "red", "green"), lty = 1)
