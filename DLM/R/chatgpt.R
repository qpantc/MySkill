# 加载所需的包
library(dlm)
library(ggplot2)

# 创建一个示例数据集
set.seed(123)
n <- 100
t <- 1:n
trend <- 0.5 * t
seasonality <- sin(2 * pi * t / 12) + cos(2 * pi * t / 12)
error <- rnorm(n, sd = 0.5)
y <- trend + seasonality + error

# 创建状态空间模型
buildModel <- function(data) {
  dlmModPoly(order = 2, dV = 0.1^2) + dlmModTrig(s = 12)
}

# 拟合模型
fit <- dlmMLE(y, parm = rep(0, 4), build = buildModel)

# 提取组件
trend_comp <- dlmFilter(y, fit$FF)
seasonality_comp <- dlmFilter(y, fit$GG)

# 绘制原始数据和分离的趋势和周期成分
plot_data <- data.frame(t = t, y = y, trend = trend_comp$m[,,1], seasonality = seasonality_comp$m[,,2])
ggplot(plot_data, aes(x = t)) +
  geom_line(aes(y = y), color = "black", linetype = "solid", size = 1) +
  geom_line(aes(y = trend), color = "blue", linetype = "dashed", size = 1) +
  geom_line(aes(y = seasonality), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Original Data with Trend and Seasonality Components",
       x = "Time",
       y = "Value") +
  theme_minimal()
