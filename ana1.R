# データファイルの読み込み
data = read.csv("c01.csv", header = TRUE, fileEncoding = "utf-8")

# データ抽出
data = data[, c("都道府県名", "西暦年", "人口総数", "人口男", "人口女")]
data = data[data$都道府県名 == "全国", ]
#x = data$"西暦年"
#y1 = data$"人口総数"

# 最小二乗法による線形近似
result1 = lm(data[, c("人口総数")] ~ data[, c("西暦年")], data = data)
result2 = lm(data[, c("人口男")] ~ data[, c("西暦年")], data = data)
result3 = lm(data[, c("人口女")] ~ data[, c("西暦年")], data = data)
#print(summary(result1))
#print(summary(result2))
#print(summary(result3))

# データプロット
par(mfrow = c(2, 2))
# 左上
plot(data[,c("西暦年")], data[,c("人口総数")], type="p", col=1, lty=1, xlab="year", ylab="population of sum", xlim=c(1910, 2020), ylim=c(2e7, 2e8), main="population transition")
lines(data[,c("西暦年")], fitted(result1), col=2)
legend("topleft", legend=c("row data", "linear line"), pch=c(1,-1), lty=c(0,1), col=c("black", "red"))
# 右上
plot(data[,c("西暦年")], data[,c("人口男")], type="p", col=1, lty=1, xlab="year", ylab="population of men", xlim=c(1910, 2020), ylim=c(2e7, 2e8), main="population transition")
lines(data[,c("西暦年")], fitted(result2), col=2)
legend("topleft", legend=c("row data", "linear line"), pch=c(1,-1), lty=c(0,1), col=c("black", "red"))
# 左下
plot(data[,c("西暦年")], data[,c("人口女")], type="p", col=1, lty=1, xlab="year", ylab="population of women", xlim=c(1910, 2020), ylim=c(2e7, 2e8), main="population transition")
lines(data[,c("西暦年")], fitted(result3), col=2)
legend("topleft", legend=c("row data", "linear line"), pch=c(1,-1), lty=c(0,1), col=c("black", "red"))