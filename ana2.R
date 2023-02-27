# データファイルの読み込み
data = read.csv("c01.csv", header = TRUE, fileEncoding = "utf-8")

# データ抽出
data = data[, c("都道府県名", "西暦年", "人口総数", "人口男", "人口女")]
data = data[data$都道府県名 == "全国", ]
x = data$"西暦年"
# 文字列型から整数型へ変換
y1 = as.integer(data$"人口総数")
y2 = as.integer(data$"人口男")
y3 = as.integer(data$"人口女")
# 標準化
#x = scale(x)
#y1 = scale(y1)
#y2 = scale(y2)
#y3 = scale(y3)

# 多項式関数で非線形近似
result1 = nls(y1~a*x^2+b*x+c, start=c(a=-1,b=1,c=1))
result2 = nls(y2~a*x^2+b*x+c, start=c(a=-1,b=1,c=1))
result3 = nls(y3~a*x^2+b*x+c, start=c(a=-1,b=1,c=1))
print(summary(result1))
print(summary(result2))
print(summary(result3))

# データプロット
par(mfrow = c(2, 2))
# 左上
plot(data[,c("西暦年")], data[,c("人口総数")], type="p", col=1, lty=1, xlab="year", ylab="population of sum", xlim=c(1910, 2020), ylim=c(2e7, 2e8), main="population transition")
lines(data[,c("西暦年")], fitted(result1), col=2)
legend("topleft", legend=c("row data", "quadratic"), pch=c(1,-1), lty=c(0,1), col=c("black", "red"))
# 右上
plot(data[,c("西暦年")], data[,c("人口男")], type="p", col=1, lty=1, xlab="year", ylab="population of men", xlim=c(1910, 2020), ylim=c(2e7, 2e8), main="population transition")
lines(data[,c("西暦年")], fitted(result2), col=2)
legend("topleft", legend=c("row data", "quadratic"), pch=c(1,-1), lty=c(0,1), col=c("black", "red"))
# 左下
plot(data[,c("西暦年")], data[,c("人口女")], type="p", col=1, lty=1, xlab="year", ylab="population of women", xlim=c(1910, 2020), ylim=c(2e7, 2e8), main="population transition")
lines(data[,c("西暦年")], fitted(result3), col=2)
legend("topleft", legend=c("row data", "quadratic"), pch=c(1,-1), lty=c(0,1), col=c("black", "red"))