# Rの使い方 基本編

# 変数の代入
a <- 1
print(a)

# Vectorの代入
english.test.score <- c(49,61,29,82)
math.test.score <- c(35,74,49,42)

# 個人の平均点
mean.score <- (english.test.score + math.test.score) / 2

# 全体の平均点
mean.all <- mean(mean.score)

print(mean.score)
print(mean.all)