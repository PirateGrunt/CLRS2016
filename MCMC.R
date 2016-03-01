library(ggplot2)
y <- 0:9
theta = seq(0.5, 9, 0.5)

y1 <- dpois(y, theta[1])
plt <- ggplot(as.data.frame(y1), aes(y, y1)) + geom_bar(stat = "identity")
plt

y12 <- dpois(y, theta[12])
plt <- ggplot(as.data.frame(y12), aes(y, y12)) + geom_bar(stat = "identity")
plt

y18 <- dpois(y, theta[18])
plt <- ggplot(as.data.frame(y18), aes(y, y18)) + geom_bar(stat = "identity")
plt

mat <- sapply(theta, dpois, x = y)
normalize <- colSums(mat)

for (i in seq_along(normalize)){
  mat[, i] <- mat[, i] / normalize[i]
}

colSums(mat)
colnames(mat) <- theta

dfHeatMap <- tidyr
plt <- ggplot(mat, aes())

sampleY <- c(5, 8, 7, 7, 4, 2, 9, 7, 1, 2, 1, 3, 7, 0, 4, 9, 8, 9, 8, 2, 7, 7, 2, 7, 1, 5, 9, 0)
table(sampleY)

plt <- ggplot(data.frame(sampleY), aes(sampleY)) + geom_bar()
plt
