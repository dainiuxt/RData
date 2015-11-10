library("lattice")
xyplot(y~x | QOM * CumOil, monthly)
xyplot(QOM ~ CumOil, monthly)
xyplot(QOM ~ WCT, monthly)
xyplot(QOM ~ CumOil, monthly)
xyplot(WCT ~ CumOil, monthly)
xyplot(CumOil ~ WCT, monthly)
xyplot(CumOil ~ WCT | Well_N, monthly)
library("sqldf")
NSD.sub <- sqldf(select * from monthly where Well_N like NSD)
NSD.sub <- sqldf("select * from monthly where Well_N like NSD")
NSD.sub <- sqldf("select * from monthly where Well_N like 'NSD%'")
xyplot(CumOil ~ WCT | Well_N, NSD.sub)
xyplot(WCT ~ CumOil | Well_N, NSD.sub)
KRT.sub <- sqldf("select * from monthly where Well_N like 'KRT%'")
library(datasets)
data(airquality)
airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)
library(ggplot)
library(ggplot2)
airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)
qplot(Wind, Ozone, data = airquality, facets = . ~ factor(Month))
qplot(Wind, Ozone, data = airquality)
qplot(Wind, Ozone, data = airquality, geom = "smooth")
g <- ggplot(movies, aes(votes, rating))
print(g)
qplot(votes, rating, data = movies)
qplot(votes, rating, data = movies) + geom_smooth()
qplot(votes, rating, data = movies, panel = panel.loess)
qplot(votes, rating, data = movies, smooth = "loess")
qplot(votes, rating, data = movies) + stats_smooth("loess")
xyplot(CumOil ~ WCT | Well_N, monthly)
library(lattice)
xyplot(CumOil ~ WCT | Well_N, monthly)
xyplot(WCT ~ CumOil | Well_N, NSD.sub)
library(ggplot2)
str(mpg)
qplot(displ, hwy data= mpg)
qplot(displ, hwy, data= mpg)
qplot(displ, hwy, data= mpg, color=drv)
qplot(displ, hwy, data=mpg, geom=c("point", "smooth"))
qplot(hwy, data=mpg, fill=drv)
qplot(displ, hwy, data=mpg, facets= .~drv)
qplot(hwy, data=mpg, facets= drv~., binwidth=2)

testdat<-data.frame(x=1:100,y=rnorm(100))
testdat[50,2] <- 100
plot(testdat$x, testdat$y, type="1", ylim=c(-3,3))
plot(testdat$x, testdat$y, ylim=c(-3,3))
plot(testdat$x, testdat$y, type="line", ylim=c(-3,3))
g<-ggplot(testdat, aes(x=x, y=y))
g<-ggplot(testdat, aes(x=x, y=y))
g+geom_line()

## Setup ggplot with data frame
     g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
## Add layers
     g + geom_point(alpha = 1/3)
     + facet_wrap(bmicat ~ no2dec, nrow = 2, ncol = 4)
     + geom_smooth(method="lm", se=FALSE, col="steelblue")
     + theme_bw(base_family = "Avenir", base_size = 10)
     + labs(x = expression("log " * PM[2.5])
     + labs(y = "Nocturnal Symptoms")
     + labs(title = "MAACS Cohort")

GRK.sub <- sqldf("select * from monthly where Well_N like 'GRK%'")
xyplot(WCT ~ CumOil | Well_N, GRK.sub)
LIZ.sub <- sqldf("select * from monthly where Well_N like 'LIZ%'")
xyplot(WCT ~ CumOil | Well_N, LIZ.sub)
ABL.sub <- sqldf("select * from monthly where Well_N like 'ABL%'")
xyplot(WCT ~ CumOil | Well_N, ABL.sub)
library(nlme)
xyplot(weight ~ Time | Diet, BodyWeight)
library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)
p
xyplot(WCT ~ CumF | Well_N, NSD.sub)

set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

dataFrame <- data.frame(x = x, y = y)
dist(dataFrame)

dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)

myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)),
                      hang = 0.1, ...) {
     ## modifiction of plclust for plotting hclust objects *in colour*!  Copyright
     ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
     ## of labels of the leaves of the tree lab.col: colour for the labels;
     ## NA=default device foreground colour hang: as in hclust & plclust Side
     ## effect: A display of hierarchical cluster with coloured leaf labels.
     y <- rep(hclust$height, 2)
     x <- as.numeric(hclust$merge)
     y <- y[which(x < 0)]
     x <- x[which(x < 0)]
     x <- abs(x)
     y <- y[order(x)]
     x <- x[order(x)]
     plot(hclust, labels = FALSE, hang = hang, ...)
     text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
          col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}

dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)


set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)
kmeansObj$cluster

par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")

set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

par(mar = rep(0.2, 4))
heatmap(dataMatrix)

set.seed(678910)
for (i in 1:40) {
     # flip a coin
     coinFlip <- rbinom(1, size = 1, prob = 0.5)
     # if coin is heads add a common pattern to that row
     if (coinFlip) {
          dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
     }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

par(mar = rep(0.2, 4))
heatmap(dataMatrix)

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector",
     pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)

svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1",
     ylab = "Right Singular Vector 1")
abline(c(0, 1))

constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)

set.seed(678910)
for (i in 1:40) {
     # flip a coin
     coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
     coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
     # if coin is heads add a common pattern to that row
     if (coinFlip1) {
          dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
     }
     if (coinFlip2) {
          dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
     }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained",  pch = 19)

dataMatrix2 <- dataMatrixOrdered
## Randomly insert some missing data
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2))  ## Doesn't work!

library(impute)  ## Available from http://bioconductor.org
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=FALSE)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2)); plot(svd1$v[,1],pch=19); plot(svd2$v[,1],pch=19)

load("data/face.rda")
image(t(faceData)[, nrow(faceData):1])

x<- c(rnorm(10), runif(10), rnorm(10,1))
f <- gl(3,10)
f
tapply(x, f, mean)
tapply(x,f,mean,simplify=FALSE)
tapply(x,f,range)
split(x, f)
lapply(split(x, f), mean)
library(datasets)
head(airquality)
s <- split(airquality, airquality$Month)
lapply(s, function(x), colMeans(x[,c("Ozone", "Solar.R", "Wind")]))
s <- split(airquality, airquality$Month)
lapply(s, function(x) colMeans(x[,c("Ozone", "Solar.R", "Wind")]))
sapply(s, funxtion(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm=TRUE))
x <- rnorm(10)
f1 <- gl(2, 5)
f2 <- gl(5, 2)
f1
f2
interaction(f1, f2)
str(split(x, list(f1, f2)))
str(split(x, list(f1, f2), drop = TRUE))
list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))
mapply(rep, 1:4, 4:1)
noise <- function(n, mean, sd) {
     rnorm(n, mean, sd)
}
noise(5, 1, 2)
noise(1:5, 1:5, 2)
mapply(noise, 1:5, 1:5, 2)
list(noise(1,1,2), noise(2,2,2), noise(3,3,2), noise(4,4,2), noise(5,5,2))
x <- invisible(10)
x
traceback
traceback()
x <- ln(-5)
x <- log(-5)
x <- ln(-5)
traceback()
traceback(x)
lm(y-x)
traceback()
debug(lm)
lm(y-x)
??str
structure(1:6, dim = 2:3)
structure(f1)
structure(f2)
structure(str)
str(str)
str(f1)
str(f2)
str(structure)
summary(str)
summary(f1)
str(f1)
library(datasets)
head(airquality)
str(airquality)
m <- matrix(rnorm(100), 10, 10)
str(m)
m[, 1]
s <- split(airquality, airquality$Month)
str(s)
x<- rnorm(10)
x
x<- rnorm()
x<- rnorm(100)
x
set.seed(1)
rnorm(5)
rnorm(5)
set.seed(1)
set.seed(1)
rnorm(5)
set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 +2 * x + e
y
plot(x,y)
set.seed(10)
x <- rbinom(100, 1, 0.5)
e <- rnorm(100, 0, 2)
y <- 0.5 +2 * x + e
plot(x,y)
set.seed(1)
x <- rnorm(100)
log.mu <- 0.5+0.3*x
y <- rpos(100, exp(log.mu))
y <- rpois(100, exp(log.mu))
plot(x,y)
set.seed(1)
sample(1:10, 4)
sample(1:10, 4)
sample(letters, 5)
sample(1:10)
sample(1:10, replace = TRUE)
sample(1:10)
sample(1:10)
sample(1:10)
system.time()
system.time(library(datasets)
            s <- split(airquality, airquality$Month)
            lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
            sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm=TRUE)))
system.time(noise)
hilbert <- function(n) {+}
hilbert <- function(n) {
     i<-1:n
     1 / outer(i-1, i, "+")}
hilbert <- function(n) {
     i<-1:n
     1 / outer(i-1, i, "+")}
x<- hilbert(1000)
system.time(svd(x))
system.time({
     n<-1000
     r<-numeric(n)
     for(i in 1:n) {
          x <- rnorm(n)
          r[i]<- mean(x)
     }
})
system.time({
     n<-10000
     r<-numeric(n)
     for(i in 1:n) {
          x <- rnorm(n)
          r[i]<- mean(x)
     }
})
Rprof({
     n<-10000
     r<-numeric(n)
     for(i in 1:n) {
          x <- rnorm(n)
          r[i]<- mean(x)
     }
})
sample.interval=10000
lm(y~x)
sample.interval=10000
Rprof()
summaryRprof()
summaryRprof({
     n<-10000
     r<-numeric(n)
     for(i in 1:n) {
          x <- rnorm(n)
          r[i]<- mean(x)
     }
})
$by.total
summaryRprof()
Rprof()
x<- hilbert(10000)
x<- hilbert(1000)
summaryRprof()
Rprof()
summaryRprof()
set.seed(1)
rpois(5, 2)
set.seed(10)
x <- rbinom(10, 10, 0.5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
plot(x,y)
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)
Rprof(NULL)
