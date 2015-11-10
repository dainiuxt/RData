#lapply
x <- list(a = 1:5, b = rnorm(10))
lapply(x, mean)
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))

x<- 1:10
lapply(x, runif)
lapply(x, runif, min=0, max =10)

x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))

lapply(x, function(elt) elt[,1])

sapply(x, mean)

#apply()
x <- matrix(rnorm(200), 20, 10)
apply(x, 2, mean)
apply(x, 1, sum)
apply(x, 1, quantile, probs = c(.25, .75))
a <- array(rnorm(2*2*10), c(2,2,10))
apply(a, c(1 ,2), mean)
rowMeans(a, dims = 2)

#tapply() (operating with subsets of a vector)
x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
tapply(x, f, mean)
tapply(x, f, mean, simplify=FALSE)
tapply(x, f, range)

#split takes a vector or other objects and splits it into groups
split(x, f)

lapply(split(x, f), mean)

library(datasets)
s <- split(airquality, airquality$Month)
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm=TRUE))

x <- rnorm(10)
f1 <- gl(2, 5)
f2 <- gl(5, 2)
f1
f2
interaction(f1, f2)
str(split(x, list(f1, f2)))
str(split(x, list(f1, f2), drop = TRUE))

#mapply is multiaviate apply
list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))
mapply(rep, 1:4, 4:1) #same result as line above
noise <- function(n, mean, sd) {
     rnorm(n, mean, sd)
}
noise(5, 1, 2)
noise(1:5, 1:5, 2)
mapply(noise, 1:5, 1:5, 2)
list(noise(1,1,2), noise(2,2,2), noise(3,3,2), noise(4,4,2), noise(5,5,2))


pollution <- read.csv("data/avgpm25.csv", colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)
summary(pollution$pm25)
boxplot(pollution$pm25)
boxplot(pollution$pm25, col = "blue")
boxplot(pollution$pm25, col = "red")
boxplot(pollution$pm25, col = "grey")
hist(pollution$pm25)
rug(pollution$pm25)
hist(pollution$pm25, breaks=100)
rug(pollution$pm25)
hist(pollution$pm25, breaks=1000)
hist(pollution$pm25, breaks=500)
hist(pollution$pm25, breaks=200)
boxplot(pollution$pm25, col = "blue")
abline(h=12)
hist(pollution$pm25, breaks=200)
hist(pollution$pm25, breaks=200, col ="green")
rug(pollution$pm25)
abline(v=12, lwd=2)
abline(v=median(pollution$pm25), col="magenta", lwd=2)
barplot(table(pollution$region), col="wheat", main="No of counties in each region")
boxplot(pm25 ~region, data=pollution, col="red")
par(mfrow=c(2,1), mar=c(4,4,2,1))
hist(subset(pollution, region=="east")$pm25, col="green")
hist(subset(pollution, region=="west")$pm25, col="green")
with(pollution, plot(latitude, pm25))
abline(h=12, lwd=2, lty=2)
with(pollution, plot(latitude, pm25, col=region))
abline(h=12, lwd=2, lty=2)
par(mfrow=c(1,2), mar=c(5,4,2,1))
with(subset(pollution, region=="west"), plot(latitude, pm25, main="West"))
with(subset(pollution, region=="east"), plot(latitude, pm25, main="East"))

library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

library(datasets)
data(cars)
with(cars, plot(speed, dist))

library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

library(datasets)
hist(airquality$Ozone)  ## Draw a new plot
library(datasets)
with(airquality, plot(Wind, Ozone))

library(datasets)
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

library(datasets)
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")  ## Add a title

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City",
                      type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City",
                      pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

par(mfrow = c(1, 2))
with(airquality, {
     plot(Wind, Ozone, main = "Ozone and Wind")
     plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
     plot(Wind, Ozone, main = "Ozone and Wind")
     plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
     plot(Temp, Ozone, main = "Ozone and Temperature")
     mtext("Ozone and Weather in New York City", outer = TRUE)
})

x<- rnorm(100)
hist(x)
y<-rnorm(100)
plot(x,y)
x<- rnorm(100)
hist(x)
y<-rnorm(100)
plot(x,y)
par(mar=c(2,2,2,2))
plot(x,y)
par(mar=c(4,4,2,2))
plot(x,y)
plot(x,y, pch=99)
plot(x,y, pch=20)
example(points)
x<- rnorm(100)
y<-rnorm(100)
plot(x,y, pch=20)
title("scatter")
text(-2,-2,"label")
legend("topleft", legend="Data")
legend("topleft", legend="Data", pch=20)
fit<-lm(y~x)
abline(fit)
plot(x,z, pch=20)
z<-rpois(100,2)
par(mfrow=c(2,1))
par(mfrow=c(1,1))
x<-rnorm(100)
y<- x+rnorm(100)
g<- gl(2,50)
g<- gl(2,50,labels=c("Male", "Female"))
str(g)
plot(x,y)
plot(x,y, type="n")
points(x[g=="Male"], y[g=="Male"], col="green")
points(x[g=="Female"], y[g=="Female"], col="blue", pch=19)
par(mar=c(2,2,1,1))
plot(x,y, pch=20)

pdf(file = "myplot.pdf")  ## Open PDF device; create 'myplot.pdf' in my working directory
## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")  ## Annotate plot; still nothing on screen
dev.off()  ## Close the PDF file device
## Now you can view the file 'myplot.pdf' on your computer

library(datasets)
with(faithful, plot(eruptions, waiting))  ## Create plot on screen device
title(main = "Old Faithful Geyser data")  ## Add a main title
dev.copy(png, file = "geyserplot.png")  ## Copy my plot to a PNG file
dev.off()  ## Don't forget to close the PNG device!
plot(x,z, pch=20)

