# Lecture XX Analysis of Variance

# Create a simulated dataset
trt1.mean <- 6
trt2.mean <- trt1.mean+.01
trt3.mean <- trt1.mean+.01
# play with the sd and see where the results are no longer significant.
trt1.sd <- trt2.sd <- trt3.sd <- .5

trt1 <- rnorm(20,trt1.mean,trt1.sd)
trt2 <- rnorm(20,trt2.mean,trt2.sd) + rlnorm(20,2,1)
trt3 <- rnorm(20,trt3.mean,trt3.sd) + rlnorm(20,1,1)
all <- c(trt1, trt2,trt3)

#Create a dataframe
dataset <-data.frame(treat=rep(c("Trt1","Trt2","Trt3"),each=20),x=all)
dataset
head(dataset)
str(dataset)
names(dataset)


mean(all)
mean(trt1)
mean(trt2)
mean(trt3)

tapply(dataset$x, dataset$treat, mean)

dataset$xdot <-mean(dataset$x)
dataset$xbar[1:20] <- mean(dataset$x[1:20])
dataset$xbar[21:40] <- mean(dataset$x[21:40])
dataset$xbar[41:60] <- mean(dataset$x[41:60])
obs <- sort(sample(60,10)); obs
dataset[obs,]


dataset$dev.btwn<-dataset$xbar-dataset$xdot
dataset$dev.wthn<-dataset$x-dataset$xbar
dataset$sum<-dataset$xdot+dataset$dev.btwn+dataset$dev.wthn

dataset[obs,]

sum(dataset$dev.btwn)

dataset$dev.btwn2 <- dataset$dev.btwn^2
dataset$dev.wthn2 <- dataset$dev.wthn^2

dataset[obs,]


par(mfrow=c(2,2))
boxplot(dataset$x~dataset$treat,ylab="Observations", xlab="Treatment")
plot(dataset$x,dataset$sum, xlab="Observations", ylab="Recomposed")
plot(dataset$x,dataset$dev.btwn2, xlab="Observations", ylab="Between Group Deviance Squared")
plot(dataset$x,dataset$dev.wthn2, xlab="Observations", ylab="Within Group Deviance Sqaured")

summary(aov(x~treat, dataset))
ssd.w<-sum(dataset$dev.wthn2); ssd.w
ssd.b<-sum(dataset$dev.btwn2); ssd.b

ms.w <-ssd.w/(60-3); ms.w
ms.b <-ssd.b/(3-1); ms.b

ms.b/ms.w


1-pf(ms.b/ms.w,2,57)
x <- seq(0:20)


par(mfcol=c(1,1))
hist(all,freq=F,br=14, ylim=c(0,.3))
curve(dnorm(x,mean(all),sd(all)),add=T, col="green", lwd=3)
curve(dnorm(x,mean(trt1),sd(trt1))/2,add=T, col="red", lwd=2)
curve(dnorm(x,mean(trt2),sd(trt2))/2,add=T, col="purple", lwd=2)
curve(dnorm(x,mean(trt3),sd(trt3))/2,add=T, col="orange", lwd=2)

model1.aov <- (aov(x~treat,data=dataset))
summary(model1.aov)
par(mfrow=c(2,2))
plot(model1.aov)

dataset$log_x <- log(dataset$x)
model2.aov <- aov(log_x~treat, data=dataset)
plot(model2.aov)


kruskal.test(x~treat, data=dataset)

par(mfrow=c(1,1))
plot(x, 1-pf(x,2,57), type="l")
arrows((ms.b/ms.w),.2, (ms.b/ms.w),0, col="red")
text((ms.b/ms.w)+1, .21, round(1-pf(ms.b/ms.w,1,38), 5))
summary(model1.aov)

var.between <- var(tapply(dataset$x, dataset$treat, mean))
var.within <- var(dataset$x)

args(power.anova.test)

power.anova.test(groups=3, n=20, between.var = var.between, within.var = var.within, sig.level=0.05)