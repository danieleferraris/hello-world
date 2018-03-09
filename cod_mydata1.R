
data <- mydata1[sample(1:nrow(mydata1),1000,replace=F),]
set.seed(5)
b <- data.frame(x=runif(NROW(data),0,12))
Value <- b*10
data <- cbind(data,Value)
cost <- 10

index <- sample(1:NROW(data),700,replace=F)
train <- data[index,]
test <- data[-index,]

library(rpart)

x_rp <- rpart(y~.-Value,data=train) 

pred <- predict(x_rp,type="prob",newdata=test)
pred <- as.data.frame(pred)
z <- pred[,2]*(test$x-cost)+pred[,1]*(-cost)

newdata <- cbind(pred[,2],z)
colnames(newdata) <- c('prob','EV')

test <- cbind(test,newdata)
test <- test[order(test$EV,decreasing = T),]

NROW(test)
perc <- seq(1,300)/300
test <- cbind(test, perc)
cum_EV <- cumsum(test$EV)
test <- cbind(test, cum_EV)

ggplot(data=test, aes(x=perc, y=cum_EV)) + geom_line(size=1,col="blue") 

plot(test$perc,test$cum_EV,type="l", ylim = c(-1000, 1200))
lines(test$perc,test$cum_EV,col="red")
