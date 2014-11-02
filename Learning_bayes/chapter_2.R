library(LearnBayes)
p <- seq(0.05, 0.95, by = 0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
plot(p, prior, type = "h", ylab="Prior Probability")


data = c(11, 16)
post = pdisc(p, prior, data)
round(cbind(p, prior, post),2)


library(lattice)
PRIOR <- data.frame("prior",p,prior)
POST <- data.frame("posterior",p,post)
names(PRIOR) <- c("Type","P","Probability")
names(POST) <- c("Type","P","Probability")
data <- rbind(PRIOR,POST)
xyplot(Probability~P|Type,data=data,layout=c(1,2),type="h",lwd=3,col="black")


quantile_1 <- list(p = 0.9, x=0.5)
quantile_2 <- list(p = 0.5, x=0.3)

beta_result <- beta.select(quantile1 = quantile_1,quantile2 = quantile_2)
curve(dbeta(x,beta_result[[1]]+11,beta_result[[2]]+16),from=0, to=1,xlab="p",ylab="Density",lty=1,lwd=4)
curve(dbeta(x,11+1,16+1),add=TRUE,lty=2,lwd=4)
curve(dbeta(x,beta_result[[1]],beta_result[[2]]),add=TRUE,lty=3,lwd=4)
legend(.7,4,c("Prior","Likelihood","Posterior"),lty=c(3,2,1),lwd=c(3,3,3))