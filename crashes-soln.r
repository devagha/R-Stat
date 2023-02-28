#CODE FOR #3 dnbinom(3,size=3.2,prob=.5)
## load data
load('hwk5.Rdata')
attach(hwk5)


x=hwk5[["data2"]]

nll.lnorm=function(theta,x){
  m=theta[1]
  s2=theta[2]
  -sum(dlnorm(x,meanlog=m,sdlog=sqrt(s2),log=TRUE))
}
fit=optim(1,fn=nll.lnorm,x=x)
fit
meanlog.hat=fit$par
meanlog.hat
sdlog.hat=fit$par
sdlog.hat
fit=optim(1,fn=nll.lnorm,x=x,hessian=TRUE)
fit
I=fit$hessian
I
se.hat=1/sqrt(I)
se.hat
lambda.hat=fit$par
lambda.hat
upperboun=lambda.hat+1.96*se.hat
lowerboun=lambda.hat-1.96*se.hat
upperboun
lowerboun

c(lambda.hat-qnorm(.975)*se.hat,lambda.hat+qnorm(.975)*se.hat)

nll.lnorm=function(theta,x){
  m=theta[1]
  sd=theta[2]
  -sum(dlnorm(x,meanlog=m,sdlog=sd,log=TRUE))
}
#then you get it for sigma^2

## first make sure to set your working directory to the folder where you downloaded the data
load("crash.data.Rdata")
attach(crash.data)

summary(crash.data)
## plot the data
pairs(crash.data)

### description of data
##
## each row is a road segment in New York State
##
## Total.Crashes = total number of crashes on that road segment
##
## PCT.Miles.Div = % of the road that is a divided highway
##
## Ref.Paint = 1 if the road has reflective paint on it and =0 if not
##
## PCT.Rural = % of road that is in a rural area (instead of a city)
##
## logVMT = log(total Vehicle Miles Traveled) on the road
##        = a measure of how many cars are on the road

##
## 1. Let's model the number of crashes with an appropriate statistical model
##      - we want the statistical distribution to match the data (count data)
##      - we want the mean # of crashes to be affected by all other variables
##      - the mean needs to stay positive

Total.Crashes[i] ~ Pois(exp(b1+b2*PCT.Miles.Div+b3*Ref.Paint+b4*PCT.Rural+b5*logVMT))

##
## 2. Now, let's write a function to compute the negative-log-likelihood of the data
##    for the above model.
##

nll.crash=function(beta,Total.Crashes,PCT.Miles.Div,Ref.Paint,PCT.Rural,logVMT){
    b1=beta[1]
    b2=beta[2]
    b3=beta[3]
    b4=beta[4]
    b5=beta[5]
    -sum(dpois(Total.Crashes,lambda=exp(b1+b2*PCT.Miles.Div+b3*Ref.Paint+b4*PCT.Rural+b5*logVMT),log=TRUE))
}

## test it out
nll.crash(c(1,1,1,1,1),Total.Crashes=Total.Crashes,PCT.Miles.Div=PCT.Miles.Div,Ref.Paint=Ref.Paint,PCT.Rural=PCT.Rural,logVMT=logVMT)

##
## 3. Now let's estimate parameters and get confidence intervals for them using optim
##
##

out=optim(c(1,1,1,1,1),fn=nll.crash,Total.Crashes=Total.Crashes,PCT.Miles.Div=PCT.Miles.Div,Ref.Paint=Ref.Paint,PCT.Rural=PCT.Rural,logVMT=logVMT,hessian=TRUE)
out

## MLEs
beta.hat=out$par
## se.hats
I=out$hessian
se.hat=sqrt(diag(solve(I)))

## CI for PCT.Miles.Div
beta.hat[2]
c(beta.hat[2]-1.96*se.hat[2],beta.hat[2]+1.96*se.hat[2])

## CI for Ref.Paint
beta.hat[3]
c(beta.hat[3]-1.96*se.hat[3],beta.hat[3]+1.96*se.hat[3])

## CI for PCT.Rural
beta.hat[4]
c(beta.hat[4]-1.96*se.hat[4],beta.hat[4]+1.96*se.hat[4])

## CI for logVMT
beta.hat[5]
c(beta.hat[5]-1.96*se.hat[5],beta.hat[5]+1.96*se.hat[5])
