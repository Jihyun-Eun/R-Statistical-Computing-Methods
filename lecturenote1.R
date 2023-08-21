### Slide 1 ###

arith_mean <- function(x) {
  s <- 0
  n <- length(x)
  for(i in 1:n)
    s <- s + x[i]
  return(s/n)
}
x <- 0.2:5.9; arith_mean(x); sum(x)/length(x); mean(x)

### Slide 2 ###

n <-10^4; x <- seq(0,1,length=n); mu_vec1 <- numeric(n)
for (i in 1:n) mu_vec1[i] <- mean(x[1:i])
mu_vec2 <-numeric(n)
for (i in 1:n) mu_vec2[i] <- arith_mean(x[1:i])
mu_vec3<-numeric(n); mu_vec3[1] <- x[1]
for (i in 1:(n-1)) mu_vec3[i+1] <- (i*mu_vec3[i] + x[i+1])/(i+1)
mu_vec1[15]; mu_vec2[15]; mu_vec3[15]
mu_vec4 <- cumsum(x)/(1:n)
mu_vec1[15]; mu_vec2[15]; mu_vec3[15]; mu_vec4[15]

### Slide 3 ###

Fibo_func1 <- function(n, x1, x2){
  temp.1 <- x1
  temp.2 <- x2
  for (k in 3:n){
    temp.3 <- temp.1 + temp.2
    temp.1 <- temp.2
    temp.2 <- temp.3
  }
  return(temp.3)
}
Fibo_func1(3,1,1); Fibo_func1(10,1,1); Fibo_func1(20,1,1)

phi=(1+sqrt(5))/2
n=1:10; Fn=(phi^n-(-phi)^(-n))/sqrt(5)
print(Fn)

### Slide 4 ###

n <- 20; Fn <- numeric(n)
Fn[1] <- 1; Fn[2] <- 1
for (i in 3:n){
  Fn[i] = Fn[i-1] + Fn[i-2]}

Fn <- c(1,1); i <- 2
while (Fn[i] <= 100) {
  i <- i + 1; 
  Fn[i] <- Fn[i-1] + Fn[i-2]}

Fibo_func2 = function(n) {
  if (n==1 | n==2) return(1)
  else return (Fibo_func2(n-1)+Fibo_func2(n-2))
}
Fibo_func2(30)

### Slide 5 ###

n <- 1000; F <- rep(0,n)
F[1] <- 1; F[2] <- 1
for (k in 3:n) F[k] <- F[k-2]+ F[k-1]
Fibo_benford <- table(F %/% 10^floor(log(F,10)))/n
exact_benford <- round(log(2:10,10) - log(1:9,10),3)
benford <- rbind(Fibo_benford,exact_benford)
colnames(benford) <- 1:9; rownames(benford) <- c("Fibonacci","Exact")
benford

### Slide 5 ###

n <- 6
n_factorial <- 1
for (i in 1:n) {
  n_factorial <- n_factorial * i
}
n_factorial

prod(1:n)

nfact=numeric(n)
nfact[1]=1
for (i in 1:(n-1)) {
  nfact[i+1]=nfact[i]*(i+1)
}
nfact

factorial(n); factorial(1:n)

### Slide 7 ###

n <- 23; a <- 1
for (k in 1:n) a<-a*(365-k+1)/365
1-a

1-prod((365-n+1):365)/365^n

pbday <- function (n) {
  1- factorial(365)/(factorial(365-n)*365^n)
}
pbday(n)
factorial(365)
factorial(365-n)

### Slide 8 ###

pbday <- function(n){
  1-exp(lfactorial(365)-lfactorial(365-n)-n*log(365))
}
pbday(n)
pbday(20:30)
nstudents <- 2:50; prob_birthday <- pbday(nstudents)
rbind(nstudents,prob_birthday);data.frame(nstudents,prob_birthday)
plot(1:100,pbday(1:100),xlab='Number of students', ylab=expression(P(A)),
     col="red",lwd=2,type="l")

pbirthday(50)
qbirthday(0.97)

### Slide 9 ###

# Exact result
n = 1:100
plot(n,exp(lgamma(366) - lgamma(366-n) - n*log(365)),ylab="P(no match)",
     pch=19,cex=0.5)
abline(h=0.5,col="blue")

# Simulated Result - no coincidence birthday
n = 25; exp(lgamma(366) - lgamma(366-n) - n*log(365))

iter <- 10^4; count <- numeric(iter)
for (i in 1:iter) {
  bdays = sample(1:365,n,replace=TRUE)
  count[i] <- (length(bdays) == length(unique(bdays)))}
mean(count)

### Slide 11 ###

bdays <- sample(1:365,23,replace=TRUE);
bdays
tabulate(bdays)
coincidebdays <- replicate(10^4,max(tabulate(sample(1:365,23,replace=TRUE))))
sum(coincidebdays >= 2)/10^4

nocoincide <- function(n){
  bdays <- sample(1:365,n,replace=TRUE)
  val <- (length(bdays) == length(unique(bdays)))
  return(val)}
count <- replicate(10^4, nocoincide(25)); mean(count)

rolling_dice <- function(d){
  sample(1:6, size = d, replace = TRUE)
}
tworolls <- replicate(10^4, rolling_dice(2))
mean(tworolls[1,]==tworolls[2,])

### Slide 12 ###

Frequency <- c(0.0026123,0.0026785,0.0026838,0.0026426,0.0026702,0.0027424,
               0.0028655,0.0028954,0.0029407,0.0027705,0.0026842,0.0026864)
par(mar=c(4,6,4,4))
plot(1:12,Frequency,type="n",xaxt="n",yaxt="n",
     xlab="",ylab="",ylim=c(0.0025,0.0030))
points(1:12,Frequency,pch=16)
lines(1:12,Frequency)
abline(h=mean(Frequency),lty=2)
axis(1,at=c(1,4,7,10,12),labels=c("JAN","APR","JUL","OCT","DEC"))
axis(2,las=2)
title(xlab="MONTH",mgp=c(2,0,0))
title(ylab="FREQUENCY",mgp=c(4,0,0))

### Slide 13 ###

wts=c(rep(0.0026123,31),rep(0.0026785,28),rep(0.0026838,31),rep(0.0026426,30),
      rep(0.0026702,31),rep(0.0027424,30),rep(0.0028655,31),rep(0.0028954,31),
      rep(0.0029407,30),rep(0.0027705,31),rep(0.0026842,30),rep(0.0026864,31))
nocoincidewt <- function(n){
  bdays <- sample(1:365,n,replace=TRUE,prob=wts)
  val <- (length(bdays) == length(unique(bdays)))
  return(val)}
count <- replicate(10^4,nocoincidewt(23)); mean(count
