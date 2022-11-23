################################################################################################################
############### Topic 1 - Exercise 1 - Statistical Learning ########## Juan Marcos   Arranz Díez ###############
################################################################################################################

## 5 distributions
## Cauchy, chi-square, exponential, F, Student-t


### 1. Sets of 100 observations, histogram, kernel density, and compare with real density. Repeat with 1000, 10000, and conclusions


## Cauchy 

cauchy1 <- rcauchy(100)
cauchy2 <- rcauchy(1000)
cauchy3 <- rcauchy(10000)

## The few instances that reach large values (in absolute value) do not let us appreciate the centre of the
## histogram, arguably the most interesting part. Let's restrict ourselves to that centre.

hist(cauchy1, freq=F, nclass = 150, xlim=c(-50, 50), ylim=c(0,dcauchy(0)), main="Cauchy, n=100", xlab="")
z <- seq(-100, 100, 0.01)
lines(z, dcauchy(z), col="red")
kernel <- density(cauchy1, from=-40, to=40)
lines(kernel, col="blue")
kernel_bad <- density(cauchy1)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

hist(cauchy2, freq=F, nclass = 600, xlim=c(-50, 50), ylim=c(0,dcauchy(0)), main="Cauchy, n=1,000", xlab="")
z <- seq(-100, 100, 0.01)
lines(z, dcauchy(z), col="red")
kernel <- density(cauchy2, from=-40, to=40)
lines(kernel, col="red")
kernel_bad <- density(cauchy2)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

hist(cauchy3, freq=F, nclass=6000, xlim=c(-50, 50), ylim=c(0,dcauchy(0)), main="Cauchy, n=10,000", xlab="")
z <- seq(-100, 100, 0.01)
lines(z, dcauchy(z), col="red")
kernel <- density(cauchy3, from=-40, to=40)
lines(kernel, col="purple")
kernel_bad <- density(cauchy3)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density", "Kernel density, corrected"), col=c("red", "blue", "purple"), lwd=2)



## Chi squared

chisq1 = rchisq(100, df=1)
chisq2 = rchisq(1000, df=1)
chisq3 = rchisq(10000, df=1)

hist(chisq1, freq=F, nclass = 15, main="Chi squared, n=100", xlab="")
z <- seq(min(chisq1), max(chisq1), 0.01)
lines(z, dchisq(z, df=1), col="red")
kernel_bad <- density(chisq1)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)


hist(chisq2, freq=F, nclass = 30, main="Chi squared, n=1,000", xlab="")
z <- seq(min(chisq2), max(chisq2), 0.01)
lines(z, dchisq(z, df=1), col="red")
kernel_bad <- density(chisq2)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)


hist(chisq3, freq=F, nclass = 30, main="Chi squared, n=10,000", xlab="")
z <- seq(min(chisq3), max(chisq3), 0.01)
lines(z, dchisq(z, df=1), col="red")
kernel_bad <- density(chisq3)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

## Exponential


exp1 = rexp(100, rate = 1)
exp2 = rexp(1000, rate=1)
exp3 = rexp(10000, rate=1)

hist(exp1, freq=F, nclass = 15, main="Exponential, n=100", xlab="")
z <- seq(min(exp1), max(exp1), 0.01)
lines(z, dexp(z, rate=1), col="red")
kernel_bad <- density(exp1)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

hist(exp2, freq=F, nclass = 25, main="Exponential, n=1,000", xlab="", ylim=c(0,1))
z <- seq(min(exp2), max(exp2), 0.01)
lines(z, dexp(z, rate=1), col="red")
kernel_bad <- density(exp2)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

hist(exp3, freq=F, nclass = 35, main="Exponential, n=10,000", xlab="", ylim=c(0,1))
z <- seq(min(exp3), max(exp3), 0.01)
lines(z, dexp(z, rate=1), col="red")
kernel_bad <- density(exp3)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

# F distribution

f1 = rf(100, df1=5, df2=2)
f2 = rf(1000, df1=5, df2=2)
f3 = rf(10000, df1=5, df2=2)

hist(f1, freq=F, nclass = 100, main="F, n=100", xlab="", ylim=c(0, 0.6), xlim=c(0,50))
z <- seq(min(f1), max(f1), 0.01)
lines(z, df(z, df1=5, df2=2), col="red")
kernel_bad <- density(f1)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

hist(f2, freq=F, nclass = 2500, main="F, n=1,000", xlab="", ylim=c(0, 0.6), xlim=c(0,50))
z <- seq(min(f2), max(f2), 0.01)
lines(z, df(z, df1=5, df2=2), col="red")
kernel_bad <- density(f2)
lines(kernel_bad, col="blue")
kernel <- density(f2, from=0, to=50)
lines(kernel, col="purple")
legend("topright", c("True density", "Kernel density", "Kernel density, corrected"), col=c("red", "blue", "purple"), lwd=2)

hist(f3, freq=F, nclass = 16000, main="F, n=10,000", xlab="", ylim=c(0, 0.6), xlim=c(0,50))
z <- seq(min(f3), max(f3), 0.01)
lines(z, df(z, df1=5, df2=2), col="red")
kernel_bad <- density(f3)
lines(kernel_bad, col="blue")
kernel <- density(f3, from=0, to=50)
lines(kernel, col="purple")
legend("topright", c("True density", "Kernel density", "Kernel density, corrected"), col=c("red", "blue", "purple"), lwd=2)


# Student-t

t1 = rt(100, df=5)
t2 = rt(1000, df=5)
t3 = rt(10000, df=5)

hist(t1, freq=F, nclass = 30, main="Student's t, n=100", xlab="")
z <- seq(min(t1), max(t1), 0.01)
lines(z, dt(z, df=5), col="red")
kernel_bad <- density(t1)
lines(kernel_bad, col="blue")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

hist(t2, freq=F, nclass = 30, main="Student's t, n=1,000", xlab="")
z <- seq(min(t2), max(t2), 0.01)
lines(z, dt(z, df=5), col="red")
kernel_bad <- density(t2)
lines(kernel_bad, col="blue")
#kernel <- density(f2, from=0, to=50)
#lines(kernel, col="purple")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

hist(t3, freq=F, nclass = 30, main="Student's t, n=10,000", xlab="")
z <- seq(min(t3), max(t3), 0.01)
lines(z, dt(z, df=5), col="red")
kernel_bad <- density(t3)
lines(kernel_bad, col="blue")
#kernel <- density(t3, from=0, to=50)
#lines(kernel, col="purple")
legend("topright", c("True density", "Kernel density"), col=c("red", "blue"), lwd=2)

###########################

# Cauchy
quant_cauchy <- qcauchy(c(0.25, 0.5, .75))
IQR <- quant_cauchy[3] - quant_cauchy[1]
upper_cauchy <- quant_cauchy[3] + 1.5*IQR
lower_cauchy <- quant_cauchy[1] - 1.5*IQR
upper_cauchy
lower_cauchy

prob_out_cauchy <- pcauchy(lower_cauchy) + 1 - pcauchy(upper_cauchy)
prob_out_cauchy

par(mfrow=c(1,3))
boxplot(cauchy1)
boxplot(cauchy2)
boxplot(cauchy3)

emp_quant1 <- quantile(x=cauchy1, probs=c(0.25, 0.5, 0.75))
emp_quant2 <- quantile(x=cauchy2, probs=c(0.25,.5, 0.75))
emp_quant3 <- quantile(x=cauchy3, probs=c(0.25, .5,0.75))
IQR1 <- emp_quant1[3] - emp_quant1[1]
IQR2 <- emp_quant2[3] - emp_quant2[1]
IQR3 <- emp_quant3[3] - emp_quant3[1]

emp_quant1
emp_quant2
emp_quant3
quant_cauchy
IQR1
IQR2
IQR3



sample1up <- emp_quant1[3] + 1.5*IQR1
sample1lo <- emp_quant1[1] - 1.5*IQR1
sample1up
sample1lo

sample2up <- emp_quant2[3] + 1.5*IQR2
sample2lo <- emp_quant2[1] - 1.5*IQR2
sample2up
sample2lo

sample3up <- emp_quant3[3] + 1.5*IQR3
sample3lo <- emp_quant3[1] - 1.5*IQR3
sample3up
sample3lo

emp_out_1 <- sum(cauchy1 < sample1lo | cauchy1 > sample1up) / length(cauchy1)
emp_out_2 <- sum(cauchy2 < sample2lo | cauchy2 > sample2up) / length(cauchy2)
emp_out_3 <- sum(cauchy3 < sample3lo | cauchy3 > sample3up) / length(cauchy3)
emp_out_1
emp_out_2
emp_out_3

# Chi squared

quant_chisq <- qchisq(c(0.25, 0.5, .75), df=1)
IQR <- quant_chisq[3] - quant_chisq[1]
upper_chisq <- quant_chisq[3] + 1.5*IQR
lower_chisq <- quant_chisq[1] - 1.5*IQR

prob_out_chisq <- pchisq(lower_chisq, df=1) + 1 - pchisq(upper_chisq, df=1)
prob_out_chisq


par(mfrow=c(1,3))
boxplot(chisq1)
boxplot(chisq2)
boxplot(chisq3)

emp_quant1 <- quantile(x=chisq1, probs=c(0.25, .5,0.75))
emp_quant2 <- quantile(x=chisq2, probs=c(0.25,.5, 0.75))
emp_quant3 <- quantile(x=chisq3, probs=c(0.25,.5, 0.75))
IQR1 <- emp_quant1[3] - emp_quant1[1]
IQR2 <- emp_quant2[3] - emp_quant2[1]
IQR3 <- emp_quant3[3] - emp_quant3[1]

sample1up <- emp_quant1[3] + 1.5*IQR1
sample1lo <- emp_quant1[1] - 1.5*IQR1

sample2up <- emp_quant2[3] + 1.5*IQR2
sample2lo <- emp_quant2[1] - 1.5*IQR2

sample3up <- emp_quant3[3] + 1.5*IQR3
sample3lo <- emp_quant3[1] - 1.5*IQR3

emp_out_1 <- sum(chisq1 < sample1lo | chisq1 > sample1up) / length(chisq1)
emp_out_2 <- sum(chisq2 < sample2lo | chisq2 > sample2up) / length(chisq2)
emp_out_3 <- sum(chisq3 < sample3lo | chisq3 > sample3up) / length(chisq3)
emp_out_1
emp_out_2
emp_out_3

prob_out_chisq

# Exponential

quant_exp <- qexp(c(0.25, 0.5, .75), rate=1)
IQR <- quant_exp[3] - quant_exp[1]
upper_exp <- quant_exp[3] + 1.5*IQR
lower_exp <- quant_exp[1] - 1.5*IQR

prob_out_exp <- pexp(lower_exp, rate=1) + 1 - pexp(upper_exp, rate=1)
prob_out_exp

par(mfrow=c(1,3))
boxplot(exp1)
boxplot(exp2)
boxplot(exp3)

emp_quant1 <- quantile(x=exp1, probs=c(0.25, .5,0.75))
emp_quant2 <- quantile(x=exp2, probs=c(0.25, .5,0.75))
emp_quant3 <- quantile(x=exp3, probs=c(0.25, .5,0.75))
IQR1 <- emp_quant1[3] - emp_quant1[1]
IQR2 <- emp_quant2[3] - emp_quant2[1]
IQR3 <- emp_quant3[3] - emp_quant3[1]

sample1up <- emp_quant1[3] + 1.5*IQR1
sample1lo <- emp_quant1[1] - 1.5*IQR1

sample2up <- emp_quant2[3] + 1.5*IQR2
sample2lo <- emp_quant2[1] - 1.5*IQR2

sample3up <- emp_quant3[3] + 1.5*IQR3
sample3lo <- emp_quant3[1] - 1.5*IQR3

emp_out_1 <- sum(exp1 < sample1lo | exp1 > sample1up) / length(exp1)
emp_out_2 <- sum(exp2 < sample2lo | exp2 > sample2up) / length(exp2)
emp_out_3 <- sum(exp3 < sample3lo | exp3 > sample3up) / length(exp3)
emp_out_1
emp_out_2
emp_out_3

prob_out_exp




quant_f <- qf(c(0.25, 0.5, .75), df1=5, df2=2)
IQR <- quant_f[3] - quant_f[1]
upper_f <- quant_f[3] + 1.5*IQR
lower_f <- quant_f[1] - 1.5*IQR

prob_out_f <- pf(lower_f, df1=5, df2=2) + 1 - pf(upper_f, df1=5, df2=2)
prob_out_f


par(mfrow=c(1,3))
boxplot(f1)
boxplot(f2)
boxplot(f3)

emp_quant1 <- quantile(x=f1, probs=c(0.25, .5,0.75))
emp_quant2 <- quantile(x=f2, probs=c(0.25, .5,0.75))
emp_quant3 <- quantile(x=f3, probs=c(0.25, .5,0.75))
IQR1 <- emp_quant1[3] - emp_quant1[1]
IQR2 <- emp_quant2[3] - emp_quant2[1]
IQR3 <- emp_quant3[3] - emp_quant3[1]

sample1up <- emp_quant1[3] + 1.5*IQR1
sample1lo <- emp_quant1[1] - 1.5*IQR1

sample2up <- emp_quant2[3] + 1.5*IQR2
sample2lo <- emp_quant2[1] - 1.5*IQR2

sample3up <- emp_quant3[3] + 1.5*IQR3
sample3lo <- emp_quant3[1] - 1.5*IQR3

emp_out_1 <- sum(f1 < sample1lo | f1 > sample1up) / length(f1)
emp_out_2 <- sum(f2 < sample2lo | f2 > sample2up) / length(f2)
emp_out_3 <- sum(f3 < sample3lo | f3 > sample3up) / length(f3)
emp_out_1
emp_out_2
emp_out_3

prob_out_f









quant_t <- qt(c(0.25, 0.5, .75), df=5)
IQR <- quant_t[3] - quant_t[1]
upper_t <- quant_t[3] + 1.5*IQR
lower_t <- quant_t[1] - 1.5*IQR

prob_out_t <- pt(lower_t, df=5) + 1 - pt(upper_t, df=5)
prob_out_t

par(mfrow=c(1,3))
boxplot(t1)
boxplot(t2)
boxplot(t3)

emp_quant1 <- quantile(x=t1, probs=c(0.25, .5,0.75))
emp_quant2 <- quantile(x=t2, probs=c(0.25, .5,0.75))
emp_quant3 <- quantile(x=t3, probs=c(0.25, .5,0.75))
IQR1 <- emp_quant1[3] - emp_quant1[1]
IQR2 <- emp_quant2[3] - emp_quant2[1]
IQR3 <- emp_quant3[3] - emp_quant3[1]

sample1up <- emp_quant1[3] + 1.5*IQR1
sample1lo <- emp_quant1[1] - 1.5*IQR1

sample2up <- emp_quant2[3] + 1.5*IQR2
sample2lo <- emp_quant2[1] - 1.5*IQR2

sample3up <- emp_quant3[3] + 1.5*IQR3
sample3lo <- emp_quant3[1] - 1.5*IQR3

emp_out_1 <- sum(t1 < sample1lo | t1 > sample1up) / length(t1)
emp_out_2 <- sum(t2 < sample2lo | t2 > sample2up) / length(t2)
emp_out_3 <- sum(t3 < sample3lo | t3 > sample3up) / length(t3)
emp_out_1
emp_out_2
emp_out_3

prob_out_t







