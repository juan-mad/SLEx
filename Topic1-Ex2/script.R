library(ggplot2)
library(mvtnorm)


bivar <- function(pi, mu1, sigma1, mu2, sigma2) {
  ## pi: weight for the first population (there are only two populations!)
  ## mu1: mean vector for pop1
  ## sigma1: covariance matrix for pop1
  ## mu2: mean vector for pop2
  ## sigma2: covariance matrix for pop2
  
  # Compute the mean vector
  mu = pi * mu1 + (1-pi) * mu2
  
  # Compute the covariance matrix
  sigma = pi * (sigma1 + mu1 %*% t(mu1)) + (1-pi) * (sigma2 + mu2 %*% t(mu2)) - mu %*% t(mu)
  
  # Compute the correlation matrix
  
  rho <- sigma
  rho[1,1] <- rho[1,1] / sigma[1,1]
  rho[1,2] <- rho[1,2] / (sqrt(sigma[1,1] * sigma[2,2]))
  rho[2,1] <- rho[2,1] / (sqrt(sigma[1,1] * sigma[2,2]))
  rho[2,2] <- rho[2,2] / sigma[2,2]
  
  out <- list("mean"=mu, "cov"=sigma, "corr"=rho)
  return(out)
}

sample <- function(size, pi, mu1, sigma1, mu2, sigma2) {
  pop1 <- sum(runif(size) < pi)
  pop1 <- ceiling(pop1)
  pop2 <- size - pop1

  s1 <- rmvnorm(pop1, mean=mu1, sigma=sigma1)
  s2 <- rmvnorm(pop2, mean=mu2, sigma=sigma2)
  
  x <- c(s1[,1], s2[,1])
  y <- c(s1[,2], s2[,2])
  
  outcolor <- c(rep(1,pop1), rep(2, pop2))
  out <- data.frame(xx=x, yy=y, color=outcolor)
}



# 1.
pi <- 0.5
mu1 <- c(0,0)
mu2 <- c(3,3)
sigma1 <- matrix(data = c(1, .7, .7, 1), nrow=2, byrow=T)
sigma2 <- matrix(data = c(1, .7, .7, 1), nrow=2, byrow=T)

one <- bivar(pi, mu1, sigma1, mu2, sigma2)
one

z <- sample(1000, pi, mu1, sigma1, mu2, sigma2)
aa <- ggplot(data=z) + geom_point(aes(x=xx, y=yy, colour=factor(color))) + xlab("x") + ylab("y")
aa <- aa + ggtitle("Mixture 1") + labs(colour="Distribution")
aa

# 2.
pi <- 0.5
mu1 <- c(0,0)
mu2 <- c(0,0)
sigma1 <- matrix(data = c(1, .7, .7, 1), nrow=2, byrow=T)
sigma2 <- matrix(data = c(1, -.7, -.7, 1), nrow=2, byrow=T)

two <- bivar(pi, mu1, sigma1, mu2, sigma2)
two

z <- sample(1000, pi, mu1, sigma1, mu2, sigma2)
aa <- ggplot(data=z) + geom_point(aes(x=xx, y=yy, colour=factor(color))) + xlab("x") + ylab("y")
aa <- aa + ggtitle("Mixture 2") + labs(colour="Distribution")
aa

# 3.
pi <- 0.5
mu1 <- c(-3,3)
mu2 <- c(3,-3)
sigma1 <- matrix(data = c(1, .7, .7, 1), nrow=2, byrow=T)
sigma2 <- matrix(data = c(1, .7, .7, 1), nrow=2, byrow=T)

three <- bivar(pi, mu1, sigma1, mu2, sigma2)
three

z <- sample(1000, pi, mu1, sigma1, mu2, sigma2)
aa <- ggplot(data=z) + geom_point(aes(x=xx, y=yy, colour=factor(color))) + xlab("x") + ylab("y")
aa <- aa + ggtitle("Mixture 3") + labs(colour="Distribution")
aa

# 4.
pi <- 0.5
mu1 <- c(-3,-3)
mu2 <- c(3,3)
sigma1 <- matrix(data = c(1, -.7, -.7, 1), nrow=2, byrow=T)
sigma2 <- matrix(data = c(1, -.7, -.7, 1), nrow=2, byrow=T)

four <- bivar(pi, mu1, sigma1, mu2, sigma2)
four

z <- sample(1000, pi, mu1, sigma1, mu2, sigma2)
aa <- ggplot(data=z) + geom_point(aes(x=xx, y=yy, colour=factor(color))) + xlab("x") + ylab("y")
aa <- aa + ggtitle("Mixture 4") + labs(colour="Distribution")
aa

#####################################
#####################################
#####################################

















