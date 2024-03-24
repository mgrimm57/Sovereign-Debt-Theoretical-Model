### packages
library("rootSolve")
library("ggplot2")




### looping over simulations

done = rep(NULL, 1000)
prob = rep(NULL, 1000)
last = rep(1, 1000)
# for resetting
g <- function(J){
  rm(list = ls())
### Set parameters
mu_h = 1
mu_l=0
q=0.5
sigma_h_lower_bar=0.5
sigma_h = sigma_h_lower_bar
sigma_l_lower_bar = 0.55

# outside option (state of the world)


# initial penalty from default
pen = 0.05

# sensitivity
alpha=0.05

# investment return
r = 0.05

# number of loops
n=50

# for for loop
A= numeric(n)
gamma = numeric(n)
x_l = numeric(n)
beta = numeric(n)
p = numeric(n)
sigma_l = numeric(n)
fail = numeric(n)

min_plus_one = numeric(n-1)
max_plus_one = numeric(n-1)
min = append(min_plus_one, 0.5, 0) 
max = append(max_plus_one, 2, 0)

mu = numeric(n-1)
mu = append(mu, mu_l, 0)
I = numeric(n)


## Loop For simulation
for (i in 2:n) {
  # establish p and beta
  mu[i] = mu_l + r * I[i] 
  p[i] = max(alpha*A[i]+q-pen,0)
  beta[i] = max((1-q)/(q)*(p[i])/(1-p[i]),0)
  
  # choice of sigma_l
  sigma = seq(sigma_l_lower_bar, 10, by = 0.01)
  likelihood = pnorm(max[i-1], mu[i], sigma) - pnorm(min[i-1], mu[i], sigma)
  choice = data.frame(sigma, likelihood)
  
  sigma_l[i]=choice[choice$likelihood==max(choice$likelihood),1]
  
  
  # determine retention regime
  f <- function (x, b) dnorm(x, mu_h, sigma_h) - (beta[i])*dnorm(x, mu_l, sigma_l[i])
  X = uniroot.all(f, lower = -10, upper = 10)
  if (is.na(X[1])){
    min[i] = 0
  }
  else if (X[1] < 0){
    min[i] = 0
    done[j] <<- i
    prob[j] <<- 0
    break
  }
  else {
    min[i]=X[1]
    prob[j] <<- 1
  }
  if (is.na(X[2])){
    max[i] = 3
  }
  else if (X[2] > 4){
    max[i] = 4
  }
  else {
    max[i]=X[2]
  }
  
  # pull for low type
  x_l[i] = rnorm(1, mu[i], sigma_l[i])
  
  # update beliefs of lending country
  if (x_l[i]<max[i] & x_l[i]>min[i]){
    A[i+1] =A[i] +1
    I[i+1]=I[i]+2
  } 
  else{
    A[i+1]=A[i]-1
    I[i+1]=I[i]-1
  }
  
}

t = c(1:n)
results = data.frame(t, x_l, p, min, max, sigma_l, mu)
last[j] <<- mu[n]


}

## looping over simulation
for (j in 1:1000) {
  g(1) 
}

# probability of not hitting lower bound
mean(prob, na.rm=TRUE)






