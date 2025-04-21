#make sure the code runs in a fresh session
#did i correctly compare the distributions
#should i delete percentages

################################################################################
# HW 9 R CODE
# YULIIA HELEVERIA
# MATH 240 - SPRING 2025
################################################################################

################################################################################
# Load Libraries
################################################################################
library(VGAM)

################################################################################
# QUESTION 1
################################################################################
mu0 <- 0

################################################################################
# Part a
################################################################################
alpha <- 0.05
df_20 <- 20 -1
#get the critical value for statistically discernible support
critical_20 <- qt(p = 1 - alpha, df = df_20) 

################################################################################
# Part b
################################################################################
df_30 <- 30 -1
#get the critical value for statistically discernible support
critical_30 <- qt(p = 1 - alpha, df = df_30) 

################################################################################
# Part c
################################################################################
#Conduct a simulation study to assess the Type I error 
a <- 0
b <- 4.0
n.simulations <- 1000
type1.count <- 0 #count the number of times we got Type I error

#conduct the simulation
for (i in 1:n.simulations){
  sim <- rlaplace(n = 30, location = a, scale = b)
  
  #run t-test for 20 observations
  sim20 <- sim[1:20]
  t20 <- t.test(sim20, mu = mu0, alternative = "greater")
  
  #check if we make type 1 error
  if (t20$p.value < 0.05){
    type1.count = type1.count +1
  }else{ #if p >= 0.05, try t-test on 30 observations
    t30 <- t.test(sim, mu = mu0, alternative = "greater")
    if (t30$p.value < 0.05){
      type1.count = type1.count +1
    }
  }
}

#calculate the rate of receiving Type I error 
rate.type1 <- type1.count/n.simulations

################################################################################
# QUESTION 2
################################################################################
n <-15
#calculate the true means for each of beta distributions
mean.beta.10.2 <- 10/(10+2)
mean.beta.2.10 <- 2/(2+10)
mean.beta.10.10 <- 10/(10+10)

################################################################################
# Part a
################################################################################
#left-tailed test for beta(10,2)
count.error.left.10.2 <- 0
#perform the simulation
for (i in 1:n.simulations){
  #get a sample from beta distribution
  sample <- rbeta(n, shape1 = 10, shape2 = 2)
  #perform a t-test on the sample
  t_test <- t.test(sample, alternative = "less", mu = mean.beta.10.2)
  #check if we got a type 1 error
  if (t_test$p.value < alpha){
    count.error.left.10.2 <- count.error.left.10.2 +1
  }
}
#calculate the proportion of time we make a Type 1 error
rate.left.10.2 <- count.error.left.10.2/n.simulations


#left-tailed test for beta(2,10)
count.error.left.2.10 <- 0
#perform the simulation
for (i in 1:n.simulations){
  #get a sample from beta distribution
  sample <- rbeta(n, shape1 = 2, shape2 = 10)
  #perform a t-test on the sample
  t_test <- t.test(sample, alternative = "less", mu = mean.beta.2.10)
  #check if we got a type 1 error
  if (t_test$p.value < alpha){
    count.error.left.2.10 <- count.error.left.2.10 +1
  }
}
#calculate the proportion of time we make a Type 1 error
rate.left.2.10 <- count.error.left.2.10/n.simulations


#left-tailed test for beta(10,10)
count.error.left.10.10 <- 0
#perform the simulation
for (i in 1:n.simulations){
  #get a sample from beta distribution
  sample <- rbeta(n, shape1 = 10, shape2 = 10)
  #perform a t-test on the sample
  t_test <- t.test(sample, alternative = "less", mu = mean.beta.10.10)
  #check if we got a type 1 error
  if (t_test$p.value < alpha){
    count.error.left.10.10 <- count.error.left.10.10 +1
  }
}
#calculate the proportion of time we make a Type 1 error
rate.left.10.10 <- count.error.left.10.10/n.simulations

################################################################################
# Part b
################################################################################
#right-tailed test for beta(10,2)
count.error.right.10.2 <- 0
#perform the simulation
for (i in 1:n.simulations){
  #get a sample from beta distribution
  sample <- rbeta(n, shape1 = 10, shape2 = 2)
  #perform a t-test on the sample
  t_test <- t.test(sample, alternative = "greater", mu = mean.beta.10.2)
  #check if we got a type 1 error
  if (t_test$p.value < alpha){
    count.error.right.10.2 <- count.error.right.10.2 +1
  }
}
#calculate the proportion of time we make a Type 1 error
rate.right.10.2 <- count.error.right.10.2/n.simulations


#right-tailed test for beta(2,10)
count.error.right.2.10 <- 0
#perform the simulation
for (i in 1:n.simulations){
  #get a sample from beta distribution
  sample <- rbeta(n, shape1 = 2, shape2 = 10)
  #perform a t-test on the sample
  t_test <- t.test(sample, alternative = "greater", mu = mean.beta.2.10)
  #check if we got a type 1 error
  if (t_test$p.value < alpha){
    count.error.right.2.10 <- count.error.right.2.10 +1
  }
}
#calculate the proportion of time we make a Type 1 error
rate.right.2.10 <- count.error.right.2.10/n.simulations


#right-tailed test for beta(10,10)
count.error.right.10.10 <- 0
#perform the simulation
for (i in 1:n.simulations){
  #get a sample from beta distribution
  sample <- rbeta(n, shape1 = 10, shape2 = 10)
  #perform a t-test on the sample
  t_test <- t.test(sample, alternative = "greater", mu = mean.beta.10.10)
  #check if we got a type 1 error
  if (t_test$p.value < alpha){
    count.error.right.10.10 <- count.error.right.10.10 +1
  }
}
#calculate the proportion of time we make a Type 1 error
rate.right.10.10 <- count.error.right.10.10/n.simulations

################################################################################
# Part c
################################################################################
#two-tailed test for beta(10,2)
count.error.two.10.2 <- 0
#perform the simulation
for (i in 1:n.simulations){
  #get a sample from beta distribution
  sample <- rbeta(n, shape1 = 10, shape2 = 2)
  #perform a t-test on the sample
  t_test <- t.test(sample, alternative = "two.sided", mu = mean.beta.10.2)
  #check if we got a type 1 error
  if (t_test$p.value < alpha){
    count.error.two.10.2 <- count.error.two.10.2 +1
  }
}
#calculate the proportion of time we make a Type 1 error
rate.two.10.2 <- count.error.two.10.2/n.simulations


#two-tailed test for beta(2,10)
count.error.two.2.10 <- 0
#perform the simulation
for (i in 1:n.simulations){
  #get a sample from beta distribution
  sample <- rbeta(n, shape1 = 2, shape2 = 10)
  #perform a t-test on the sample
  t_test <- t.test(sample, alternative = "two.sided", mu = mean.beta.2.10)
  #check if we got a type 1 error
  if (t_test$p.value < alpha){
    count.error.two.2.10 <- count.error.two.2.10 +1
  }
}
#calculate the proportion of time we make a Type 1 error
rate.two.2.10 <- count.error.two.2.10/n.simulations


#two-tailed test for beta(10,10)
count.error.two.10.10 <- 0
#perform the simulation
for (i in 1:n.simulations){
  #get a sample from beta distribution
  sample <- rbeta(n, shape1 = 10, shape2 = 10)
  #perform a t-test on the sample
  t_test <- t.test(sample, alternative = "two.sided", mu = mean.beta.10.10)
  #check if we got a type 1 error
  if (t_test$p.value < alpha){
    count.error.two.10.10 <- count.error.two.10.10 +1
  }
}
#calculate the proportion of time we make a Type 1 error
rate.two.10.10 <- count.error.two.10.10/n.simulations
