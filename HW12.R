#make sure the code runs in a fresh session
#for part 1 c, what do we use for n?

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
n.simpulations <- 1000
type1.count <- 0 #count the number of times we got Type I error

#conduct the simulation
for (i in 1:n.simpulations){
  sim <- rlaplace(n = 30, location = a, scale = b)
  #calculate the critical point
  t_sim <- mean(sim)/ (sd(sim)/sqrt(30))
  
  #check if t is larger than the critical point
  if (t_sim > critical_30){
    type1.count = type1.count + 1
  }
}

#calculate the rate of receiving Type I error 
rate.type1 <- type1.count/n.simpulations
