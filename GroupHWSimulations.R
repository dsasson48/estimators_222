# n is sample size from exponential distribution
# replicates is number of samples from exponential distribution
# rate is exponential rate parameter

simulate_exponential_gamma <- function(n, rate, replicates){

  alpha <- c()
  beta <- c()
  for(i in 1:replicates){ 
    x <- rexp(n, rate)
    beta_i <- sum(x)
    alpha_i <- n + 1
    alpha <- c(alpha, alpha_i)
    beta <- c(beta, beta_i)
  }


  hits <- c()
  for(i in 1:replicates){
    GAMMAS <- qgamma(c(0.025,0.975), alpha[i], beta[i])
    if(rate > GAMMAS[2] || rate < GAMMAS[1]){
      hits = c(hits,0) 
    } else {
      hits = c(hits,1)
    }
  }
  
  bias <- alpha/beta - rate
  variance <- alpha/beta^2
  MSE <- variance + bias^2
  
  return(list(coverage = mean(hits), bias = bias, variance = variance, MSE = MSE))  

}  

simulations <- simulate_exponential_gamma(n = 10000, rate = 1, replicates = 1000)

mean(simulations$coverage)
mean(simulations$MSE)
mean(simulations$bias)

n <- 20:500

data <- lapply(n, simulate_exponential_gamma, rate = 5, replicates = 1000)

coverages <- c()
mean_bias <- c()
mean_variance <- c()
mean_MSE <- c()
for(i in 1:length(data)){
  coverages <- c(coverages, data[[i]]$coverage)
  mean_bias <- c(mean_bias, mean(data[[i]]$bias))
  mean_variance <- c(mean_variance, mean(data[[i]]$variance))
  mean_MSE <- c(mean_MSE, median(data[[i]]$MSE))
}


manyN <- data.frame(n, coverages, mean_bias, mean_variance, mean_MSE)
manyN %>% ggplot(aes(x = n, y = coverages)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
manyN %>% ggplot() + geom_smooth(aes(n, mean_bias), col = 'red') + 
  geom_smooth(aes(n,mean_variance), col = 'blue') + 
  geom_smooth(aes(n,mean_MSE), col = 'green')


rate <- seq(1,10,0.1)
data2 <- lapply(rate, simulate_exponential_gamma, n = 25, replicates = 1000)

coverages <- c()
mean_bias <- c()
mean_variance <- c()
mean_MSE <- c()
for(i in 1:length(data2)){
  coverages <- c(coverages, data2[[i]]$coverage)
  mean_bias <- c(mean_bias, mean(data2[[i]]$bias))
  mean_variance <- c(mean_variance, mean(data2[[i]]$variance))
  mean_MSE <- c(mean_MSE, median(data2[[i]]$MSE))
}

manyR10 <- data.frame(rate, coverages, mean_bias, mean_variance, mean_MSE)
manyR10 %>% ggplot(aes(x = rate, y = coverages)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
manyR10 %>% ggplot() + geom_smooth(aes(rate, mean_bias), col = 'red') + 
  geom_smooth(aes(rate, mean_variance), col = 'blue') + 
  geom_smooth(aes(rate, mean_MSE), col = 'green')


data3 <- lapply(rate, simulate_exponential_gamma, n = 50, replicates = 1000)

coverages <- c()
mean_bias <- c()
mean_variance <- c()
mean_MSE <- c()
for(i in 1:length(data2)){
  coverages <- c(coverages, data3[[i]]$coverage)
  mean_bias <- c(mean_bias, mean(data3[[i]]$bias))
  mean_variance <- c(mean_variance, mean(data3[[i]]$variance))
  mean_MSE <- c(mean_MSE, median(data3[[i]]$MSE))
}

manyR50 <- data.frame(rate, coverages, mean_bias, mean_variance, mean_MSE)
manyR50 %>% ggplot(aes(x = rate, y = coverages)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
manyR50 %>% ggplot() + geom_smooth(aes(rate, mean_bias), col = 'red') + 
  geom_smooth(aes(rate, mean_variance), col = 'blue') + 
  geom_smooth(aes(rate, mean_MSE), col = 'green')


data4 <- lapply(rate, simulate_exponential_gamma, n = 25, replicates = 1000)

coverages <- c()
mean_bias <- c()
mean_variance <- c()
mean_MSE <- c()
for(i in 1:length(data2)){
  coverages <- c(coverages, data4[[i]]$coverage)
  mean_bias <- c(mean_bias, mean(data4[[i]]$bias))
  mean_variance <- c(mean_variance, mean(data4[[i]]$variance))
  mean_MSE <- c(mean_MSE, median(data4[[i]]$MSE))
}

manyR25 <- data.frame(rate, coverages, mean_bias, mean_variance, mean_MSE)
manyR25 %>% ggplot(aes(x = rate, y = coverages)) + geom_point(alpha = 0.5) + geom_smooth(method = 'loess')
manyR25 %>% ggplot() + geom_smooth(aes(rate, mean_bias), col = 'red') + 
  geom_smooth(aes(rate, mean_variance), col = 'blue') + 
  geom_smooth(aes(rate, mean_MSE), col = 'green')