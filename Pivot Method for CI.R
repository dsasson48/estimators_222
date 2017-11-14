pivot_ci <- function(observations, parameter, n_size){
  
  
  dat <- data.frame(observation = seq(1,observations,1), true_parameter = parameter, n_size = rep(n_size,observations))
  
  
  for (i in 1:length(dat$observation)){
    
    dat$estimate[i] <- 1/mean(rexp(n = dat$n_size[i], rate = dat$true_parameter[i]))
    
    dat$lower[i] <- dat$estimate[i] - qgamma(0.05 / 2, shape = dat$n_size[i], rate = dat$n_size[i])*sqrt(dat$estimate[i]^2)
    
    dat$upper[i] <- dat$estimate[i] + qgamma((1 - 0.05) / 2, shape = dat$n_size[i], rate = dat$n_size[i])*sqrt(dat$estimate[i]^2)
    
    dat$hit[i] <- ifelse(dat$true_parameter[i] > dat$lower[i] & dat$true_parameter[i] < dat$upper[i], 1, 0)
  } 
  return(sum(dat$hit)/observations)
}


dat_pivot_paramter <- data.frame(parameter = seq(0.1, 10, length.out = 100))

for (i in 1:length(dat_pivot_paramter$parameter)) {
  
  dat_pivot_paramter$cov_prob[i] <- pivot_ci(1000, dat_pivot_paramter$parameter[i], 100)
}

dat_pivot_paramter %>%
  ggplot() +
  geom_point(aes(x = parameter, y = cov_prob))


dat_pivot_n <- data.frame(n_size = seq(20, 1000, 1))

for (i in 1:length(dat_pivot_n$n_size)) {
  
  dat_pivot_n$cov_prob[i] <- pivot_ci(1000, 5, n_size = dat_pivot_n$n_size[i])
}

dat_pivot_n %>%
  ggplot() +
  geom_point(aes(x = n_size, y = cov_prob))