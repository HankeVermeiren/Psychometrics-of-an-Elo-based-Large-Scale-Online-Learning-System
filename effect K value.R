library(patchwork)
library(dplyr)
set.seed(1)
np = 50
games <- 150
nI <- 200
ability <- rnorm(np)
difficulty <- rnorm(nI)
pp <- sample(np,1)
weights <- list(.2,.5)
store_ability <- numeric()
store_K <- numeric()

for (K in weights){
  est_ab <- rep(0, np)
  est_dif <- rep(0, nI)
  for (j in 1:games) {
    store_K <- c(store_K,K)
    store_ability <-c(store_ability, est_ab[pp])
    it <- sample((1:nI), 1, prob = abs(dnorm(est_ab[pp] - est_dif, sd = 1)))#item smpling is dependent on estimates
    outcome <- 1 * ((exp(ability[pp] - difficulty[it])/ (1+ (exp(ability[pp] -difficulty[it])))) > runif(1)) # 1 player wint; 0 item wint
    exp_outcome <-exp(est_ab[pp] - est_dif[it])/ (1+ (exp(est_ab[pp] - est_dif[it]))) # calculate estimated outcome based on rasch model
    #update estimates according to elo rule 
    est_ab[pp] <- est_ab[pp] + K * (outcome - exp_outcome)
    est_dif[it] <- est_dif[it] + K * (exp_outcome - outcome)
    
  }
}
length(store_K)
length(store_ability)

d <- data.frame(time = 1:150, K= as.character(store_K),est = store_ability)

library(ggplot2)
library(reshape2)

p <- d%>%
  ggplot(aes(x=time, y=est, colour=K, linetype = K))+
  geom_line(size= 1)+
  geom_hline(yintercept = ability[pp]) +
  labs(x = "Trial", y = "Ability estimate", colour = "K value", linetype = "K value")+
  theme_classic()

ggsave("p.png", dpi = 300)

ggplot(d, aes(x = time, y = est, colour = K, linetype = K)) +
  geom_line(size = 1, lineend = "round") +  # Gebruik 'round' voor afgeronde lijnuiteinden
  geom_hline(yintercept = ability[pp]) +
  labs(x = "Trial", y = "Ability estimate", colour = "K value", linetype = "K value") +
  theme_classic(base_size = 14) +  # Verhoog de basisgrootte voor betere leesbaarheid
  theme(legend.position = "top") 
  

