rm(list=ls())
load("~/research-collaboration/data/logs_dom1_sept_hanke.Rdata")
library(gridExtra)
library(reshape2)
library(ggplot2)
library(dplyr)

df_reduced<-dat_dom_1%>%
  group_by(user_id)%>%
  mutate(n_ID = n())%>%
  filter(n_ID>=100)
dat_dom_1$sign <-sign(dat_dom_1$score-dat_dom_1$expected_score)
set.seed(10)

p <- list()
for (i in 1:5){
  ID <-sample(unique(df_reduced$user_id),1)
  df <-dat_dom_1[dat_dom_1$user_id %in% ID,]
  names(df)[names(df)=="new_user_domain_rating_uncertainty"] <- "Uncertainty"
  names(df)[names(df)=="user_domain_k_factor"] <- "K value"
  names(df)[names(df)=="sign"] <- "Sign(S-E(S))"
  drop <- c('Sign(S-E(S))','trial', 'Uncertainty','K value')

  p[[i]]<-df%>%
    mutate(trial=1:length(created))%>%
    select( one_of(drop))%>%
    melt(id='trial')%>%
    ggplot(aes(x = trial,y = value,color = variable, linetype=variable)) + 
    geom_line(show.legend = T) + 
    scale_linetype_manual(values=c("dotted", "solid", "solid")) + 
    scale_color_manual(values =c("black","#F8766D","#00BA38")) + 
    theme_classic()+ labs(x = "Trial", y = "Value")+
    theme(legend.position="bottom", legend.title=element_blank())
}
p

