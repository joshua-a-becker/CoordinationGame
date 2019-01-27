rm(list=ls());gc()
library(dplyr)
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

files=list.files("density_data")
d = do.call(rbind, 
                  lapply(paste0("density_data/",files[grepl("csv",files)]), function(x){
                    read.csv(x, stringsAsFactors=F, header=F)
                  })
) %>% do((function(x){
  names(x)=c("mean_payoff","count_strategies","is_optimal","game_length","setsize","initiator","initiator_degree","N",'k',"network","param","gini","simple","components","centralization","density")
  x
})(.)) %>%
  mutate(
      optimal = is_optimal>=(N*0.99)
    , gini_round=round(gini*2, 1)/2
    , cent_round = round(centralization*2,1)/2
    , EN=N
  ) %>% subset(gini_round<=0.5 & N==1000 &
                 k %in% c(20,50,100,200,300,400,500,750,999))



mean(d$count_strategies==1)

d_sum = d %>% 
  group_by(gini_round, N, network, density) %>%
  summarize(
                EN=length(N)
              , k=mean(k)
              
              , optimal_ci = (sd(optimal)/sqrt(EN))*1.96
              , optimal=mean(optimal)    
              
              , count_strategies=mean(count_strategies)
              
              
              , game_length_ci=(sd(game_length)/sqrt(EN))*1.96
              , game_length=mean(game_length)

              
              , payoff=mean(mean_payoff)
              
            )


ggplot(d_sum, aes(x=density, y=optimal)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=optimal-optimal_ci, ymax=optimal+optimal_ci), width=0) +
  geom_point(size=2, fill="#AAAAAA") + 
  beckertheme +
  scale_y_continuous(breaks=seq(0,1,by=0.1))+  
  scale_x_continuous(breaks=seq(0,1,by=0.1))+
  scale_shape_manual(values=c(21, 19), labels=c("BA","Generic")) +
  labs(x="Density", y="Probability of Optimal Coordination") +
  adjX + rotateX

ggsave("Figure 6 left.png", width=3.2, height=3.2)

ggplot(d_sum, aes(x=density, y=game_length/N)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=(game_length/N)-(game_length_ci/N), 
                    ymax=(game_length/N)+(game_length_ci/N), 
                width=0)) +
  geom_point(size=2, fill="#AAAAAA") + 
  scale_x_continuous(breaks=seq(0,1,by=0.1))+
  beckertheme +
  scale_shape_manual(values=c(21, 19), labels=c("BA","Generic")) +
  labs(x="Density", y="Average Updates per Agent") +
  adjX + rotateX
ggsave("Figure 6 right.png", width=3.2, height=3.2)
