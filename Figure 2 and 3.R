rm(list=ls());gc()
library(dplyr)
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

files=list.files("fig2_data")
d = do.call(rbind, 
                  lapply(paste0("fig2_data/",files[grepl("csv",files)]), function(x){
                    read.csv(x, stringsAsFactors=F, header=F)
                  })
) %>% do((function(x){
  names(x)=c("mean_payoff","count_strategies","is_optimal","game_length","setsize","initiator","initiator_degree","N",'k',"network","param","gini","simple","components","centralization","local_clustering","global_clustering","avg_path_length","min_deg")
  x
})(.)) %>%
  mutate(
      optimal = is_optimal/N
    , is_optimal = optimal>=0.99
    , gini_round=round(gini*2, 1)/2
    , cent_round = round(centralization*2,1)/2
    , EN=N
    , path_round = round(avg_path_length*10)/10
  )%>% subset(gini_round<=0.5 & setsize == -1 & k<22)


d$mean_payoff_normalized = d$mean_payoff/sqrt((exp(1)-1)*exp(1))

d$density=d$k/d$N

mean(d$count_strategies==1)

d_sum = d %>% 
  group_by(gini_round, N, network) %>%
  summarize(
      EN=length(N)
    , k=mean(k)
    , density=mean(density)
    
    , optimal_ci = (sd(optimal)/sqrt(EN))*1.96
    , optimal=mean(optimal)    
    
    , count_strategies=mean(count_strategies)
    
    
    , game_length_ci=(sd(game_length)/sqrt(EN))*1.96
    , game_length=mean(game_length)

    
    , payoff=mean(mean_payoff)
    , transitivity=mean(local_clustering)
  )


d_sum_payoff = d %>%
  group_by(gini_round, N, network, is_optimal) %>%
  summarize(
      EN=length(N)
    , k=mean(k)
    , density=mean(density)
    , count_strategies=mean(count_strategies)
    , game_length=mean(game_length)

    , payoff_ci = (sd(mean_payoff)/sqrt(EN))*1.96
    , payoff=mean(mean_payoff)
  )



ggplot(d_sum, aes(x=gini_round, y=optimal, shape=network)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=optimal-optimal_ci, ymax=optimal+optimal_ci), width=0) +
  geom_point(size=2, fill="#AAAAAA") + 
  beckertheme +
  scale_shape_manual(values=c(21, 19), labels=c("BA","Generic")) +
  labs(x="Centralization (Gini Coefficient)", y="Probability of Optimal Coordination") +
  guides(shape=F)+
  theme(axis.ticks.length=unit(0, "cm"))+
  scale_x_continuous(expand=expand_scale(mult = c(0, 0))
                     ,lim=c(-0.03, 0.53)
                     , breaks=seq(0,0.5,by=0.1))+
  scale_y_continuous(expand=expand_scale(mult = c(0, 0))
                     ,lim=c(0.05, 0.75)
                     ,breaks=seq(0,1,by=0.1))+
  annotate(  geom="segment", y=seq(0,1,0.1), yend=seq(0,1,0.1), x=-0.03, xend= -0.02) +
  annotate(  geom="segment", y=seq(0,1,.05), yend=seq(0,1,.05),  x=-0.03, xend= -0.024) +
  
  annotate(  geom="segment", y=seq(0,1,0.1), yend=seq(0,1,0.1), x=0.53, xend= 0.52) +
  annotate(  geom="segment", y=seq(0,1,.05), yend=seq(0,1,.05),  x=0.53, xend= 0.524) +
  
  annotate(  geom="segment", x=seq(0,0.5,0.1), xend=seq(0,0.5,0.1), y=0.05, yend=0.067) +
  annotate(  geom="segment", x=seq(0,0.5,0.05), xend=seq(0,0.5,0.05), y=0.05, yend=0.06) +
  
  annotate(  geom="segment", x=seq(0,0.5,0.1), xend=seq(0,0.5,0.1), y=0.75, yend=0.733) +
  annotate(  geom="segment", x=seq(0,0.5,0.05), xend=seq(0,0.5,0.05), y=0.75, yend=0.74)
  
ggsave("Figure 2 left.png", width=3.2, height=3.2)



(ggplot(d_sum, aes(x=gini_round, y=game_length/N, shape=network)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=(game_length/N)-(game_length_ci/N), 
                    ymax=(game_length/N)+(game_length_ci/N), 
                width=0)) +
  geom_point(size=2, fill="#AAAAAA") + 
  beckertheme +
  scale_fill_manual(values=c("grey","black"))+
  scale_shape_manual(values=c(19, 21), labels=c("BA","Generic")) +
  labs(x="Centralization (Gini Coefficient)", y="Average Updates Per Agent") +
  guides(shape=F)) %>%
  innerTicks(   plot_obj=.
              , left_limit=-0.02
              , right_limit=0.6
              , y_major_breaks = seq(1,1.5,by=0.1)
              , y_minor_breaks = seq(1,1.5,by=0.05)
              , y_major_length=0.01
              , y_minor_length=0.006
              , bottom_limit=0.97
              , top_limit=1.53
              , x_major_breaks = seq(0,1.5,by=0.1)
              , x_minor_breaks = seq(0,1.5,by=0.05)
              , x_major_length=0.02
              , x_minor_length=0.008)

ggsave("Figure 2 right.png", width=3.2, height=3.2)

(ggplot(d_sum_payoff, aes(x=gini_round, y=payoff, shape=network, linetype=is_optimal)) + 
  geom_line() +
  geom_errorbar(aes(ymin=payoff-payoff_ci, ymax=payoff+payoff_ci), width=0) +
  geom_point(size=2, fill="#AAAAAA") + 
  scale_shape_manual(values=c(21, 19), labels=c("BA","Generic")) +
  beckertheme +
  guides(linetype=F, fill=F, shape=F)+
  labs(x="Centralization (Gini Coefficient)", y="Average Payoff")) %>%
  innerTicks(  plot_obj=.
               , left_limit=-0.03
               , right_limit=0.53
               , y_major_breaks = seq(0,35,by=5)
               , y_minor_breaks = seq(0,35,by=1)
               , y_major_length=0.01
               , y_minor_length=0.006
               , bottom_limit=5
               , top_limit=36
               , x_major_breaks = seq(0,0.5,by=0.1)
               , x_minor_breaks = seq(0,0.5,by=0.05)
               , x_major_length=0.75
               , x_minor_length=0.5)

  

ggsave("Figure 3.png", width=3.2, height=3.2)
