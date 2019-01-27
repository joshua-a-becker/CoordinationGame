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
  )

table(d$network)


mean(d$count_strategies==1)


d$mean_payoff_normalized = d$mean_payoff/sqrt((exp(1)-1)*exp(1))

d$density=d$k/d$N

mean(d$count_strategies==1)

d_sum = d %>% 
  group_by(gini_round, N, network, setsize) %>%
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

d_sum$setsize[d_sum$setsize==-1]=1000000

plotlist = lapply(sort(unique(d_sum$setsize)), function(s){
  setsize_label=ifelse(s==1000000, "\u221E", s)
  
  ggplot(d_sum%>%subset(setsize==s), aes(x=gini_round, y=optimal, shape=network)) + 
    geom_line() + 
    geom_point(size=2, fill="#AAAAAA") + 
    geom_errorbar(aes(ymin=optimal-optimal_ci, ymax=optimal+optimal_ci), width=0) +
    beckertheme +
    scale_y_continuous(breaks=seq(0,1,by=0.2), 
                       lim=c(0,1), 
                       labels=paste0(seq(0,100,by=20),"%"))+  
    scale_x_continuous(breaks=seq(0,1,by=0.1), lim=c(0,0.5))+
    scale_shape_manual(values=c(21, 19), labels=c("BA","Generic")) +
    labs(x="Centralization (Gini)", y="Prob. Optimal",
         title=paste0(setsize_label, " solutions")) +
    guides(shape=F)
})


#png("Figure 7.png"
    ,width=4.5, height=4.5, units="in", res=300)


Rmisc::multiplot(plotlist=plotlist, 
          layout=matrix(c(1,2,3,4),ncol=2, byrow=T))

dev.off()