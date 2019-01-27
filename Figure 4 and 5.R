rm(list=ls());gc()

library(ggplot2)
library(igraph)
library(dplyr)
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

folder="effect_of_cent_data"
files=list.files(folder)

d = do.call(rbind, 
                  lapply(paste0(folder,"/",files[grepl("csv",files)]), read.csv, header=F)
)

nrow(d)


names(d) = c("mean_payoff", "count_strategies","is_optimal","game_length","setsize", "initiator", "initiator_degree", "N", "k", "network", "param", "net_number")


glist = lapply(1:100, function(i){
  readRDS(paste0("nets/g",i,".Rds"))
})

deg_table = lapply(glist, function(g){
  table(degree(g))
})

### maybe remove runs that were interrupted
### where the network was only run a handful of times
table(as.numeric(factor(d$net_number)))

degdist=unique(d$net_number)

expected  = do.call(rbind, lapply(unique(d$net_number), function(x){
  count=sum(d$net_number==x)
  out=data.frame(deg_table[[x]])
  colnames(out)=c("degree","count")
  out$count_adjust = out$count*count
  out
})) %>% group_by(degree) %>% summarize(count=sum(count_adjust)/1000)

list_of_degree = unique(expected$degree)

found = table(factor(d$initiator_degree, levels=list_of_degree))

mean(t(expected)[1,]==names(found))

likelihood = data.frame(
  degree=as.numeric(names(found))
  , found=as.numeric(found)
  , expected=(as.numeric(t(expected)[2,]))
) %>% mutate(
  lr = found/expected
) %>% 
  arrange(degree)


### SANITY CHECK
sum(likelihood$expected)==nrow(d)
sum(likelihood$found)
nrow(d)

head(likelihood)
l_sum = likelihood %>% 
  mutate(
    degree = round(degree/10)*10
  ) %>%
  group_by(degree) %>% 
  summarize(
    lr = mean(lr)
  )




ggplot(likelihood, aes(x=degree, y=lr)) +
  geom_point() +
  scale_y_log10() + scale_x_log10() + 
  beckertheme +
  geom_hline(yintercept=1, linetype="dashed") +
  annotation_logticks(sides="bltr") + 
  theme(axis.ticks.length=unit(0, "cm"))+
  labs(y="Likelihood Ratio L(D)\n(Advantage of Centrality)", x="Degree\n") 

ggsave("Fig 4.png", width=5, height=3)


d = d %>% group_by(net_number) %>% do((function(x){
  g = glist[[unique(x$net_number)]]
  x$degree = degree(g)[x$initiator]
  x
})(.))

#d$degree = d$initiator


d_sum_initiator = d %>% 
  mutate(
    initiator_chunk = round(degree/1)*1
  ) %>%
  group_by(initiator_chunk) %>% 
  summarize(
    N=length(mean_payoff)
    , payoff = mean(mean_payoff)
    , ci = (sd(mean_payoff)/sqrt(N))*1.96
  )



(ggplot(d_sum_initiator%>%
         subset(), aes(x=initiator_chunk, y=payoff)) +
  guides(color=F)+
  geom_point() + 
  #geom_line() +
  beckertheme +
  #geom_errorbar(aes(ymin=payoff-ci, ymax=payoff+ci), width=0) +
  labs(x="Degree of Node Introducing\nEmergent Convention",y="\nAverage Payoff")+
  theme(axis.ticks.length=unit(0, "cm"))) %>%
  innerTicks(   plot_obj=.
                , left_limit=-10
                , right_limit=210
                , y_major_breaks = seq(0,35,by=5)
                , y_minor_breaks = seq(0,35,by=1)
                , y_major_length=6
                , y_minor_length=3
                , bottom_limit=7
                , top_limit=28
                , x_major_breaks = seq(-50,210,by=50)
                , x_minor_breaks = seq(-10,210,by=10)
                , x_major_length=0.5
                , x_minor_length=0.25)

ggsave("Figure 5.png", width=5, height=3)

