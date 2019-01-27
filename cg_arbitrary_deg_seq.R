#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library(igraph)
library(DescTools)
library(plyr)

pldist = function(N, alpha, max=N, min=1) {
  x1 = max           # Maximum value
  x0 = min         # Min value can't be zero; otherwise X^0^(neg) is 1/0.
  alpha = -2.5     # It has to be negative.
  y = runif(N)   # Number of samples
  ((x1^(alpha+1) - x0^(alpha+1))*y + x0^(alpha+1))^(1/(alpha+1))
}

N=1000

source("coordination_game.R")


file_number = floor(runif(1,0,100000))

if (length(args)>0) {
  file_number<<-args[1]
}


### FILE SETUP
outfile = paste0("fig2_data/cg_arb_data_",file_number,".csv") 


map_fn = read.csv("map_fn.csv")

### RUN GAMES
param_list = rep(13:nrow(map_fn))

setsize_list = c(2,10,100,-1)
for(i in param_list) {
  print(i)
  max=map_fn$max[i]
  min=map_fn$min[i]
  
  x=floor(pldist(N, alpha, max=max, min=min))
  while(mean(x)<19 | mean(x)>21) {
    x=floor(pldist(N, alpha, max=max, min=min))  
  }
  if(sum(x)%%2>0) {
    x[1]=x[1]+1
  }
  
  
  g = degree.sequence.game(x, method="vl")
  
  for(setsize in setsize_list) {

    new_row = run_game(g, setsize=setsize)
    new_row$N=N
    new_row$k=mean(degree(g))
    new_row$network="arbitrary"
    new_row$param=paste0(min,"-",max)
    new_row$gini = Gini(degree(g))
    new_row$simple = is.simple(g)
    new_row$components = components(g)$no    
    new_row$centralization = centralization.degree(g)$centralization
    new_row$local_clustering = mean(transitivity(g, type="local"))
    new_row$global_clustering = mean(transitivity(g, type="global"))
    new_row$avg_path_length = average.path.length(g)
    new_row$min_deg = min(degree(g))
    
    write.table(new_row, outfile, sep=",",append=T, col.names=F, row.names=F)
    
    print(setsize)
  }
}
