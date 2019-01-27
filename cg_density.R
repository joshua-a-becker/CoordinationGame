#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
suppressMessages(library(igraph))
suppressMessages(library(DescTools))

suppressMessages(source("coordination_game.R"))

N=1000

file_number = floor(runif(1,0,100000))

if (length(args)>0) {
  file_number<-args[1]
}


### FILE SETUP
outfile = paste0("density_data/cg_dens_data_",file_number,".csv") 



k_list = c(20,50,100,200,300,400,500,750,999)
for(k in k_list) {
  cat("starting ", k, "\n")
  g=degree.sequence.game(rep(k,N), method="vl")
  new_row = run_game(g)
  new_row$N=N
  new_row$k=k
  new_row$network="decent_vl"
  new_row$param=k
  new_row$gini = Gini(degree(g))
  new_row$simple = is.simple(g)
  new_row$components = components(g)$no    
  new_row$centralization = centralization.degree(g)$centralization
  new_row$density = graph.density(g)
  write.table(new_row, outfile, sep=",",append=T, col.names=F, row.names=F)
}