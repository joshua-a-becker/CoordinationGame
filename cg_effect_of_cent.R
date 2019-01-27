#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
suppressMessages(library(igraph))
suppressMessages(library(RSQLite))

start_time = Sys.time()

source("coordination_game.R")

cores=1

N=1000
k=20
pow=1

if (length(args)>0) {
  network = args[1]
} else {
  stop("Must specify network");
}

file_number = floor(runif(1,0,100000))

if (length(args)>1) {
  file_number<<-args[2]
}

### FILE SETUP
outfile = paste0("effect_of_cent_data/cg_data_",file_number,".csv") 



gfile = paste0("nets/g",network,".Rds")
g = readRDS(gfile) 



table_dat = data.frame(table(degree(g)))
names(table_dat)=c("degree","count")

new_row = run_game(g)
new_row$N=N
new_row$k=k
new_row$network="ba"
new_row$param=pow
new_row$net_number = network

write.table(new_row, outfile, sep=",",append=T, col.names=F, row.names=F)