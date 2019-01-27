#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
suppressMessages(library(igraph))
suppressMessages(library(RSQLite))
suppressMessages(library(DescTools))

source("coordination_game.R")

N=1000
k=20

file_number = floor(runif(1,0,100000))

if (length(args)>0) {
  file_number<<-args[1]
}


### FILE SETUP
outfile = paste0("fig2_data/cg_ba_data_",file_number,".csv") 


pow_list = rep(seq(1.75,2.5,by=0.25))
setsize_list = c(2,10,100,-1)

for(pow_index in 1:7) {
	for(setsize_index in 1:4) {
		pow = pow_list[pow_index]
		setsize = setsize_list[setsize_index]
		g=barabasi.game(n=N, m=k/2, power=pow, directed=F)

		new_row = run_game(g, setsize=setsize)
		new_row$N=N
		new_row$k=k
		new_row$network="ba"
		new_row$param=pow
		new_row$gini = Gini(degree(g))
		new_row$simple = is.simple(g)
		new_row$components = components(g)$no    
		new_row$centralization = centralization.degree(g)$centralization
		new_row$local_clustering = mean(transitivity(g, type="local"))
		new_row$global_clustering = mean(transitivity(g, type="global"))
		new_row$avg_path_length = average.path.length(g)
		new_row$min_deg = min(degree(g))


		write.table(new_row, outfile, sep=",",append=T, col.names=F, row.names=F)

	}
  a=gc()
}
