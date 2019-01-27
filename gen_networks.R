library(igraph)

pow = 1
N = 1000
k = 20

for(i in 1:100) {
  g = barabasi.game(n=N, m=k/2, power=pow, directed=F)
  gname = paste0("nets/g",i,".Rds")
  saveRDS(g, gname)
}
