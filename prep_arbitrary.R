library(DescTools)
library(dplyr)

N=1000

pldist = function(N, alpha, max=N, min=1) {
  x1 = max           # Maximum value
  x0 = min         # Min value can't be zero; otherwise X^0^(neg) is 1/0.
  alpha = -2.5     # It has to be negative.
  y = runif(N)   # Number of samples
  ((x1^(alpha+1) - x0^(alpha+1))*y + x0^(alpha+1))^(1/(alpha+1))
}

### GET MAP FOR PL DISTRIBUTION WITH CONSTANT MEAN
max_seq=c(20:39, ceiling(10^(seq(1.6,3,by=0.05))))




alpha=-2
d = data.frame(min=numeric(),
               max=numeric(), 
               gini=numeric(), 
               mean=numeric())
for(i in 1:20) {
  print(i)
  for(min in seq(5,21,by=1)) {    
    for(max in max_seq) {
      x=floor(pldist(N, alpha, max=max, min=min))
      d[nrow(d)+1,]=c(min,max, Gini(x), mean(x))
      
    }
  }
}

d_good=d%>%subset(mean<21&mean>19)

map_fn = d_good %>% group_by(max) %>%
  summarize(
     min=floor(mean(min))
   , Gini=mean(gini)
  )
map_fn$min[map_fn$min>20]=20

write.csv(map_fn, "map_fn.csv", row.names=F)
