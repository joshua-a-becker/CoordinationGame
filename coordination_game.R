### BEST RESPONSE FUNCTION
best_response = function(j, g, payoff) {
  neighbor_strats = table(V(g)[nei(j)]$strategy)
  payoff_table = rbind(
    neighbor_strats
    , payoff[as.numeric(names(neighbor_strats))]
    , neighbor_strats*payoff[as.numeric(names(neighbor_strats))]
  )
  
  as.numeric(names(neighbor_strats)[which.max(payoff_table[3,])])
} 


### INITIALIZATION
initialize_game = function(g, setsize=vcount(g), payoff_distribution=function(n){rlnorm(n,0,1)}) {
  if(setsize==-1) {
    V(g)$strategy = 1:vcount(g)
    payoff = payoff_distribution(vcount(g))
  }
  if(setsize>0) {
    V(g)$strategy = as.numeric(as.factor(sample(setsize, vcount(g), replace=T)))
    payoff = payoff_distribution(length(unique(V(g)$strategy)))
  }
  
  ### initialize needs udpate
  ### when all are the same, nobody needs to update..
  V(g)$needs_update = unlist(sapply(V(g), function(j){
    V(g)[j]$strategy != best_response(j, g, payoff)
  }))
  
  list(g=g, payoff=payoff)
}


### RUN ONE UPDATE
run_update = function(game, agent, best_response_strategy, noise=0.1) {

  initial_strategy = V(game$g)[agent]$strategy
  
  ## otherwise, choose best response strategy with probability (1-noise)
  ## with probability noise, choose random neighbors strategy
  if(runif(1,0,1)<noise) {
    V(game$g)[agent]$strategy = sample(V(game$g)$strategy,1)
    
    ## update the "needs update" status
    V(game$g)[agent]$needs_update = V(game$g)[agent]$strategy==best_response_strategy
  } else {
    
    V(game$g)[agent]$strategy = best_response_strategy
    
    ## agent does not need to be updated
    V(game$g)[agent]$needs_update = T
  }
  
  ## if changed, mark friends as needing update
  ## (they might not actually need one--this is a heuristic to reduce computational expense)
  if(V(game$g)[agent]$strategy != initial_strategy) {
    V(game$g)[nei(agent)]$needs_update = T
  }

  ## return updated game object
  game
}



run_game=function(g, setsize=-1, cutoff=vcount(g)*100, smart=T, speedy=F){
  game=initialize_game(g=g, setsize=setsize)
  
  ### cutoff limits game length to reduce computational expense 
  ### from non-converging games
  ### this will rarely end a game, and can be easily verified in the data
  ### (by looking at game lengths)
  
  i=0
  while(i<cutoff) { 
    if(sum(V(game$g)$needs_update)==0) {
      
      ### CHECK TO BE SURE WE'RE REALLY DONE!
      ### FOR SOME REASON, WE OCCASIONALLY GET A LINGERING AGENT
      ### WHO STILL NEEDS AN UPDATE...
      ### IF YOU CAN FIGURE OUT WHY---
      ### please email joshua.aaron.becker@gmail.com !!
      
      V(game$g)$needs_update = unlist(sapply(V(g), function(j){
        V(game$g)[j]$strategy != best_response(j, game$g, game$payoff)
      }))
      
      if(sum(V(game$g)$needs_update)==0) break
    };
    
    ## pick an agent.  
    agent = sample(V(game$g), 1)
    
    ## if agent doesn't need update, skip to next round
    if(!V(game$g)[agent]$needs_update) {
      next
    }
    
    ## calc their best response
    best_response_strategy = best_response(agent, game$g, game$payoff)
    
    ## if agent is already a best response, skip to next round
    ## and mark them as not needing an update
    if(V(game$g)[agent]$strategy==best_response_strategy) {
      V(game$g)[agent]$needs_update = F
      next
    }
    
    ## otherwise, run update
    i = i + 1 #count update
    game=run_update(game, agent, best_response_strategy)
    
  }
  
  strat_table = sort(table(V(game$g)$strategy), decreasing=T)
  initiator = as.numeric(names(strat_table)[1])
  initiator_degree = degree(g)[as.numeric(names(strat_table)[1])]
  
  out_row=data.frame(
      mean_payoff      = mean(game$payoff[V(game$g)$strategy])
    , count_strategies = length(unique(V(game$g)$strategy))
    , is_optimal       = sum( V(game$g)$strategy==which.max(game$payoff) )
    , game_length = i
    , setsize = setsize
    , initiator = ifelse(setsize==-1, initiator, NA)
    , initiator_degree = initiator_degree
  )  
  
  out_row
}

run_games_in_parallel=function(reps, ...) {
  num_cores=detectCores()
  if(Sys.info()["sysname"]=="Windows") num_cores=1
  do.call(rbind,mclapply(1:reps, FUN=function(i){
    run_game(...)
  }, mc.cores=num_cores))
}
