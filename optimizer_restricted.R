#'@import Rsolnp
#'@importFrom compiler cmpfun
optimizer_restricted=function(data_table,LB,UB,idx,k){
  
  minifun <- function(x){
    return (t(stake)%*%(x/oddsF -rep(1,dim))) 
  }
  minifun = compiler::cmpfun(minifun)
  
  eqn=function(x){
    return( sum(sapply(x,inv))-(margin)) 
  }
  inv=function(x){
    return (1/x)
  }
  

    odd                   <- array(1,3)
    odds                   <- array(1,3)

    oddsF   	             <- c( as.numeric(as.character(data_table[k,quote1])), as.numeric(as.character(data_table[k,quote2])),as.numeric(as.character(data_table[k,quote3])))

    oddsF                  <- oddsF[!is.na( oddsF)]
    stake                  <- c( as.numeric(as.character(data_table[k,stake1])), as.numeric(as.character(data_table[k,stake2])),as.numeric(as.character(data_table[k,stake3])))
    
    stake                  <- stake[!is.na( stake)]
    dim		                 <- length(stake)
    
    margin                 <- as.numeric(as.character(data_table [k,margin]))-1/UB
    
   # LB_restricted          <- rep(1.001,dim)
   # LB_restricted[idx]     <- LB
    LB_restricted          <- rep(1.001,2)
    
    
    UB_rest                <- oddsF
    UB_rest[idx]           <- UB
    idx_opt                <- which(UB_rest!=UB)
    UB_restricted          <- UB_rest [idx_opt]
    
    #provide feasible starting point
    #initOdd   	           <- LB_restricted
    #initOdd   	           <- c( as.numeric(as.character(data_table[k,Lehninger_odd.V1])), as.numeric(as.character(data_table[k,Lehninger_odd.V2])),as.numeric(as.character(data_table[k,Lehninger_odd.V3])))
    #initOdd [idx]          <- LB
    #initOdd                <- initOdd[!is.na(initOdd)]
 
    margin_factor           <- margin/sum(1/oddsF[idx_opt])
    
    initOdd                 <- oddsF[idx_opt]/margin_factor
    
 
      
      
  
      
      # use SQP base solver, dispatch the optimizing jobs 
      
      optimized_odds        <- Rsolnp::solnp(pars = initOdd, fun = minifun, eqfun = eqn, eqB = 0,UB=UB_restricted ,LB=LB_restricted ,control= list("outer.iter"=100))
      
      if(is.null(optimized_odds)){
        odd                 <- initOdd 
      }
      else{
        odd                 <- optimized_odds$pars
      }
      
      odds[idx_opt]        <- odd 
      
      odds[idx]            <- UB
    
   
    #solnp =   compiler::cmpfun(solnp)
  return (odds)
}