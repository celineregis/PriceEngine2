#' Compute smoothed & optimized odds
#' @param reference odds and stake distribution
#' @return final odds
#' @importFrom Rsolnp solnp
#' @importFrom doSNOW  registerDoSNOW
#' @importFrom parallel makeCluster stopCluster clusterEvalQ clusterCall clusterExport
#' @importFrom foreach %do% %dopar%
#' @importFrom compiler cmpfun
#' @import compiler
#'

optimizer=function(data_table,homeGoals, awayGoals){

  odds                         <- array(dim=c(nrow(data_table),3))
  cl                           <- parallel::makeCluster(8, type = "SOCK")
 # compiler::cmpfun(parallel::makeCluster)
  doSNOW::registerDoSNOW(cl)
  parallel::clusterExport(cl,c('minifun','eqn'),envir=environment())
  parallel::clusterEvalQ(cl, library(Rsolnp))
  parallel::clusterEvalQ(cl, library(doParallel))
  parallel::clusterEvalQ(cl, library(data.table))
  parallel::clusterEvalQ(cl, library(compiler))
  parallel::clusterCall(cl, function() { source("//mtfs01/Departments/Bookmaking/17. Data Exchange Bookmaking/Celine/R/PriceEngine2/R/helper.R") })
  
  idx_equivalent_std          <- get_idx_equivalent_std(data_table,homeGoals, awayGoals )
  solnp = compiler::cmpfun(Rsolnp::solnp)
  results<-foreach (i = 1:nrow(data_table),.combine = "rbind")%dopar%{
    
    initOdd   	                 <- c( as.numeric(as.character(data_table[i,Lehninger_odd.V1])), as.numeric(as.character(data_table[i,Lehninger_odd.V2])),as.numeric(as.character(data_table[i,Lehninger_odd.V3])))
    
    oddsF   	                   <- c( as.numeric(as.character(data_table[i,quote1])), as.numeric(as.character(data_table[i,quote2])),as.numeric(as.character(data_table[i,quote3])))
    
    oddsF                        <- oddsF[!is.na( oddsF)]
    
    if(length(idx_equivalent_std)>0 &i==idx_equivalent_std| data_table[i,resultTypeId]=="double-chance" | min(oddsF)<1){ 
        
      odds                      <- initOdd 

    }else{
      initOdd                   <- initOdd[!is.na(initOdd)]
       
      oddsF                     <- oddsF[!is.na( oddsF)]
      stake                     <- c( as.numeric(as.character(data_table[i,stake_avg1])), as.numeric(as.character(data_table[i,stake_avg2])),as.numeric(as.character(data_table[i,stake_avg3])))
      
      stake                     <- stake[!is.na( stake)]
      
      dim		                    <- length(stake)
      margin                    <- as.numeric(as.character(data_table [i,margin]))
      
      LB                        <- rep(1.0001,dim)
      UB                        <- oddsF
      
      #provide feasible starting point
      # initOdd  	                <- LB
      # non_fav                   <- which(oddsF!= min(oddsF))
      # initOdd[non_fav]          <- (dim-1)/(margin-1/1.0001)
      # 

      
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
      # use SQP base solver, dispatch the optimizing jobs 
      
      optimized_odds          <- Rsolnp::solnp(pars=initOdd, fun=minifun, eqfun = eqn, eqB = 0, LB = LB, UB = UB)
      
      if(is.null(optimized_odds)){
        odds                  <- oddsF
        
      }else{
        
        if (length(oddsF)==3){
          odds                <- optimized_odds$pars
        }else {
          odds                <- c(optimized_odds$pars,NA)
        }
      }
    }
  }
 
  parallel::stopCluster(cl)

  return (results)
}