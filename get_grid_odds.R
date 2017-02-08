# Compute grid odds
#' @import data.table
#' @return data_table
#' @importFrom R.cache saveCache loadCache
#' @include helper.R
#' 
get_grid_odds = function(data_table,homeGoals, awayGoals,rounding_grid_coeff,idx_equivalent_std){
  
  #initialize input data  
  ladder                                         <- read.csv("S:/Bookmaking/17. Data Exchange Bookmaking/Celine/R/oddsladder.csv")
  ladder_mat                                     <- as.matrix(ladder)
  
  quotes_old                                     <- data.table::data.table(R.cache::loadCache(key=list(5)))
  
  quotes_old_Tipico                              <- data.table::data.table(R.cache::loadCache(key=list(6)))
  
  penalty_rate                                   <- 0.3 # penalty for moving the odd against the natural trend
  
  favourite                                      <- rep(NA, nrow(data_table))
  utility_final                                  <- rep(0, nrow(data_table))
  utility_previous                               <- rep(0, nrow(data_table))
  flag_penalty                                   <- rep(0, nrow(data_table))
  penalty_final                                  <- rep(0, nrow(data_table))
  current_utility                                <- rep(0, nrow(data_table))
  new_odd                                        <- matrix(0.9, nrow = nrow(data_table), ncol = 3)
  
  flag_penalty_Tipico                            <- rep(0, nrow(data_table))
  distance_to_previous                           <- rep(0, nrow(data_table))
  distance_to_previous_Tipico                    <- rep(0, nrow(data_table))
  
  for (i in 1:nrow(data_table)){ #loop over each market

    #determine natural move for favourite, ie which odd should go down
    favourite[i]                                 <- get_favourite(data_table,i)
    if(!is.na(favourite[i]) &  length(quotes_old_Tipico)>0){
      if(!is.na(matrix(quotes_old_Tipico[i,])[favourite[i]])){
       Tipico_quote   	                         <- c( as.numeric(as.character(data_table[i,Tipico_quote1])), as.numeric(as.character(data_table[i,Tipico_quote2])),as.numeric(as.character(data_table[i,Tipico_quote3])))
       if( Tipico_quote [favourite[i]]>matrix(quotes_old_Tipico[i,])[favourite[i]]){
        flag_penalty_Tipico [i]                  <- 1
      }
     }
    }

    oddsF   	                                   <- c( as.numeric(as.character(data_table[i,quote1])), as.numeric(as.character(data_table[i,quote2])),as.numeric(as.character(data_table[i,quote3])))
    oddsF                                        <- oddsF[!is.na( oddsF)]
    
    stake                                        <- c( as.numeric(as.character(data_table[i,stake_avg1])), as.numeric(as.character(data_table[i,stake_avg2])),as.numeric(as.character(data_table[i,stake_avg3])))
    stake                                        <- stake[!is.na( stake)]
   
    # initialize new_odd to default values of Lehninger, in case we have no stakes 
    Lehni                                        <- data_table[i,c(Lehninger_odd.V1,Lehninger_odd.V2, Lehninger_odd.V3)]
    Lehni                                        <- Lehni[!is.na( Lehni)]
    
    idx1_L                                       <- nearest.vec(Lehni ,ladder_mat)
    idx1_L[idx1_L==0]                            <- 1  
    Lehni_adjusted                               <- ladder_mat[idx1_L]
    
    if(length(stake)==2){
      new_odd[i,c(1,2)]                          <- Lehni_adjusted 
      new_odd[i,3]                               <- NA 
    }else{
      new_odd[i,]                                <- Lehni_adjusted 
    }
    optima                                       <- 0
    utility_optima                               <- 0
    #look for grid optima
    if(length(idx_equivalent_std)>0 &i==idx_equivalent_std| data_table[i,resultTypeId]=="double-chance"){
      next
      }
    
    if(min(oddsF)>1 & min(Lehni)>1  & sum(stake)!=0){  
      grid_with_margin                           <- get_grid(oddsF,stake,Lehni,rounding_grid_coeff,ladder_mat,i)
      
      old_outcome                                <- as.numeric(as.character(quotes_old[i,]))
      
      old_outcome_original                       <- old_outcome
      
      old_outcome                                <- old_outcome[!is.na(old_outcome)]
      if(length( grid_with_margin )>0){# get index of optima
        opt                                      <- evaluate_grid_utility(grid_with_margin,old_outcome ,oddsF,stake,favourite[i],penalty_rate) 
        optima                                   <- opt[1]
        utility_optima                           <- opt[2]
        penalty_mask_optima                      <- opt[3]
      }
      comparison_output                          <- compare_optima_to_old_outcome(optima,rounding_grid_coeff,old_outcome,stake,oddsF,grid_with_margin,Lehni, utility_optima,penalty_mask_optima)
      
      new_odd[i,]                                <- comparison_output[1:3]
      current_utility[i]                         <- comparison_output[4]
      utility_final[i]                           <- comparison_output[5]
      penalty_final[i]                           <- comparison_output[6]
      
      distance_to_previous[i]                    <- 0
      distance_to_previous_Tipico[i]             <-0
      
      if(length(old_outcome)>0){
        odds_to_compare                           <- rbind(old_outcome_original,new_odd[i,] )
        
        ind                                       <- apply(odds_to_compare, 2, function(x) all(is.na(x)))
        
        odds_to_compare                           <- odds_to_compare[,!ind ]
        
        distance_to_previous[i]                   <- sum(abs(apply( odds_to_compare,2,getGridDistance)))
      }
      if(length(quotes_old_Tipico)>0){
        ##compute same distance for Tipico quotes
        odds_to_compare_T                          <- rbind(as.numeric(as.character(quotes_old_Tipico[i,])),Tipico_quote )
        
        ind_T                                      <- apply(odds_to_compare_T, 2, function(x) all(is.na(x)))
        
        odds_to_compare_T                           <- odds_to_compare_T[,!ind_T ]
        
        distance_to_previous_Tipico[i]              <- sum(abs(apply( odds_to_compare_T,2,getGridDistance)))
      }
    }
  }
  done                                           <- new_odd
  data_table[,new_odd.V1                        := done[,1] ]
  data_table[,new_odd.V2                        := done[,2] ]
  data_table[,new_odd.V3                        := done[,3] ]
  data_table[,penalty_mask                      := penalty_final]
  data_table[,flag_penalty_Tipico               := flag_penalty_Tipico ]
  data_table[,utility                           := utility_final]
  data_table[,utility_previous                  := current_utility]
  data_table[,distance_to_previous              := distance_to_previous]
  data_table[,distance_to_previous_Tipico       := distance_to_previous_Tipico]
  
  R.cache::saveCache(data_table[,.(new_odd.V1 ,new_odd.V2,new_odd.V3)], key=list(5)) 
  R.cache::saveCache(data_table[,.(Tipico_quote1 ,Tipico_quote2,Tipico_quote3)], key=list(6)) 
  return(data_table)
}



compare_optima_to_old_outcome = function(optima,rounding_grid_coeff,old_outcome,stake,oddsF,grid_with_margin,Lehni,utility_optima,penalty_mask_optima){
  current_utility                                <- 0
  utility_fin                                    <- 0
  penalty_final                                  <- 0
  margin_buffer                                  <- 1.1 # don't update the odds for a utility change of less than 20%
  safe_barrier_old_outcome_1                     <- oddsF[1]*1.5*rounding_grid_coeff+(1-rounding_grid_coeff)*Lehni[1]
  safe_barrier_old_outcome_2                     <- oddsF[2]*1.5*rounding_grid_coeff+(1-rounding_grid_coeff)*Lehni[2]
  
  # current_utility arising from staying on the same grid point, if still below the safe barrier
  if(length(old_outcome)>0){
    if(min(oddsF)>1 & min(old_outcome,na.rm=TRUE)>1 & old_outcome[1]< safe_barrier_old_outcome_1 & old_outcome[2]< safe_barrier_old_outcome_2){
      current_utility                            <- t(stake)%*%(old_outcome/oddsF -rep(1,length(stake)))
    }
  }
    utility_fin                                    <- current_utility
  
if(length( grid_with_margin )==0){
    if(length(stake)==2 &length(old_outcome)>0 & sum(stake)!=0){
      new_odds                                   <- c(old_outcome,NA)
    }else{
      if(length(stake)==3 &length(old_outcome)>0 & sum(stake)!=0){
        new_odds                                 <- old_outcome
      }
    }
}else{   ######################################################################################################
    if( utility_optima< margin_buffer*current_utility & sum(stake)!=0){ #minimization pb, smaller is better
      utility_fin                                 <- utility_optima #update the new utility
      
      penalty_final                               <- penalty_mask_optima
      

      
      if(length(stake)==2){
        
        if(ncol(as.matrix(grid_with_margin))==1){
          new_odds                                <- c(t(as.matrix(grid_with_margin)),NA)
        }else{
          new_odds                                <- c(as.matrix(grid_with_margin[optima,]),NA)
        }
      }
      if(length(stake)>2){
          if(ncol(as.matrix(grid_with_margin))==1){
            new_odds                               <- t(as.matrix(grid_with_margin))
          }else{
            new_odds                               <- as.matrix(grid_with_margin[optima,])
          }
        }############################################################################################
    }else{ #previous utility was better
      if(length(stake)==2 &length(old_outcome)>0 & sum(stake)!=0){
        new_odds                                   <- c(old_outcome,NA)
      }else{
        if(length(stake)==3 &length(old_outcome)>0 & sum(stake)!=0){
          new_odds                                 <- old_outcome
        }
      }
    }
  }
  return(c(new_odds ,current_utility, utility_fin,penalty_final ))
}

evaluate_grid_utility = function(grid_with_margin ,old_outcome ,oddsF,stake,fav,penalty_rate){
  if(length( grid_with_margin )>0){
    output                                      <- matrix(0, nrow(as.array(grid_with_margin)), 2)#as.matrix(rep(0, nrow(data_table)*2),ncol=2,byrow=TRUE)
    if(ncol(as.matrix(grid_with_margin))==1){
      grid_with_margin                          <- t(as.matrix(grid_with_margin))
      output                                    <- apply(grid_with_margin,1,get_utility,stake=stake,oddsF=oddsF,old = old_outcome,fav,penalty_rate=penalty_rate)
      
    }else{
      output                                    <- apply(as.matrix(grid_with_margin),1,get_utility,stake=stake,oddsF=oddsF,old = old_outcome,fav,penalty_rate=penalty_rate)
    }
    
    utility                                     <- output[1,]
    penalty_mask                                <- output[2,]     
    optima                                      <- which.min(utility)
  }
 return(c(optima,utility[optima],penalty_mask[optima]) )
}

get_grid = function(oddsF,stake,Lehni,rounding_grid_coeff,ladder_mat,i){

  if(length(stake)==2){

    upper_barrier_1                            <- oddsF[1]*rounding_grid_coeff+(1-rounding_grid_coeff)*Lehni[1]
    upper_barrier_2                            <- oddsF[2]*rounding_grid_coeff+(1-rounding_grid_coeff)*Lehni[2]
    
    Var1                                       <- ladder_mat[ladder_mat< upper_barrier_1]
    extra_outcome                              <- inv(margin[i]-sapply(Var1 ,inv))
    odds_grid                                  <- data.table::data.table(Var1)
    extra_outcome_laddered                     <- ladder_mat[nearest.vec(extra_outcome,ladder_mat)]
    odds_grid[,extra_outcome                   := extra_outcome_laddered ]
    odds_grid_reduced                          <- odds_grid[extra_outcome< upper_barrier_2]
    
  }else{
    upper_barrier_1                            <- oddsF[1]*rounding_grid_coeff+(1-rounding_grid_coeff)*Lehni[1]
    upper_barrier_2                            <- oddsF[2]*rounding_grid_coeff+(1-rounding_grid_coeff)*Lehni[2]
    upper_barrier_3                            <- oddsF[3]*rounding_grid_coeff+(1-rounding_grid_coeff)*Lehni[3]
    lower_barrier_2                            <- inv(margin[i]-sapply(upper_barrier_1 ,inv))
  
    Var                                        <- expand.grid( ladder_mat[ladder_mat< upper_barrier_1], ladder_mat[ladder_mat< upper_barrier_2 &ladder_mat>lower_barrier_2])
    extra_outcome                              <- inv(margin[i]-rowSums(sapply(Var,inv)))
    odds_grid                                  <- data.table::data.table(Var)
    #laddering the extra outcome
    extra_outcome_laddered                     <- ladder_mat[nearest.vec(extra_outcome,ladder_mat)]
    odds_grid[,extra_outcome                   := extra_outcome_laddered ]
    odds_grid_reduced                          <- odds_grid[extra_outcome< upper_barrier_3 & extra_outcome>lower_barrier_2]
  }
  
  grid_reduced_mat                             <- as.matrix(odds_grid_reduced)
  grid_reduced_mat[which(grid_reduced_mat==0.9)] <- NA 
  if(length(grid_reduced_mat)>0){
    actual_margin                                <- rowSums(matrix(sapply( grid_reduced_mat,inv),ncol=length(stake)),na.rm = TRUE)
    idx_margin_ok                                <- which(actual_margin >0.99*margin[i] & actual_margin <=1.01*margin[i])
    grid_with_margin                             <- grid_reduced_mat[ idx_margin_ok,]
    grid_with_margin[which(is.na(grid_with_margin))]<-0.9  
    return(grid_with_margin)
  }else {
    return (grid_reduced_mat)}
}


get_favourite = function(data_table,i){
   fav                                            <-NA
  #determine natural move for favourite, ie which odd should go down
  if(data_table[i,resultTypeId]=="handicap-rest"){  
    param                                         <- unlist(strsplit(data_table [i,fixedParam],":"))
    if(which(param>0)==1){
      fav                                         <- 1
    }else{
      fav                                         <- 3 
    }
  } else if(data_table[i,resultTypeId]=="points-more-less-rest"){#under
    fav                                           <- 2
  } else if(data_table[i,resultTypeId]%in% c("next-point","standard-rest")){ #draw
    fav                                           <- 2
  } else if (data_table[i,resultTypeId]=="standard" & homeGoals>awayGoals){
    fav                                           <- 1
  } else if(data_table[i,resultTypeId]=="standard" & homeGoals<awayGoals){ 
    fav                                           <- 3
  }else if (data_table[i,resultTypeId]=="standard"){
    fav                                           <- 2  
  }
  return(fav) 
}


aggregateDCto1X2_grid_odds = function(data_table){
  
  ladder                                      <- read.csv("S:/Bookmaking/17. Data Exchange Bookmaking/Celine/R/oddsladder.csv")
  ladder_mat                                  <- as.matrix(ladder)
  
 
  Idx_DC                                      <-  data_table[resultTypeId=="double-chance",which = TRUE]
  Idx_1X2                                     <-  data_table[resultTypeId=="standard",which = TRUE] 
  
  # replace DC optimized odds
  data_table[Idx_DC,new_odd.V1                := max(0.9,ladder_mat[nearest.vec(1/(1/data_table[Idx_1X2,new_odd.V1]+1/data_table[Idx_1X2,new_odd.V2]),ladder_mat)])]
  data_table[Idx_DC,new_odd.V2                := max(0.9,ladder_mat[nearest.vec(1/(1/data_table[Idx_1X2,new_odd.V2]+1/data_table[Idx_1X2,new_odd.V3]),ladder_mat)])]
  data_table[Idx_DC,new_odd.V3                := max(0.9,ladder_mat[nearest.vec(1/(1/data_table[Idx_1X2,new_odd.V1]+1/data_table[Idx_1X2,new_odd.V3]),ladder_mat)])]

  #replace DC ref quotes
  data_table[Idx_DC,quote1                    := max(0.9,ladder_mat[nearest.vec(as.numeric(as.character( 1/(1/as.numeric(as.character(data_table[Idx_1X2,quote1]))+1/as.numeric(as.character(data_table[Idx_1X2,quote2]))))),ladder_mat)])]
  data_table[Idx_DC,quote2                    := max(0.9,ladder_mat[nearest.vec(as.numeric(as.character(1/(1/as.numeric(as.character(data_table[Idx_1X2,quote2]))+1/as.numeric(as.character(data_table[Idx_1X2,quote3]))))),ladder_mat)])]
  data_table[Idx_DC,quote3                    := max(0.9,ladder_mat[nearest.vec(as.numeric(as.character(1/(1/as.numeric(as.character(data_table[Idx_1X2,quote1]))+1/as.numeric(as.character(data_table[Idx_1X2,quote3]))))),ladder_mat)])]
  
  # replace DC Lehninger odds
  data_table[Idx_DC,Lehninger_odd.V1          := max(0.9,ladder_mat[nearest.vec(1/(1/data_table[Idx_1X2,Lehninger_odd.V1]+1/data_table[Idx_1X2,Lehninger_odd.V2]),ladder_mat)])]
  data_table[Idx_DC,Lehninger_odd.V2          := max(0.9,ladder_mat[nearest.vec(1/(1/data_table[Idx_1X2,Lehninger_odd.V2]+1/data_table[Idx_1X2,Lehninger_odd.V3]),ladder_mat)])]
  data_table[Idx_DC,Lehninger_odd.V3          := max(0.9,ladder_mat[nearest.vec(1/(1/data_table[Idx_1X2,Lehninger_odd.V1]+1/data_table[Idx_1X2,Lehninger_odd.V3]),ladder_mat)])]

return(data_table) 
}
get_utility = function(x,stake,oddsF,old,fav,penalty_rate){
  
  
  flag_penalty                                   <- 0
  
  utility                                        <- 0
  
  dim		                                         <- length(stake)
  
  stake [which(x==0.9)]                          <- 0
  
  utility                                        <-  t(stake)%*%(x/oddsF -rep(1,dim))
  
  if(!is.na(fav) &  !is.na(old[fav])){
    
    if(x[fav]>old[fav]){ # is this specific grid point going against the natural trend?
      
      utility                                     <-  (1-penalty_rate)*t(stake)%*%(x/oddsF -rep(1,dim))
      
      flag_penalty                                <- 1 
      
      
    }else{ 
      if ( x[fav]==old[fav] & Reduce("|",x[-fav]!=old[-fav]) ){ # penalty  for changing the other outcomes and not the favourite
        
        utility                                   <-  (1-penalty_rate/2)*t(stake)%*%(x/oddsF -rep(1,dim))
        
        flag_penalty                              <- 0.5
      }
    }
    
  }
  
  return (cbind(utility,flag_penalty))
  
}

eqn=function(x){
  return( sum(sapply(x,inv))-(margin)) 
}
inv=function(x){
  return (1/x)
}

getGridDistance=function(odds){
  return(which(ladder_mat>=odds[2])[1]-which(ladder_mat>=odds[1])[1])
}