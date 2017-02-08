#' @importFrom compiler cmpfun
#' @include Lehninger.R
#process INWT quotes
sepQuotes                      <- function(x, numeric = FALSE){
  
  quotes                       <- strsplit(as.character(x), split = "/", fixed = TRUE)
  
  quotes                       <- if(numeric) lapply(quotes, function(x) if(length(x) < 3) c(as.numeric(x), NA) else as.numeric(x)) else lapply(quotes, function(x) if(length(x) < 3) c(x, NA) else x)
 
  quotes                       <- as.data.frame(do.call(rbind, quotes))
  
  names(quotes)                <- paste("quote", 1:3, sep = "")
  
  return(quotes)  
}

get_idx_equivalent_std = function(data_table,homeGoals, awayGoals){
  
  if(homeGoals== awayGoals)
    
    idx_equivalent_std            <- which(data_table[,resultTypeId]=="standard-rest")
      
  else{
        
     idx_handicap                  <- which(data_table[,resultTypeId]=="handicap-rest")
        
     param                         <- t(matrix(as.numeric(unlist(strsplit(data_table[idx_handicap,fixedParam],":"))),nrow=2))
        
     idx_equivalent_std            <- idx_handicap[which(param[,1]- param[,2] == as.numeric(homeGoals)- as.numeric(awayGoals))]
     
     if(length(idx_equivalent_std )==0){
       
       idx_equivalent_std          <- 0
     }
    }
  
  return( idx_equivalent_std)
}

# utility function to be optimized
minifun                        <- function(x){
 
   return (t(stake)%*%(x/oddsF -rep(1,dim))) 
}
#overround constraint
eqn=function(x){

    return( sum(sapply(x,inv))-(margin)) 
}

inv=function(x){
 
   return (1/x)
}

sticky_filter                     <- function(data_table ,quotes_new, quotes_old , stickiness_coeff,homeGoals, awayGoals ){

  favourite                       <- rep(NA, nrow(data_table))

  arr_temp                        <- as.matrix(quotes_new)


  

  
  for (k in 1: nrow(data_table)){


    #determine natural move for favourite, ie which odd should go down
    if(data_table[k,resultTypeId]=="handicap-rest"){  
      param                        <- unlist(strsplit(data_table [k,fixedParam],":"))
      if(which(param>0)==1){
      favourite[k]                 <- 1
      }else{
        favourite[k]               <- 3 
      }
    } else if(data_table[k,resultTypeId]=="points-more-less-rest"){#under
      favourite[k]                 <- 2
    } else if(data_table[k,resultTypeId]%in% c("next-point","standard-rest")){ #draw
      favourite[k]                 <- 2
    } else if (data_table[k,resultTypeId]=="standard" & homeGoals>awayGoals){
      favourite[k]                 <- 1
    } else if(data_table[k,resultTypeId]=="standard" & homeGoals<awayGoals){ 
      favourite[k]                 <- 3
    }else if (data_table[k,resultTypeId]=="standard"){
      favourite[k]                 <- 2  
    }
    
    new                              <- as.numeric(as.character(quotes_new[k,]))
    
    old                              <- as.numeric(as.character(quotes_old[k,]))
    
    
    odds_to_compare                  <- rbind(old,new)
    
    ind                              <- apply(odds_to_compare, 2, function(x) all(is.na(x)))
    
    odds_to_compare                  <- odds_to_compare[,!ind ]
    
    Lehninger_distance               <- apply( odds_to_compare,2,getLehningerDistance)

     
    if(!is.na(favourite[k]) & !is.nan(Lehninger_distance[favourite[k]])& Lehninger_distance[favourite[k]]>0 & Lehninger_distance[favourite[k]]<5*stickiness_coeff){ #move against trend #Lehninger distance >0 if favourite odd goes up
      
      if(data_table[k,resultTypeId!="points-more-less-rest"] & Reduce("&",as.matrix(old)<as.matrix(data_table[k,c(quote1,quote2,quote3)])) |data_table[k,resultTypeId=="points-more-less-rest"] & Reduce("&",as.matrix(old[1:2])<as.matrix(data_table[k,c(quote1,quote2)]))){
        
        #arr_temp[k,favourite[k]]      <- as.numeric(as.matrix(old[favourite[k]]))
        arr_temp[k,]                   <- as.numeric(as.matrix(old))
        
        if( Reduce("&",abs(Lehninger_distance[-favourite[k]]) > 5*stickiness_coeff  )){ #re-optimize
          
          if( data_table[k,resultTypeId=="points-more-less-rest"]){
            
            arr_temp[k,1]             <- old[1]
            arr_temp[k,3]             <- NA
          }else{
            
            arr_temp[k,]              <- optimizer_restricted(data_table,LB=as.numeric(arr_temp[k,favourite[k]])-0.001,UB=as.numeric(arr_temp[k,favourite[k]])+0.001,idx=favourite[k],k)
          }
        }
      }
    }
  }
 
  return (arr_temp)
}

sticky_filter = compiler::cmpfun(sticky_filter)

smoothing_filter              <- function(data_asian, correct_coeff,balanced_coeff){
  
 odds                         <- get_odds(data_asian)
 
 equal_odds                   <- odds[,1:3] # equal prob
 
 correct_odds                 <- odds[,4:6]

 optimized_odds               <- data_asian[c("optimized_odd1","optimized_odd2", "optimized_odd3")]
  
 smoothed_odds                <- (balanced_coeff*equal_odds + correct_coeff*correct_odds + (1-correct_coeff-balanced_coeff)*optimized_odds)

 equal_odds_df                <- as.data.frame( equal_odds)
 
 names( equal_odds_df)        <- paste("equal_odd", 1:3, sep = "")
 
 data_asian                   <- data.frame(cbind(data_asian, equal_odds_df))
 
 correct_odds_df              <- as.data.frame( correct_odds )
 
 names(correct_odds_df)       <- paste("correct_odd", 1:3, sep = "")
 
 data_asian                   <- data.frame(cbind(data_asian, correct_odds_df))
 
 smoothed_odds_df             <- as.data.frame( smoothed_odds )
 
 names(smoothed_odds_df)      <- paste("smoothed_odd", 1:3, sep = "")
 
 data_asian                   <- data.frame(cbind(data_asian, smoothed_odds_df))
}


get_odds                      <- function(data_asian){
  
  equal_odds                  <-  array(dim=c(nrow(data_asian),3))
  
  correct_odds                <-  array(dim=c(nrow(data_asian),3))
  
 
  
  for (j in 1:nrow(data_asian)){
    
    if(data_asian$resultTypeId[j]=="double-chance"){
      margin_raw              <- data_asian[c("margin")][j,]
      margin                  <- data_asian[c("margin")][j,]-2
      
    } else{
      margin_raw              <- data_asian[c("margin")][j,]
      margin                  <- data_asian[c("margin")][j,]-1 
    }
 
    odd1                      <- as.numeric(as.character(data_asian["quote1"][j,]))
    odd2                      <- as.numeric(as.character(data_asian["quote2"][j,]))

    
    if(is.na(data_asian["quote3"][j,])) {
      
      equal_odds[j,]          <- c(max(1,1./(1/odd1+margin/2)),max(1,1./(1/odd2+margin/2)),NA)
    
      if(odd1<odd2){
        quote1                <- max(1,odd1/margin_raw)
        quote2                <- 1/(margin_raw-1/quote1)
      } else{
        quote2                <- max(1,odd2/margin_raw)
        quote1                <- 1/(margin_raw-1/quote2)
      }
      correct_odds[j,]        <- c(quote1,quote2,NA)
  
    }else{
      
      odd3                    <- as.numeric(as.character(data_asian["quote3"][j,]))
      
      equal_odds[j,]          <-  c(max(1,1./(1/odd1+margin/3)), max(1,1./(1/odd2+margin/3)), max(1,1./(1/odd3+margin/3)))
    
      if(odd1==min(odd1,odd2,odd3)){
        quote1                <- max(1,odd1/margin_raw)
        quote2                <- max(1,odd2/margin_raw)
        quote3                <- 1/(margin_raw-1/quote1-1/quote2)
        
      } else{
        quote3                <- max(1,odd3/margin_raw)
        quote2                <- max(1,odd2/margin_raw)
        quote1                <- 1/(margin_raw-1/quote2-1/quote3)
      }
      correct_odds[j,]        <- c(quote1,quote2,quote3)
      }
  } 
  return (cbind(equal_odds,correct_odds))
}
getLehningerDistance          <- function(odd){

  
  if(odd[2]>=odd[1]){
    
    distance                  <- 3/0.065*(odd[2]/odd[1]-1)/(odd[1]-0.9)^0.5
 
  }else{
  
    distance                  <- -3/0.065*(odd[1]/odd[2]-1)/(odd[2]-0.9)^0.5
  }
  return (distance)
}

nearest.vec <- function(x, vec) #for laddering purposes
{
  smallCandidate                  <- findInterval(x, vec, all.inside=TRUE)
  
  largeCandidate                  <- smallCandidate + 1
  
  nudge <- 2 * x > vec[smallCandidate] + vec[largeCandidate]
  #nudge is TRUE if large candidate is nearer, FALSE otherwise
  nudge[which(smallCandidate==1)]=FALSE

  return(smallCandidate + nudge)
}
aggregateDCto1X2<- function(data_table,odd_optimized){
  #populate DC according to standard
  Idx_DC                             <- which(data_table[,resultTypeId]=="double-chance")
  Idx_1X2                            <- which(data_table[,resultTypeId]=="standard")
  
  # replace DC optimized odds
  odd_optimized[Idx_DC,1]            <- 1/(1/odd_optimized[Idx_1X2,1]+1/odd_optimized[Idx_1X2,2])
  odd_optimized[Idx_DC,2]            <- 1/(1/odd_optimized[Idx_1X2,2]+1/odd_optimized[Idx_1X2,3])
  odd_optimized[Idx_DC,3]            <- 1/(1/odd_optimized[Idx_1X2,1]+1/odd_optimized[Idx_1X2,3])
  
  data_table[, optimized_odd.V1     := odd_optimized[,1]]
  data_table[, optimized_odd.V2     := odd_optimized[,2]]
  data_table[, optimized_odd.V3     := odd_optimized[,3]]
  
  #replace DC ref quotes
  data_table[Idx_DC,quote1           := as.numeric(as.character( 1/(1/as.numeric(as.character(data_table[Idx_1X2,quote1]))+1/as.numeric(as.character(data_table[Idx_1X2,quote2])))))]
  data_table[Idx_DC,quote2           := as.numeric(as.character(1/(1/as.numeric(as.character(data_table[Idx_1X2,quote2]))+1/as.numeric(as.character(data_table[Idx_1X2,quote3])))))]
  data_table[Idx_DC,quote3           := as.numeric(as.character(1/(1/as.numeric(as.character(data_table[Idx_1X2,quote1]))+1/as.numeric(as.character(data_table[Idx_1X2,quote3])))))]
  
  # replace DC Lehninger odds
  data_table[Idx_DC,Lehninger_odd.V1 := 1/(1/data_table[Idx_1X2,Lehninger_odd.V1]+1/data_table[Idx_1X2,Lehninger_odd.V2])]
  data_table[Idx_DC,Lehninger_odd.V2 := 1/(1/data_table[Idx_1X2,Lehninger_odd.V2]+1/data_table[Idx_1X2,Lehninger_odd.V3])]
  data_table[Idx_DC,Lehninger_odd.V3 := 1/(1/data_table[Idx_1X2,Lehninger_odd.V1]+1/data_table[Idx_1X2,Lehninger_odd.V3])]
  return(data_table)
}

report_to_csv                        <- function(data_table,game_name){
  
  ff                                 <- file(paste("//mtfs01/Departments/Bookmaking/17. Data Exchange Bookmaking/Celine/R/PriceEngine/working/output/sticky_grid/",game_name),open="at")

  if(data_table[1,PlayingMinute]==0){
   write.table(data_table, file = ff,append=TRUE,col.names=TRUE,row.names=FALSE)
  }
  else{
    write.table(data_table, file = ff,append=TRUE,col.names=FALSE,row.names=FALSE)
  }
  
  close(ff)

}

add_DCstakes_to_1X2                 <- function(data_table){
  # add DC stakes (translated in terms of risk ie payout) to 1X2 market, exclude DC from optimization, 
  #split up DC1 stakes into 1 and X of standard market
  DC_1X_1                             <- max(0,data_table[resultTypeId=="double-chance",stake_avg1]*(as.numeric(as.character(data_table[resultTypeId=="double-chance",quote1]))-1)/(as.numeric(as.character(data_table[resultTypeId=="standard",quote1]))-1))
  DC_1X_X                             <- max(0,data_table[resultTypeId=="double-chance",stake_avg1]*(as.numeric(as.character(data_table[resultTypeId=="double-chance",quote1]))-1)/(as.numeric(as.character(data_table[resultTypeId=="standard",quote2]))-1))
  #split up DC2 stakes into X and 2 of standard market
  DC_X2_X                             <- max(0,data_table[resultTypeId=="double-chance",stake_avg2]*(as.numeric(as.character(data_table[resultTypeId=="double-chance",quote2]))-1)/(as.numeric(as.character(data_table[resultTypeId=="standard",quote2]))-1))
  DC_X2_2                             <- max(0,data_table[resultTypeId=="double-chance",stake_avg2]*(as.numeric(as.character(data_table[resultTypeId=="double-chance",quote2]))-1)/(as.numeric(as.character(data_table[resultTypeId=="standard",quote3]))-1))
  #split up DC3 stakes into 1 and 2 of standard market
  DC_12_1                             <- max(0,data_table[resultTypeId=="double-chance",stake_avg3]*(as.numeric(as.character(data_table[resultTypeId=="double-chance",quote3]))-1)/(as.numeric(as.character(data_table[resultTypeId=="standard",quote1]))-1))
  DC_12_2                             <- max(0,data_table[resultTypeId=="double-chance",stake_avg3]*(as.numeric(as.character(data_table[resultTypeId=="double-chance",quote3]))-1)/(as.numeric(as.character(data_table[resultTypeId=="standard",quote2]))-1))
  data_table[resultTypeId=="standard", stake_avg1:= stake_avg1+DC_1X_1+DC_12_1] 
  data_table[resultTypeId=="standard", stake_avg2:= stake_avg2+DC_1X_X+DC_X2_X] 
  data_table[resultTypeId=="standard", stake_avg3:= stake_avg3+DC_12_2+DC_X2_2]
}

get_Lehninger_odds                <- function(input,data_table){
  
  Lehninger_odds                    <- apply(input,1,Lehninger)
  
  data_table[, Lehninger_odd.V1     := sapply(Lehninger_odds,'[[',1)]
  
  data_table[, Lehninger_odd.V2     := sapply(Lehninger_odds,'[[',2)]
  
  mask                              <- sapply(Lehninger_odds,length)==3
  
  data_table[mask, Lehninger_odd.V3 := sapply(Lehninger_odds[mask],'[[',3)]
  
  data_table[!mask, Lehninger_odd.V3:=NA] 
  return(Lehninger_odds )
}
get_Lehninger_marketing_odds        <- function(input,data_table){
  
  Lehninger_odds                    <- apply(input,1,Lehninger)
  
  data_table[, marketing_odd.V1     := sapply(Lehninger_odds,'[[',1)]
  
  data_table[, marketing_odd.V2     := sapply(Lehninger_odds,'[[',2)]
  
  mask                              <- sapply(Lehninger_odds,length)==3
  
  data_table[mask, marketing_odd.V3 := sapply(Lehninger_odds[mask],'[[',3)]
  
  data_table[!mask, marketing_odd.V3:=NA] 
}
