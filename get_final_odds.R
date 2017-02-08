#' Compute smoothed & optimized odds
#' @rdname get_final_odds
#' @param takes as input the run index, the buffer size for stakes, the buffer size for stickiness, the margin, the stakes, the rounding coefficient, the stickiness coefficient, abd the input for the INWT Live library
#' @export get_final_odds
#' @return final odds
#' @importFrom R.cache saveCache loadCache
#' @import data.table
#' @include wrapper.R
#' @include helper.R
#' @include optimizer.R
#' @include optimizer_restricted.R
#' @include Lehninger.R
#' @include get_final_odds.R
#' 
get_final_odds = function(game_name,test_flag,input_line,margin, stake, buffer_size_stake,marketing_budget,rounding_grid_coeff,rounding_coeff, stickiness_coeff,eventStartInMinutesAgo, redCards, matchStatus, playingMinute, hcpParameter, hcpHome, hcpAway, ouParameter, ouOver, ouUnder, homeGoals, awayGoals){

  if(test_flag==0){ # AH and O/U lines  inputs processed through INWT Live library
#get reference odds
    data_asian                                      <- wrapper(eventStartInMinutesAgo, redCards, matchStatus, playingMinute, hcpParameter, hcpHome, hcpAway, ouParameter, ouOver, ouUnder, homeGoals, awayGoals) 
    
    stake_df                                        <- as.data.frame( matrix(stake,ncol=3,byrow=TRUE))
    
    names(stake_df)                                 <- paste("stake", 1:3, sep = "")
    
    status                                          <- (rep(1,nrow(stake_df)))
    

    
    status_df                                       <- data.frame(status)
    
    data_table                                      <- data.table::data.table(status_df,sepQuotes(data_asian[[1]]),data_asian ["resultTypeId"], data_asian ["fixedParam"], margin,stake_df, Lehninger_odd = matrix(rep(0,nrow(data_asian)*3),ncol=3),optimized_odd = matrix(rep(0,nrow(data_asian)*3),ncol=3),smoothed_odd =matrix(rep(0,nrow(data_asian)*3),ncol=3) )
  }else{ #process inputs from file
    #stakes
    stake_df                                        <- as.data.frame( matrix(as.numeric(input_line[c("DC_Stake_1X","DC_Stake_X2","DC_Stake_12","HCP01_Stake_1","HCP01_Stake_X","HCP01_Stake_2",
                                                                  "HCP02_Stake_1","HCP02_Stake_X","HCP02_Stake_2","HCP10_Stake_1","HCP10_Stake_X","HCP10_Stake_2",
                                                                  "HCP20_Stake_1","HCP20_Stake_X","HCP20_Stake_X","NG_Stake_1","NG_Stake_X","NG_Stake_2",
                                                                  "OU05_Stake_O","OU05_Stake_U","OU05_Stake_U","OU15_Stake_O","OU15_Stake_U","OU15_Stake_U",
                                                                  "OU25_Stake_O","OU25_Stake_U","OU25_Stake_U","RT_Stake_1","RT_Stake_X","RT_Stake_2",
                                                                  "MW_Stake_1","MW_Stake_X","MW_Stake_2")]),ncol=3,byrow=TRUE))
    names(stake_df)                                 <- paste("stake", 1:3, sep = "")
    
    stake_df[c(7,8,9),3]                            <- NA #O/U markets
    
    quote                                           <- as.data.frame( matrix(as.numeric(input_line[c("DC_Ref_1X","DC_Ref_X2","DC_Ref_12","HCP01_Ref_1", "HCP01_Ref_X","HCP01_Ref_2","HCP02_Ref_1","HCP02_Ref_X","HCP02_Ref_2",
                                                                  "HCP10_Ref_1","HCP10_Ref_X","HCP10_Ref_2", "HCP20_Ref_1","HCP20_Ref_X","HCP20_Ref_2","NG_Ref_1","NG_Ref_X","NG_Ref_2",
                                                                  "OU05_Ref_O","OU05_Ref_U","OU05_Ref_U", "OU15_Ref_O","OU15_Ref_U","OU15_Ref_U", "OU25_Ref_O","OU25_Ref_U","OU25_Ref_U",
                                                                  "RT_Ref_1", "RT_Ref_X", "RT_Ref_2","MW_Ref_1", "MW_Ref_X", "MW_Ref_2")]),ncol=3,byrow=TRUE))
    
    quote[c(7,8,9),3]                               <- NA #O/U markets
    
    names(quote)                                    <- paste("quote", 1:3, sep = "")
    
    Tipico_quote                                    <- as.data.frame( matrix(as.numeric(input_line[c("DC_Tipico_1X","DC_Tipico_X2","DC_Tipico_12","HCP01_Tipico_1", "HCP01_Tipico_X","HCP01_Tipico_2","HCP02_Tipico_1","HCP02_Tipico_X","HCP02_Tipico_2",
                                                                                                     "HCP10_Tipico_1","HCP10_Tipico_X","HCP10_Tipico_2", "HCP20_Tipico_1","HCP20_Tipico_X","HCP20_Tipico_2","NG_Tipico_1","NG_Tipico_X","NG_Tipico_2",
                                                                                                     "OU05_Tipico_O","OU05_Tipico_U","OU05_Tipico_U", "OU15_Tipico_O","OU15_Tipico_U","OU15_Tipico_U", "OU25_Tipico_O","OU25_Tipico_U","OU25_Tipico_U",
                                                                                                  "RT_Tipico_1", "RT_Tipico_X", "RT_Tipico_2","MW_Tipico_1", "MW_Tipico_X", "MW_Tipico_2")]),ncol=3,byrow=TRUE))
    
    Tipico_quote[c(7,8,9),3]                        <- NA #O/U markets
    
    names(Tipico_quote)                             <- paste("Tipico_quote", 1:3, sep = "")
    
    market_status                                   <-  input_line[c("DC_Status","HCP01_Status","HCP02_Status","HCP10_Status","HCP20_Status","NG_Status",
    
                                                                          "OU05_Status","OU15_Status","OU25_Status","RT_Status","MW_Status")]
    status                                          <- (rep(0,nrow(stake_df)))
    
    status [which(market_status =="open")]          <- 1 
    
   
    status_df                                       <- data.frame(status)
   
    fixedParam                                      <- c("","0:1","0:2","1:0","2:0","","0.5","1.5","2.5","","")
    
    resultTypeId                                    <- c("double-chance","handicap-rest","handicap-rest","handicap-rest","handicap-rest","next-point","points-more-less-rest","points-more-less-rest","points-more-less-rest","standard-rest","standard")
    
    data_table                                      <- data.table::data.table(homeGoals,awayGoals,status_df,marketing_budget,input_line["PlayingMinute"],quote,Tipico_quote, resultTypeId, fixedParam ,margin,stake_df,Lehninger_odd = matrix(rep(0,nrow(stake_df)*3),ncol=3),optimized_odd = matrix(rep(0,nrow(stake_df )*3),ncol=3),smoothed_odd =matrix(rep(0,nrow(stake_df)*3),ncol=3) )
   
    data_table [quote1<1 &quote2<1 &(quote3<1|is.na(quote3)), status       :=0]
  }
   
  #overwrite the input margin with the Tipico margins (for ok markets) 
     
    # data_table[Tipico_quote1>1&Tipico_quote2>1&Tipico_quote3>1,margin:= rowSums (1./ as.matrix(data_table[Tipico_quote1>1&Tipico_quote2>1&Tipico_quote3>1,.(Tipico_quote1, Tipico_quote2 ,Tipico_quote3)]),na.rm = FALSE)] 
  Markets1                                          <- c("standard", "standard-rest", "points-more-less-rest")
 
  Markets2                                          <- c("handicap-rest", "next-point")
  
  Markets3                                          <- c("double-chance")
  
  data_table[resultTypeId %in% Markets1,margin      := 1.05]
  data_table[resultTypeId %in% Markets2,margin      := 1.10]
  data_table[resultTypeId %in% Markets3,margin      := 2.10]
               
   
     ladder                                         <- read.csv("S:/Bookmaking/17. Data Exchange Bookmaking/Celine/R/oddsladder.csv")
    
    ladder_mat                                      <- as.matrix(ladder)
    
#get Lehninger odds    
    input                                           <- matrix(c(as.numeric(as.character(data_table[,quote1])),as.numeric(as.character(data_table[,quote2])),as.numeric(as.character(data_table[,quote3])),data_table[,margin]),ncol=4)
    
    Lehninger_odds                                  <- get_Lehninger_odds(input,data_table)
    

    
#match  handicap market with 1x2 Lehninger odds and add up stakes
    idx_equivalent_std                              <- get_idx_equivalent_std(data_table,homeGoals, awayGoals )

   if(idx_equivalent_std>0) {
    data_table[resultTypeId=="standard",stake1      := data_table[resultTypeId=="standard",stake1]+data_table[idx_equivalent_std,stake1]]
    data_table[resultTypeId=="standard",stake2      := data_table[resultTypeId=="standard",stake2]+data_table[idx_equivalent_std,stake2]]
    data_table[resultTypeId=="standard",stake3      := data_table[resultTypeId=="standard",stake3]+data_table[idx_equivalent_std,stake3]]
  
#and replace Lehninger odds accordingly for handicap mkt    
    data_table[idx_equivalent_std,Lehninger_odd.V1  := data_table[resultTypeId=="standard",Lehninger_odd.V1]]
    data_table[idx_equivalent_std,Lehninger_odd.V2  := data_table[resultTypeId=="standard",Lehninger_odd.V2]]
    data_table[idx_equivalent_std,Lehninger_odd.V3  := data_table[resultTypeId=="standard",Lehninger_odd.V3]]
   }
# by default average stakes are provided    
    data_table[, stake_avg1                         := stake1]
    data_table[, stake_avg2                         := stake2]
    data_table[, stake_avg3                         := stake3]
# in testing environment raw stakes are provided and thereafter averaged  
    
    previous_data_table                             <- R.cache::loadCache(key=list(3))
    if(test_flag==1){
      #if the score changes, delete the stakes cache
      if (length(previous_data_table )>0 ){
        if(homeGoals!= previous_data_table[1,homeGoals] |awayGoals!=previous_data_table[1,awayGoals]){
          if(!is.null(R.cache::findCache(key=list(1)))){
            file.remove(R.cache::findCache(key=list(1)))
          }
          if(!is.null(R.cache::findCache(key=list(2))) ){
            file.remove(R.cache::findCache(key=list(2)))
          }
          if(!is.null(R.cache::findCache(key=list(3))) ){
            file.remove(R.cache::findCache(key=list(3)))
          }
          if(!is.null(R.cache::findCache(key=list(5))) ){
            file.remove(R.cache::findCache(key=list(5)))
          }
 
        }
      }
      
         # average stakes over last x time intervals
         old_list                                   <- R.cache::loadCache(key=list(2))
         
         if (length(old_list )>0) {
           
            stake_avg                               <- as.matrix(old_list[[1]])
        
            new_list                                <- append(old_list,list(data_table[,.(stake1,stake2, stake3)]))
         }else{
           
           stake_avg                                <-  data_table[,.(stake1,stake2, stake3)]
           
           new_list                                 <- list(stake_avg)
         }
         
         
         #retrieve list of previous stakes
         if (length(old_list )>1){
           
           stake_per_market                            <- array(0,dim=c(nrow(new_list[[1]]),3,buffer_size_stake))
     
           l                                           <- array(1,nrow(new_list[[1]]))
          
           is_non_zero                                 <- FALSE
           
           for (j in  (length(new_list):1)){
             
           #retrieve non-zero stakes
             Idx_non_zero                              <- unique(which(new_list[[j]]!=0)%%nrow(as.matrix(new_list[[j]])))
           
             Idx_non_zero[Idx_non_zero==0]             <- nrow(as.matrix(new_list[[j]]))
           
             Idx_non_zero                              <- unique(Idx_non_zero)
             
             if(length(Idx_non_zero)>0){
               is_non_zero                             <- TRUE
               
               for (m in (1:length(Idx_non_zero))){ # loop over each market m
                
                while(l[m]< buffer_size_stake & sum(stake_per_market[Idx_non_zero[m],,l[m]],na.rm=TRUE)!=0){
                   
                   l[m]                                 <- l[m]+1
                }
                 
                stake_per_market[Idx_non_zero[m],,l[m]] <- as.matrix(new_list[[j]])[Idx_non_zero[m],]
                 
               }
             }
           }
           max_length                                   <- 1
          if(is_non_zero){
           max_length                                   <- min(buffer_size_stake,l[m] )#length(new_list))
          }
    
           stake_avg                                    <- stake_per_market[,,max_length]
         
           if(max_length>1){
             for (j in ((max_length-1):1)){
                 #retrieve last non-zeros stakes market by market
               
                stake_avg                              <- stake_avg + (max_length-j+1) * stake_per_market[,,j]
             }
            stake_avg                                  <- stake_avg/(sum(1:max_length))
           }
        
         }
      
        R.cache::saveCache(new_list, key=list(2))
       
      
        data_table[, stake_avg1                        := as.matrix(stake_avg)[,1]]
        data_table[, stake_avg2                        := as.matrix(stake_avg)[,2]]
        data_table[!is.na(stake3), stake_avg3          := as.matrix(stake_avg)[ data_table[!is.na(stake3),which = TRUE],3]]
        data_table[is.na(stake3),stake_avg3            := NA]
        
    }
    ##########################################################################
    # alternative methodology: compute the grid-based odds
    if(rounding_grid_coeff>0){
      
     data_table                                         <- get_grid_odds(data_table,homeGoals, awayGoals,rounding_grid_coeff,idx_equivalent_std)
      
      if(idx_equivalent_std>0) {  # duplicate std odds for corresponding handicap market
        data_table[idx_equivalent_std,new_odd.V1          :=data_table[resultTypeId=="standard",new_odd.V1]]
        data_table[idx_equivalent_std,new_odd.V2          :=data_table[resultTypeId=="standard",new_odd.V2]]
        data_table[idx_equivalent_std,new_odd.V3          :=data_table[resultTypeId=="standard",new_odd.V3]]
      }
      data_table                                        <- aggregateDCto1X2_grid_odds(data_table)
      
    }
     # ROUNDED ODDS 
    if (rounding_coeff>0){#get optimized odds and combine them with Lehninger odds to get smoothed odds
       
       data_table                                      <- add_DCstakes_to_1X2(data_table)
      
       odd_optimized                                   <- optimizer( data_table,homeGoals, awayGoals )
       # duplicate std odds for corresponding handicap market
 
       if(idx_equivalent_std>0) { 
         odd_optimized[idx_equivalent_std,]            <- odd_optimized[which(data_table[,resultTypeId=="standard"]),]
       }
       #convert back optimized odds to DC market #########################################
       data_table                                      <- aggregateDCto1X2(data_table,odd_optimized)
       
       ##forward filling of non-correct input odds (and consequently optimized, Lehninger)
       if (length(previous_data_table )>0 ){
         
         Idx_forward_filled                             <- which( data_table[,quote1<1 |quote2<1 |quote3<1])
         
         data_table[Idx_forward_filled,quote1           := previous_data_table[Idx_forward_filled,quote1]]
         data_table[Idx_forward_filled,quote2           := previous_data_table[Idx_forward_filled,quote2]]
         data_table[Idx_forward_filled,quote3           := previous_data_table[Idx_forward_filled,quote3]]
         
         data_table[Idx_forward_filled,new_odd.V1       := previous_data_table[Idx_forward_filled,new_odd.V1]]
         data_table[Idx_forward_filled,new_odd.V2       := previous_data_table[Idx_forward_filled,new_odd.V2]]
         data_table[Idx_forward_filled,new_odd.V3       := previous_data_table[Idx_forward_filled,new_odd.V3]]
         
         
         data_table[Idx_forward_filled,optimized_odd.V1 := previous_data_table[Idx_forward_filled,optimized_odd.V1]]
         data_table[Idx_forward_filled,optimized_odd.V2 := previous_data_table[Idx_forward_filled,optimized_odd.V2]]
         data_table[Idx_forward_filled,optimized_odd.V3 := previous_data_table[Idx_forward_filled,optimized_odd.V3]]
         
         data_table[Idx_forward_filled,Lehninger_odd.V1 := previous_data_table[Idx_forward_filled,Lehninger_odd.V1]]
         data_table[Idx_forward_filled,Lehninger_odd.V2 := previous_data_table[Idx_forward_filled,Lehninger_odd.V2]]
         data_table[Idx_forward_filled,Lehninger_odd.V3 := previous_data_table[Idx_forward_filled,Lehninger_odd.V3]]
         
         data_table[Idx_forward_filled,smoothed_odd.V1  := previous_data_table[Idx_forward_filled,Lehninger_odd.V1]]
         data_table[Idx_forward_filled,smoothed_odd.V2  := previous_data_table[Idx_forward_filled,Lehninger_odd.V2]]
         data_table[Idx_forward_filled,smoothed_odd.V3  := previous_data_table[Idx_forward_filled,Lehninger_odd.V3]]
         
         data_table[!Idx_forward_filled,smoothed_odd.V1 := rounding_coeff*data_table[!Idx_forward_filled,optimized_odd.V1] +  (1-rounding_coeff)*data_table[!Idx_forward_filled,Lehninger_odd.V1]]
         data_table[!Idx_forward_filled,smoothed_odd.V2 := rounding_coeff*data_table[!Idx_forward_filled,optimized_odd.V2] +  (1-rounding_coeff)*data_table[!Idx_forward_filled,Lehninger_odd.V2]]
         data_table[!Idx_forward_filled,smoothed_odd.V3 := rounding_coeff*data_table[!Idx_forward_filled,optimized_odd.V3] +  (1-rounding_coeff)*data_table[!Idx_forward_filled,Lehninger_odd.V3]]
         
         
         Idx_forward_filled_Tipico                       <- which( data_table[,Tipico_quote1<1 |Tipico_quote2<1 |Tipico_quote3<1])
         
         data_table[Idx_forward_filled_Tipico,Tipico_quote1 := previous_data_table[Idx_forward_filled_Tipico,Tipico_quote1]]
         data_table[Idx_forward_filled_Tipico,Tipico_quote2 := previous_data_table[Idx_forward_filled_Tipico,Tipico_quote2]]
         data_table[Idx_forward_filled_Tipico,Tipico_quote3 := previous_data_table[Idx_forward_filled_Tipico,Tipico_quote3]]
         
       }
    
       # compute linear combination of Lehninger and optimized odd
       else{
         
         data_table[,smoothed_odd.V1                    := rounding_coeff*data_table[,optimized_odd.V1] +  (1-rounding_coeff)*data_table[,Lehninger_odd.V1]]
         data_table[,smoothed_odd.V2                    := rounding_coeff*data_table[,optimized_odd.V2] +  (1-rounding_coeff)*data_table[,Lehninger_odd.V2]]
         data_table[,smoothed_odd.V3                    := rounding_coeff*data_table[,optimized_odd.V3] +  (1-rounding_coeff)*data_table[,Lehninger_odd.V3]]
         
       }
       
       
    }else{ #skip optimization, just return Lehninger odds
       data_table[,smoothed_odd.V1                     := data_table[,Lehninger_odd.V1]]
       data_table[,smoothed_odd.V2                     := data_table[,Lehninger_odd.V2]]
       data_table[,smoothed_odd.V3                     := data_table[,Lehninger_odd.V3]]
    } 
    
    #compute smoothed (rounded) odds profit before marketing  
    data_table[ !is.na(stake3),profit_margin_rounded                    := stake1*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) +stake3*(1- smoothed_odd.V3/as.numeric(as.character(quote3)))]
    
    data_table[ is.na(stake3),profit_margin_rounded                    := stake1*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) ]
    
    
    rounded_profit                                   <- data_table[,sum(profit_margin_rounded)]


    if(marketing_budget>0){ 
      # MARKETING ODDS: get Lehninger odds with marketing overround for 1X2, RT, equivalent handicap market, and O/U  
      data_table                                     <- get_marketing_odds(data_table,marketing_budget,idx_equivalent_std,rounded_profit,test_flag)
    }
    # 
    data_table[smoothed_odd.V1 >= as.numeric(as.character(data_table[,quote1])),smoothed_odd.V1 := as.numeric(as.character(quote1))]  
    data_table[smoothed_odd.V2 >= as.numeric(as.character(data_table[,quote2])),smoothed_odd.V2 := as.numeric(as.character(quote2))] 
    data_table[smoothed_odd.V3 >= as.numeric(as.character(data_table[,quote3])),smoothed_odd.V3 := as.numeric(as.character(quote3))] 

    quotes_old                                        <- data.table::data.table(R.cache::loadCache(key=list(1)))
    quotes_new                                        <- data_table[,.(smoothed_odd.V1 ,smoothed_odd.V2, smoothed_odd.V3)]
   

 # go through stickiness filter
       
    if (length(quotes_old )>0) {

      odds_filtered                                   <- sticky_filter(data_table,quotes_new,quotes_old , stickiness_coeff,homeGoals, awayGoals)
     
      
      data_table[,smoothed_odd.V1                     := odds_filtered[,1]]
      data_table[,smoothed_odd.V2                     := odds_filtered[,2]]
      data_table[,smoothed_odd.V3                     := odds_filtered[,3]]
    }
    
    R.cache::saveCache(data_table[,.(smoothed_odd.V1 ,smoothed_odd.V2, smoothed_odd.V3)], key=list(1)) 
    
#ladder the smoothed odds

    idx1                                              <- nearest.vec(data_table[, smoothed_odd.V1],ladder_mat)
    idx2                                              <- nearest.vec(data_table[, smoothed_odd.V2],ladder_mat)
    idx3                                              <- nearest.vec(data_table[, smoothed_odd.V3],ladder_mat)

    data_table[,smoothed_odd.V1                       := ladder_mat[idx1]]
    data_table[,smoothed_odd.V2                       := ladder_mat[idx2]]
    data_table[,smoothed_odd.V3                       := ladder_mat[idx3]]
    
#laddering Lehninger odds
    idx1_L                                            <- nearest.vec(data_table[,Lehninger_odd.V1],ladder_mat)
    idx1_L[idx1_L==0]                                 <- 1  # 1.01 is the absolute minimum
    data_table[, Lehninger_odd.V1                     := ladder_mat[idx1_L]]
    
    idx2_L                                            <- nearest.vec(data_table[,Lehninger_odd.V2],ladder_mat)
    idx2_L[idx2_L==0]                                 <- 1  # 1.01 is the absolute minimum
    data_table[, Lehninger_odd.V2                     := ladder_mat[idx2_L]]
    
    mask                                              <- sapply(Lehninger_odds,length)==3
    idx3_L                                            <- nearest.vec(data_table[mask,Lehninger_odd.V3],ladder_mat)
    idx3_L[idx3_L==0]                                 <- 1  # 1.01 is the absolute minimum
    data_table[mask, Lehninger_odd.V3                 := ladder_mat[idx3_L]]
    data_table[!mask, Lehninger_odd.V3                := NA] 

    #ladder the new odds
    
    idx1_new                                          <- nearest.vec(data_table[, new_odd.V1],ladder_mat)
    idx2_new                                          <- nearest.vec(data_table[, new_odd.V2],ladder_mat)
    idx3_new                                          <- nearest.vec(data_table[, new_odd.V3],ladder_mat)
    
    data_table[,new_odd.V1                            := ladder_mat[idx1_new]]
    data_table[,new_odd.V2                            := ladder_mat[idx2_new]]
    data_table[,new_odd.V3                            := ladder_mat[idx3_new]]    


    
    if(test_flag>0){ #for testing purposes, compute profitability metrics
      # compute actual overround
      data_table[smoothed_odd.V1>=1 & smoothed_odd.V2>=1 & smoothed_odd.V3>=1 & !is.na(stake3),overround := 1/smoothed_odd.V1+1/smoothed_odd.V2+1/smoothed_odd.V3]
      data_table[smoothed_odd.V1>=1 & smoothed_odd.V2>=1 & is.na(stake3),overround                       := 1/smoothed_odd.V1+1/smoothed_odd.V2]
      data_table[smoothed_odd.V1<1 & smoothed_odd.V2>=1 & smoothed_odd.V3>=1 & !is.na(stake3),overround  := 1+1/smoothed_odd.V2+1/smoothed_odd.V3]
      data_table[smoothed_odd.V1<1 & smoothed_odd.V2>=1 & is.na(stake3),overround                        := 1+1/smoothed_odd.V2]
      data_table[smoothed_odd.V1>=1 & smoothed_odd.V2<1 & smoothed_odd.V3>=1 & !is.na(stake3),overround  := 1+1/smoothed_odd.V1+1/smoothed_odd.V3]
      data_table[smoothed_odd.V1>=1 & smoothed_odd.V2<1 & is.na(stake3),overround                        := 1+1/smoothed_odd.V1]
      data_table[smoothed_odd.V1>=1 & smoothed_odd.V2>=1 & smoothed_odd.V3<1 & !is.na(stake3),overround  := 1+1/smoothed_odd.V1+1/smoothed_odd.V2]
      data_table[smoothed_odd.V1<1 & smoothed_odd.V2<1 & is.na(stake3),overround                         := 1+1]
      data_table[smoothed_odd.V1<1 & smoothed_odd.V2<1 & smoothed_odd.V3<1 & !is.na(stake3),overround    := 1+1+1]
      
      data_table[new_odd.V1>=1 & new_odd.V2>=1 & new_odd.V3>=1 & !is.na(stake3),overround_new            := 1/new_odd.V1+1/new_odd.V2+1/new_odd.V3]
      data_table[new_odd.V1>=1 & new_odd.V2>=1 & is.na(stake3),overround_new                             := 1/new_odd.V1+1/new_odd.V2]
      data_table[new_odd.V1<1 & new_odd.V2>=1 & new_odd.V3>=1 & !is.na(stake3),overround_new             := 1+1/new_odd.V2+1/new_odd.V3]
      data_table[new_odd.V1<1 & new_odd.V2>=1 & is.na(stake3),overround_new                              := 1+1/new_odd.V2]
      data_table[new_odd.V1>=1 & new_odd.V2<1 & new_odd.V3>=1 & !is.na(stake3),overround_new             := 1+1/new_odd.V1+1/new_odd.V3]
      data_table[new_odd.V1>=1 & new_odd.V2<1 & is.na(stake3),overround_new                              := 1+1/new_odd.V1]
      data_table[new_odd.V1>=1 & new_odd.V2>=1 & new_odd.V3<1 & !is.na(stake3),overround_new             := 1+1/new_odd.V1+1/new_odd.V2]
      data_table[new_odd.V1<1 & new_odd.V2<1 & is.na(stake3),overround_new                               := 1+1]
      data_table[new_odd.V1<1 & new_odd.V2<1 & new_odd.V3<1 & !is.na(stake3),overround_new               := 1+1+1]
      
      
      
      data_table[smoothed_odd.V1>=1,stake1_reduced:=stake1] 
      data_table[smoothed_odd.V1<1,stake1_reduced:=0] 
      data_table[smoothed_odd.V2>=1,stake2_reduced:=stake2] 
      data_table[smoothed_odd.V2<1,stake2_reduced:=0]
      data_table[smoothed_odd.V3>=1,stake3_reduced:=stake3] 
      data_table[smoothed_odd.V3<1,stake3_reduced:=0]
  
      
      data_table[resultTypeId%in% c("handicap-rest","next-point","standard-rest","standard"),payout_ratio                := (1-1/overround)*(stake1_reduced+stake2_reduced+stake3_reduced)]
      data_table[resultTypeId=="double-chance",payout_ratio                                                              := (1-1/overround/2)*(stake1_reduced+stake2_reduced+stake3_reduced)]
      data_table[is.na(stake3),payout_ratio                                                                              := (1-1/overround)*(stake1_reduced+stake2_reduced)]
      
      data_table[resultTypeId%in% c("handicap-rest","next-point","standard-rest","standard"),payout_ratio_full_stake     := (1-1/overround)*(stake1+stake2+stake3)]
      data_table[resultTypeId=="double-chance",payout_ratio_full_stake                                                   := (1-1/overround/2)*(stake1 +stake2 +stake3)]
      data_table[is.na(stake3),payout_ratio_full_stake                                                                   := (1-1/overround)*(stake1+stake2)]
       
      
      data_table[resultTypeId%in% c("handicap-rest","next-point","standard-rest","standard"),payout_ratio_new            := (1-1/overround_new)*(stake1_reduced+stake2_reduced+stake3_reduced)]
      data_table[resultTypeId=="double-chance",payout_ratio_new                                                          := (1-1/overround_new/2)*(stake1_reduced+stake2_reduced+stake3_reduced)]
      data_table[is.na(stake3),payout_ratio_new                                                                          := (1-1/overround_new)*(stake1_reduced+stake2_reduced)]
      
      data_table[resultTypeId%in% c("handicap-rest","next-point","standard-rest","standard"),payout_ratio_full_stake_new := (1-1/overround_new)*(stake1+stake2+stake3)]
      data_table[resultTypeId=="double-chance",payout_ratio_full_stake_new                                               := (1-1/overround_new/2)*(stake1 +stake2 +stake3)]
      data_table[is.na(stake3),payout_ratio_full_stake_new                                                               := (1-1/overround_new)*(stake1+stake2)]
      
      
      if (length(previous_data_table )>0 ) {
        Idx_open_now_and_previous                                             <- intersect(which(previous_data_table[,status>0]),which(data_table[,status>0]) )
                                                                             
        data_table[Idx_open_now_and_previous,ref_quote1                       := (as.numeric(as.character(previous_data_table[Idx_open_now_and_previous,quote1])) + as.numeric(as.character(data_table[Idx_open_now_and_previous,quote1])))/2 ]
        data_table[Idx_open_now_and_previous,ref_quote2                       := (as.numeric(as.character(previous_data_table[Idx_open_now_and_previous,quote2])) + as.numeric(as.character(data_table[Idx_open_now_and_previous,quote2])))/2 ]
        data_table[Idx_open_now_and_previous,ref_quote3                       := (as.numeric(as.character(previous_data_table[Idx_open_now_and_previous,quote3])) + as.numeric(as.character(data_table[Idx_open_now_and_previous,quote3])))/2 ]
      
        data_table[!Idx_open_now_and_previous,ref_quote1                      :=  as.numeric(as.character(data_table[!Idx_open_now_and_previous,quote1]))]
        data_table[!Idx_open_now_and_previous,ref_quote2                      :=  as.numeric(as.character(data_table[!Idx_open_now_and_previous,quote2]))]
        data_table[!Idx_open_now_and_previous,ref_quote3                      :=  as.numeric(as.character(data_table[!Idx_open_now_and_previous,quote3]))]
        
      
        
      data_table[!is.na(stake3),profit_margin                                 := stake1_reduced*(1- smoothed_odd.V1 / as.numeric(as.character(ref_quote1)))+stake2_reduced*(1- smoothed_odd.V2/as.numeric(as.character(ref_quote2))) +stake3_reduced*(1- smoothed_odd.V3/as.numeric(as.character(ref_quote3)))]
      data_table[is.na(stake3),profit_margin                                  := stake1_reduced*(1- smoothed_odd.V1 / as.numeric(as.character(ref_quote1)))+stake2_reduced*(1- smoothed_odd.V2/as.numeric(as.character(ref_quote2))) ]
      
      data_table[!is.na(stake3),profit_margin_new                             := stake1_reduced*(1- new_odd.V1 / as.numeric(as.character(ref_quote1)))+stake2_reduced*(1- new_odd.V2/as.numeric(as.character(ref_quote2))) +stake3_reduced*(1- new_odd.V3/as.numeric(as.character(ref_quote3)))]
      data_table[is.na(stake3),profit_margin_new                              := stake1_reduced*(1- new_odd.V1 / as.numeric(as.character(ref_quote1)))+stake2_reduced*(1- new_odd.V2/as.numeric(as.character(ref_quote2))) ]
      
      
      data_table[!is.na(stake3),profit_margin_Tipico                          := stake1*(1- Tipico_quote1 / as.numeric(as.character(ref_quote1)))+stake2*(1- Tipico_quote2/as.numeric(as.character(ref_quote2))) +stake3*(1- Tipico_quote3/as.numeric(as.character(ref_quote3)))]
      data_table[is.na(stake3),profit_margin_Tipico                           := stake1*(1- Tipico_quote1 / as.numeric(as.character(ref_quote1)))+stake2*(1- Tipico_quote2/as.numeric(as.character(ref_quote2))) ]
      }
      else{
        data_table[,ref_quote1                                                := quote1]
        data_table[,ref_quote2                                                := quote2]
        data_table[,ref_quote3                                                := quote3]
        
        
        data_table[!is.na(stake3),profit_margin                               := stake1_reduced*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2_reduced*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) +stake3_reduced*(1- smoothed_odd.V3/as.numeric(as.character(quote3)))]
        data_table[is.na(stake3),profit_margin                                := stake1_reduced*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2_reduced*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) ]
        
        data_table[!is.na(stake3),profit_margin_new                           := stake1_reduced*(1- new_odd.V1 / as.numeric(as.character(quote1)))+stake2_reduced*(1- new_odd.V2/as.numeric(as.character(quote2))) +stake3_reduced*(1- new_odd.V3/as.numeric(as.character(quote3)))]
        data_table[is.na(stake3),profit_margin_new                            := stake1_reduced*(1- new_odd.V1 / as.numeric(as.character(quote1)))+stake2_reduced*(1- new_odd.V2/as.numeric(as.character(quote2))) ]
        
        data_table[!is.na(stake3),profit_margin_Tipico                        := stake1*(1- Tipico_quote1 / as.numeric(as.character(quote1)))+stake2*(1- Tipico_quote2/as.numeric(as.character(quote2))) +stake3*(1- Tipico_quote3/as.numeric(as.character(quote3)))]
        data_table[is.na(stake3),profit_margin_Tipico                         := stake1*(1- Tipico_quote1 / as.numeric(as.character(quote1)))+stake2*(1- Tipico_quote2/as.numeric(as.character(quote2))) ]
        
      }
 
      R.cache::saveCache(data_table, key=list(3))
      R.cache::saveCache(rounded_profit, key=list(4))
      
      # outputing status and metrics to log file
      report_to_csv(data_table,game_name)
    }    
    return (data_table[,.(quote1,quote2,quote3,smoothed_odd.V1 ,smoothed_odd.V2, smoothed_odd.V3,Lehninger_odd.V1 ,Lehninger_odd.V2, Lehninger_odd.V3)])
  
}