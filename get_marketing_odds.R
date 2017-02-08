#' @import data.table
#' @return data_table
#' @importFrom R.cache saveCache loadCache
#' @include Lehninger.R
#' @include helper.R
get_marketing_odds = function(data_table,marketing_budget,idx_equivalent_std,rounded_profit,test_flag){
  # retrieve previous budget and cost
 
  cache                                          <- R.cache::loadCache(key=list(0))
  
  if (length(cache)>0 & test_flag==1) {
    budget_left                                  <- cache[1]
    
  }else{
    budget_left                                  <- marketing_budget
  }
  if(budget_left>0){
    #update budget with realized marketing cost from previous iteration ie margin of current stake on previous smoothed prices
    #retrieve previous data_table
    previous_data_table                          <- R.cache::loadCache(key=list(3))
    
    if (length(previous_data_table )>0) {
      
      
      #compute actual cost of full marketing odds for markets of interest
      # interpolate the reference odds over the intervall
      previous_data_table[,quote1                := (as.numeric(as.character(previous_data_table[,quote1])) + as.numeric(as.character(data_table[,quote1])))/2 ]
      previous_data_table[,quote2                := (as.numeric(as.character(previous_data_table[,quote2])) + as.numeric(as.character(data_table[,quote2])))/2 ]
      previous_data_table[,quote3                := (as.numeric(as.character(previous_data_table[,quote3])) + as.numeric(as.character(data_table[,quote3])))/2 ]
      # overwrite old stakes with current stakes that hit the old prices
      previous_data_table[,stake1                :=  data_table[,stake1]]
      previous_data_table[,stake2                :=  data_table[,stake2]]
      previous_data_table[,stake3                :=  data_table[,stake3]]
      #compute margin over marketing odds
      previous_data_table[ resultTypeId!="points-more-less-rest",profit_margin_marketing         := stake1*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) +stake3*(1- smoothed_odd.V3/as.numeric(as.character(quote3)))]
      
      previous_data_table[ resultTypeId=="points-more-less-rest",profit_margin_marketing         := stake1*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) ]
      
      marketing_profit_previous                   <- previous_data_table[,sum(profit_margin_marketing)]
      
      # compute margin over rounded smoothed odds as a benchmark
      rounded_profit_previous                     <- R.cache::loadCache(key=list(4))
      
      marketing_cost_previous                     <- rounded_profit_previous- marketing_profit_previous
      
      # compute new budget_left and save it in the cache
      budget_left                                 <- budget_left - marketing_cost_previous 
      
      data_table[,marketing_budget                := budget_left]
      
      R.cache::saveCache(budget_left, key=list(0)) 
    }
    #over
    Idx_over_05                                   <- which(data_table[,resultTypeId=="points-more-less-rest" & fixedParam=="0.5"])
    
    main_over                                     <- data_table[resultTypeId=="points-more-less-rest",quote1]
    
    Idx_OU                                        <- which(data_table[,resultTypeId=="points-more-less-rest"])
    # main O/U markets
    Idx_main_OU                                   <- Idx_OU[as.numeric(as.character(main_over))>1.4 & as.numeric(as.character(main_over))<3 | as.numeric(as.character( data_table[Idx_over_05 ,quote1]))>3]
    
    
    #RT, std and equivalent handicap markets
    Idx_mkt                                       <- unique(c(idx_equivalent_std,which(data_table[,resultTypeId]%in%c("standard-rest","standard"))))
    
    data_table[,margin_marketing                 := margin]
    # divide the margin by 4
    data_table[c(Idx_main_OU,Idx_mkt),margin_marketing := (data_table[c(Idx_main_OU,Idx_mkt),margin]-1)/4+1]
    
    input_marketing                               <-  matrix(c(as.numeric(as.character(data_table[,quote1])),as.numeric(as.character(data_table[,quote2])),as.numeric(as.character(data_table[,quote3])),data_table[,margin_marketing]),ncol=4)
    # get Lehninger odds with the reduced margin
    data_table                                    <- get_Lehninger_marketing_odds(input_marketing,data_table)
    
    #under: divide the margin by 2
    input_marketing_U                             <- matrix(c(as.numeric(as.character(data_table[Idx_main_OU,quote1])),as.numeric(as.character(data_table[Idx_main_OU,quote2])),as.numeric(as.character(data_table[Idx_main_OU,quote3])),
                                                             as.numeric(as.character((data_table[ Idx_main_OU ,margin]-1)/2+1))),ncol=4)
    # get Lehninger odds with the reduced margin for under
    Lehninger_odds_under                          <- apply(input_marketing_U,1,Lehninger)
    
    data_table[Idx_main_OU, marketing_odd.V2     := Lehninger_odds_under[2,]]
    
    #compute Lehninger profit
    data_table[ resultTypeId!="points-more-less-rest",profit_margin_L         := stake1*(1- Lehninger_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- Lehninger_odd.V2/as.numeric(as.character(quote2))) +stake3*(1- Lehninger_odd.V3/as.numeric(as.character(quote3)))]
    
    data_table[ resultTypeId=="points-more-less-rest",profit_margin_L         := stake1*(1- Lehninger_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- Lehninger_odd.V2/as.numeric(as.character(quote2))) ]
    
    lehninger_profit                             <- data_table[,sum(profit_margin_L)]
    
    budget_this_iteration                        <- budget_left /((90-playingMinute)*6) #bins of 10s, ie 6 intervals per min
    #############
    # get people's favourite from standard-rest
    Idx_people_favourite                         <- which(data_table[resultTypeId=="standard-rest",.(quote1,quote2,quote3)]== min((as.matrix(data_table[resultTypeId=="standard-rest",.(quote1,quote2,quote3)]))))
    
    if(length(Idx_people_favourite )==1){ #RT market is active
      if(Idx_people_favourite==2){
        data_table[Idx_mkt ,smoothed_odd.V2      := data_table[Idx_mkt ,marketing_odd.V2]]
        data_table[Idx_mkt ,smoothed_odd.V1      := data_table[Idx_mkt ,Lehninger_odd.V1]]
        data_table[Idx_mkt ,smoothed_odd.V3      := data_table[Idx_mkt ,Lehninger_odd.V3]]
      }else{
        if(Idx_people_favourite==1){
          data_table[Idx_mkt ,smoothed_odd.V1    := data_table[Idx_mkt ,marketing_odd.V1]] 
          data_table[Idx_mkt ,smoothed_odd.V2    := data_table[Idx_mkt ,Lehninger_odd.V2]]
          data_table[Idx_mkt ,smoothed_odd.V3    := data_table[Idx_mkt ,Lehninger_odd.V3]]
        }else{
          data_table[Idx_mkt ,smoothed_odd.V3    := data_table[Idx_mkt,marketing_odd.V3]]
          data_table[Idx_mkt ,smoothed_odd.V2    := data_table[Idx_mkt,Lehninger_odd.V2]]
          data_table[Idx_mkt ,smoothed_odd.V1    := data_table[Idx_mkt,Lehninger_odd.V1]]
        }
      }
    }
    data_table[Idx_main_OU,smoothed_odd.V1       := data_table[Idx_main_OU ,marketing_odd.V1]] #OVER
    data_table[Idx_main_OU,smoothed_odd.V2       := data_table[Idx_main_OU ,marketing_odd.V2]] #UNDER
    
    #compute actual cost of full marketing odds for markets of interest
    
    data_table[ resultTypeId!="points-more-less-rest",profit_margin_marketing         := stake1*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) +stake3*(1- smoothed_odd.V3/as.numeric(as.character(quote3)))]
    
    data_table[ resultTypeId=="points-more-less-rest",profit_margin_marketing         := stake1*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) ]
    
    marketing_profit                             <- data_table[,sum(profit_margin_marketing)]
     
    marketing_cost                               <- rounded_profit-marketing_profit
    
    marketing_coeff                              <- min(1,budget_this_iteration/marketing_cost)
    # compute linear combination of Lehninger and marketing odds
    if(length(Idx_people_favourite )==1){
      if(Idx_people_favourite==2){
        data_table[Idx_mkt ,smoothed_odd.V2        := marketing_coeff*data_table[Idx_mkt ,marketing_odd.V2] +  (1-marketing_coeff)*data_table[Idx_mkt ,Lehninger_odd.V2]]
        data_table[Idx_mkt ,smoothed_odd.V1        := data_table[Idx_mkt ,Lehninger_odd.V1]]
        data_table[Idx_mkt ,smoothed_odd.V3        := data_table[Idx_mkt ,Lehninger_odd.V3]]
      }else {
        if(Idx_people_favourite==1){
          data_table[Idx_mkt ,smoothed_odd.V1      := marketing_coeff*data_table[Idx_mkt ,marketing_odd.V1] +  (1-marketing_coeff)*data_table[Idx_mkt ,Lehninger_odd.V1]]
          data_table[Idx_mkt ,smoothed_odd.V2      := data_table[Idx_mkt ,Lehninger_odd.V2]]
          data_table[Idx_mkt ,smoothed_odd.V3      := data_table[Idx_mkt ,Lehninger_odd.V3]]
        }else{
          data_table[Idx_mkt ,smoothed_odd.V3      := marketing_coeff*data_table[Idx_mkt,marketing_odd.V3] +  (1-marketing_coeff)*data_table[Idx_mkt,Lehninger_odd.V3]]
          data_table[Idx_mkt ,smoothed_odd.V2      := data_table[Idx_mkt,Lehninger_odd.V2]]
          data_table[Idx_mkt ,smoothed_odd.V1      := data_table[Idx_mkt,Lehninger_odd.V1]]
        }
      }
    }
    data_table[Idx_main_OU,smoothed_odd.V1       := marketing_coeff*data_table[Idx_main_OU ,marketing_odd.V1] +(1-marketing_coeff)*data_table[Idx_main_OU ,Lehninger_odd.V1]]
    data_table[Idx_main_OU,smoothed_odd.V2       := marketing_coeff*data_table[Idx_main_OU ,marketing_odd.V2] +(1-marketing_coeff)*data_table[Idx_main_OU ,Lehninger_odd.V2]]
    
    # marketing_cost_this_iteration
    # data_table[ resultTypeId!="points-more-less-rest",profit_margin_marketing         := stake1*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) +stake3*(1- smoothed_odd.V3/as.numeric(as.character(quote3)))]
    # data_table[ resultTypeId=="points-more-less-rest",profit_margin_marketing         := stake1*(1- smoothed_odd.V1 / as.numeric(as.character(quote1)))+stake2*(1- smoothed_odd.V2/as.numeric(as.character(quote2))) ]
    
    # marketing_profit_this_iteration              <- data_table[,sum(profit_margin_marketing)]
    
  }
  return (data_table)
}
