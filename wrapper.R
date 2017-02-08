#' Compute reference odds
#' @param Asian handicap and over/Under, match info
#' @importFrom compiler cmpfun
#' 
wrapper = function(eventStartInMinutesAgo, redCards, matchStatus, playingMinute, hcpParameter, hcpHome, hcpAway, ouParameter, ouOver, ouUnder, homeGoals, awayGoals){
  
  hcpOdds = c(hcpHome, hcpAway)
  ouOdds = c(ouOver, ouUnder)
  # (0) set up
  q <- INWTLive:::quoten (
    spielID          = 123,
    system.time      = Sys.time(),                                             #Point in time at which you look at odds
    event.start.time = as.POSIXlt(Sys.time()-playingMinute*60),                #Kick off NEEDS TO BE SET FOR EVENT
    sql.credentials  = list(),                                                 #Not needed
    provider         = c(30, 31, 34, 35, 40, 62, 63, 64),                      #Not necesairy for transformation refers to Betbrain
    pregame          = FALSE
  )
  
  
  # ------------------------------------------------------------------------------
  
  # (1) input
  
  datHcp <- function(hcpParameter, hcpOdds, section) {
    data.frame (
      resultTypeId     = if (section == "first") {
        rep(paste("asian-handicap-halftime"), 2)
      } else {
        rep(paste("asian-handicap"), 2)
      },
      fixedParam       = rep(hcpParameter, 2),
      choiceParam      = as.character(1:2),
      quote            = hcpOdds,
      stringsAsFactors = FALSE) 
  }
  
  datOU <- function(ouParameter, ouOdds, section) {
    data.frame (
      resultTypeId     = if(section == "first") {
        rep(paste("asian-points-more-less-halftime"), 2)
      } else {
        rep(paste("asian-points-more-less-than"), 2)
      },
      fixedParam       = rep(ouParameter, 2),
      choiceParam      = c("+", "-"),
      quote            = ouOdds,
      stringsAsFactors = FALSE)
  }
  
  section      <- "all" 
  
  input <- rbind (
    datHcp(hcpParameter, hcpOdds, section),
    datOU(ouParameter, ouOdds, section)
  )
  
  input$sourceProviderId <- 31                                     
  input$updated          <- as.character(Sys.time())    #IMPORTANT, last odd update
  input$marketType       <- "NORMAL"
  input$indikator        <- 0                           #add 1 instead of 0 if markets are closed
  
  input <- input[, c (
    "sourceProviderId",
    "updated",
    "resultTypeId",
    "fixedParam",
    "marketType",
    "choiceParam",
    "quote",
    "indikator"
  )]
  
  q@input <- new("input", state = "dbInput", input)
  
  # ------------------------------------------------------------------------------
  
  # (2) events
  time = c(0)
  type = c("bos")
  participantNo = c(0)
  value = c(1)
  
  while (homeGoals > 0 ) {
    time = append(time, 1)
    type = append(type, "point")
    participantNo = append(participantNo, 1)
    value = append(value,1)
    homeGoals = homeGoals - 1
  }
  
  while (awayGoals > 0 ) {
    time = append(time, 1)
    type = append(type, "point")
    participantNo = append(participantNo, 2)
    value = append(value,1)
    awayGoals = awayGoals - 1
  }
  if(matchStatus == 3){
    value = append(value,c(0,0))
    type = append(type, c("eos", "bos"))
    time = append(time,c(45*60000, ((eventStartInMinutesAgo - playingMinute - 45) * 1000 * 60)))
  }
  
  q@events <- data.frame (
    time          = time,       #time in ms
    participantNo = participantNo,                       #event for home 1 away 2, 0 is begin of section
    type          = type,  #bos= begin of section, point = goal, red-card eos eoe
    value         = value
  )
  # ------------------------------------------------------------------------------
  
  # (3) injury time
  q@extratime <- data.frame(HZ1extratime = 1.41, HZ2extratime = 3.47)
  
  # ------------------------------------------------------------------------------
  
  # (4) use simulated annealing?
  q@Korrektur <- FALSE                                  #Keep FALSE
  
  # ------------------------------------------------------------------------------
  
  # (5) log input
  q@logData <- q@input      #To log input data
  
  # ------------------------------------------------------------------------------
  
  # (6) calc odds 
  isInputGood <- function(q) 
    (nrow(q@input) > 0) & (nrow(q@events) > 0)        #actual calculation, q@output created all the output, 0.9 is NA
  
  if (isInputGood(q)) {
    q <- INWTLive:::checkReferenceClasses(q)
    q@disableEventWindow <- TRUE
    q <- INWTLive:::transformData(q)
    q <- INWTLive:::makeOutput(q)
  }
  
  data=q@output
  if(playingMinute==0){
    data=data[which(data$marketType=="asian4"),]
  }else{
    data=data[which(data$marketType=="asian3"),]
  }
  data=data[which(data$sourceProviderId==30),]
  
  # #INSERT FILTER CRITERIA HERE ON DATA
  # standard=data[which(data$resultTypeId=="standard-rest"),]
  # #standard=concat.split.multiple(standard, "quotes", "/")
  
  # over=data[which(data$resultTypeId=="points-more-less-rest"),]
  # #over=concat.split.multiple(over, "quotes", "/")
  # if(sum(over$quotes_1<2.7)==0){
  #   over=over[which(over$fixedParam==0.5),]
  # }else{
  #   over=over[which(over$quotes_1<2.7),]
  #   over=over[which(over$quotes_1>1.4),]
  # }

  desiredMarkets = c("standard", "standard-rest", "double-chance", "next-point" )
  #param                        <- unlist(strsplit(data_table [k,fixedParam],":"))
   A                            <- (strsplit(data$fixedParam,":"))
  indx                          <- sapply(A,length)
  param_df                     <- do.call(rbind,lapply(A, `length<-`,max(indx)))

  mask = data$resultTypeId%in%desiredMarkets | data$resultTypeId=="points-more-less-rest"& data$fixedParam<=2.5  | data$resultTypeId=="handicap-rest"& param_df[,1]<=2& param_df[,2]<=2
  data = data[which(mask),]

  return(data)
}  
wrapper = compiler::cmpfun(wrapper)