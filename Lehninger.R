#' Get lehninger odds
#' @rdname Lehninger
#' @importFrom compiler cmpfun
#' @export Lehninger 

#########################

Lehninger <- function(quoten){
  c2 = 50 
  c3 = 1000
  c4 = 0.8
  Zo     <- tail(quoten,n=1)
  quoten <- head(quoten,-1)
  quoten <- quoten[!is.na(quoten)]
  # Zo .. 1.08 entspricht Markt mit 8% Marge
  # c2, c3, c4 ... Parameter von Lehninger
  
  #quoten <- unlist(list(...))
  n <- length(quoten)
  c1 <- 1000 / 3 * n
  g <- quoten - 1
  
  ### Test zu Beginn: 
  # Ist der %-Wettmarkt auf den geaendert werden soll unterschiedlich vom 
  # urspruenglichen?
  
  ### Berechnung der Startwerte fuer die Iteration:
  # Bestimme Ursprungs-%-Wettmarkt:
  Uo <- sum(1 / quoten)
  # Zo auf eine maximale Aenderung von Uo auf c4 beschraenken:
  # if(Zo > Uo) {
  #   Zo <- min(Uo + c4, Zo) 
  # } else {
  #   Zo <- min(Uo - c4, Zo) 
  # }
  # Bestimme Ziel-%-Aenderung pro Schritt:
  ps <- (Zo - Uo) / c2
  # Bestimme die Faktoren fuer die Quotenaenderung:
  Fq <- 1 + (1 + quoten / c1) * abs(Zo - Uo) / c2 * 10
  # Bestimme die neuen Quoten nach diesem Iterationsschritt:
  if(Zo < Uo) {
    qNeu <- pmin(g * Fq + 1, c3) 
  } else {
    qNeu <- pmin(g / Fq + 1, c3) 
  }
  gNeu <- qNeu - 1
  # Bestimme tatsaechliche %-Aenderung pro Schritt:
  oNeu <- sum(1 / qNeu)
  pt <- oNeu - Uo
  # Bestimme Wert mit dem der Faktor potenziert werden soll.
  po <- ps / pt
  
  ### Iterationsschleife
  for(i in 1:c2){
    # Abbruchbedingung: Entweder nach c2+3 Schritten oder wenn Zo auf 0,01% 
    # genau erreicht wurde und eine Maximalschrittanzahl nicht ueberschritten 
    # wurde.
    if(i == c2 + 3 | abs(sum(1 / qNeu) - Zo) < 0.0001) break
    
    # Bestimme die Faktoren fuer die Quotenaenderung und
    # Bestimme die neuen Quoten nach diesem Iterationsschritt (o berechnet 
    # sich aus den Quoten des letzten Iterationsschrittes):
    if(i == 1){
      Fq <- 1 + (1 + quoten / c1) * abs(Zo - Uo) / c2 * 10
      o <- Uo
      gNeu <- g
    } else{
      Fq <- 1 + (1 + qNeu / c1) * abs(Zo - Uo) / c2 * 10
      o <- sum(1 / qNeu)
      gNeu <- qNeu - 1
    }
    if(Zo < o) {
      qNeu <- pmin(gNeu * Fq ^ po + 1, c3) 
    } else {
      qNeu <- pmin(gNeu / Fq ^ po + 1, c3) 
    }
    # Bestimme tatsaechliche %-Aenderung pro Schritt:
    oNeu <- sum(1 / qNeu)
    pt <- oNeu - o
    # Bestimme Wert mit der der naechste Faktor potenziert werden soll.
    if(abs(pt) > 0){
      po <- ps / pt * po * pmin(1, abs((Zo - oNeu) / ps))
    } else{
      po <- 0
    }
  }
  return(qNeu)
}
Lehninger=compiler::cmpfun(Lehninger)
