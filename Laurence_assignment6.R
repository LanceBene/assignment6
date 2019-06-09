# Das Paket HMM wird aufgerufen
library("HMM")
# Die Zahl der Würfe wird als 2000 definiert
nSim = 2000
# Die beiden States Fair und Unfair werden definiert
States = c("Fair", "Unfair")
# Der Würfel wird als sechsseitig definiert
Symbols = 1:6
# Die Matrix transprobs wird definiert, mit den Wahrscheinlichkeiten, welcher State als nächstes folgt
transProbs = matrix(c(0.99, 0.01, 0.02, 0.98), c(length(States),
                                                 length(States)), byrow = TRUE)
# Die Matrix emissionProbs wird definiert, zunächst für den fairen Würfel als 1/6, dann für den gezinkten Würfel
emissionProbs = matrix(c(rep(1/6, 6), c(rep(0.1, 5), 0.5)),
                       c(length(States), length(Symbols)), byrow = TRUE)
# Das Hidden Markov Model wird mit den zuvor definierten Parametern initialisiert
hmm = initHMM(States, Symbols, transProbs = transProbs, emissionProbs =
                emissionProbs)
# Ein Pfad von Zuständen und Beobachtungen wird simuliert
sim = simHMM(hmm, nSim)
# Mit dem Viterbi-Algorithmus wird für die Beobachtung der wahrscheinlichste Pfad errechnet
vit = viterbi(hmm, sim$observation)
# Die Wahrscheinlichkeit für die jeweiligen Zustände wird errechnet
f = forward(hmm, sim$observation)
# In i und j werden die Wahrscheinlichkeiten der jeweiligen Zustände, gegeben die Anzahl der Wiederholungen gespeichert
i <- f[1, nSim]
j <- f[2, nSim]
# Die Wahrscheinlichkeit der Beobachtungen wird errechnet
probObservations = (i + log(1 + exp(j - i)))
#########################################
## NO MORE DOCUMENTATION BELOW THIS LINE
#########################################
x = list(hmm = hmm, sim = sim, vit = vit)
# PLOT simulated throws at top #####################
mn = "Fair and unfair die"
xlb = "Throw nr."
ylb = ""
plot(x$sim$observation, ylim = c(-7.5, 6), pch = 3, main = mn,
     xlab = xlb, ylab = ylb, bty = "n", yaxt = "n")
axis(2, at = 1:6)
# PLOT Simulated, which die was used (ground truth) ###########
text(0, -1.2, adj = 0, cex = 0.8, col = "black", "True: green = fair die")
for (i in 1:nSim) {
  if (x$sim$states[i] == "Fair")
    rect(i, -1, i + 1, 0, col = "green", border = NA)
  else rect(i, -1, i + 1, 0, col = "red", border = NA)
}
# PLOT Most probable path (viterbi) #######################
text(0, -3.2, adj = 0, cex = 0.8, col = "black", "Most probable path")
for (i in 1:nSim) {
  if (x$vit[i] == "Fair")
    rect(i, -3, i + 1, -2, col = "green", border = NA)
  else rect(i, -3, i + 1, -2, col = "red", border = NA)
}
# PLOT Differences ####################
text(0, -5.2, adj = 0, cex = 0.8, col = "black", "Difference")
differing = !(x$sim$states == x$vit)
for (i in 1:nSim) {
  if (differing[i])
    rect(i, -5, i + 1, -4, col = rgb(0.3, 0.3, 0.3),
         border = NA)
  else rect(i, -5, i + 1, -4, col = rgb(0.9, 0.9, 0.9),
            border = NA)
}
  
## Aufgabe 3
  
  library("HMM")
  library("dplyr")
  nSim = 2000
  States = c("Noncoding", "Coding")
  Symbols = c("A","T","C","G")
  transProbs = matrix(c(0.99, 0.01, 0.01, 0.99), c(length(States),
                                                   length(States)), byrow = TRUE)
  emissionProbs = matrix(c(rep(1/4, 4), c(0.1, 0.1, 0.4, 0.4)),
                         c(length(States), length(Symbols)), byrow = TRUE)
  hmm = initHMM(States, Symbols, transProbs = transProbs, emissionProbs =
                  emissionProbs)
  sim = simHMM(hmm, nSim)
  vit = viterbi(hmm, sim$observation)
  f = forward(hmm, sim$observation)
  i <- f[1, nSim]
  j <- f[2, nSim]
  probObservations = (i + log(1 + exp(j - i)))
  
  #########################################
  x = list(hmm = hmm, sim = sim, vit = vit)
  # PLOT simulated throws at top #####################
  mn = "Noncoding and coding seq"
  xlb = "Base nr."
  ylb = ""
  NumberAsPattern <- recode(x$sim$observation, "A"=0, "T"=1, "C"=2, "G"=3)
  plot(NumberAsPattern, 
       ylim = c(-7.5, 6),
       pch = 4, main = mn,
       xlab = xlb, ylab = ylb, bty = "n", yaxt = "n")
  axis(2, at = 0:3, labels = Symbols, line=0,2)
  # PLOT Simulated, which die was used (ground truth) ###########
  text(0, -0.7, adj = 0, cex = 0.8, col = "black", "Green = Noncoding seq")
  for (i in 1:nSim) {
    if (x$sim$states[i] == "Noncoding")
      rect(i, -2, i + 1, -1, col = "green", border = NA)
    else rect(i, -2, i + 1, -1, col = "red", border = NA)
  }
  # PLOT Most probable path (viterbi) #######################
  text(0, -2.7, adj = 0, cex = 0.8, col = "black", "Most probable path")
  for (i in 1:nSim) {
    if (x$vit[i] == "Noncoding")
      rect(i, -4, i + 1, -3, col = "green", border = NA)
    else rect(i, -4, i + 1, -3, col = "red", border = NA)
  }
  # PLOT Differences ####################
  text(0, -4.7, adj = 0, cex = 0.8, col = "black", "Difference")
  differing = !(x$sim$states == x$vit)
  for (i in 1:nSim) {
    if (differing[i])
      rect(i, -6, i + 1, -5, col = rgb(0.3, 0.3, 0.3),
           border = NA)
    else rect(i, -6, i + 1, -5, col = rgb(0.9, 0.9, 0.9),
              border = NA)
  }
  
## Aufgabe 3 mit anderen Parametern
  
  library("HMM")
  library("dplyr")
  nSim = 2000
  States = c("Noncoding", "Coding")
  Symbols = c("A","T","C","G")
  transProbs = matrix(c(0.95, 0.05, 0.05, 0.95), c(length(States),
                                                   length(States)), byrow = TRUE)
  emissionProbs = matrix(c(rep(1/4, 4), c(0.15, 0.15, 0.35, 0.35)),
                         c(length(States), length(Symbols)), byrow = TRUE)
  hmm = initHMM(States, Symbols, transProbs = transProbs, emissionProbs =
                  emissionProbs)
  sim = simHMM(hmm, nSim)
  vit = viterbi(hmm, sim$observation)
  f = forward(hmm, sim$observation)
  i <- f[1, nSim]
  j <- f[2, nSim]
  probObservations = (i + log(1 + exp(j - i)))
  
  #########################################
  x = list(hmm = hmm, sim = sim, vit = vit)
  # PLOT simulated throws at top #####################
  mn = "Noncoding and coding seq"
  xlb = "Base nr."
  ylb = ""
  NumberAsPattern <- recode(x$sim$observation, "A"=0, "T"=1, "C"=2, "G"=3)
  plot(NumberAsPattern, 
       ylim = c(-7.5, 6),
       pch = 4, main = mn,
       xlab = xlb, ylab = ylb, bty = "n", yaxt = "n")
  axis(2, at = 0:3, labels = Symbols, line=0,2)
  # PLOT Simulated, which die was used (ground truth) ###########
  text(0, -0.7, adj = 0, cex = 0.8, col = "black", "Green = Noncoding seq")
  for (i in 1:nSim) {
    if (x$sim$states[i] == "Noncoding")
      rect(i, -2, i + 1, -1, col = "green", border = NA)
    else rect(i, -2, i + 1, -1, col = "red", border = NA)
  }
  # PLOT Most probable path (viterbi) #######################
  text(0, -2.7, adj = 0, cex = 0.8, col = "black", "Most probable path")
  for (i in 1:nSim) {
    if (x$vit[i] == "Noncoding")
      rect(i, -4, i + 1, -3, col = "green", border = NA)
    else rect(i, -4, i + 1, -3, col = "red", border = NA)
  }
  # PLOT Differences ####################
  text(0, -4.7, adj = 0, cex = 0.8, col = "black", "Difference")
  differing = !(x$sim$states == x$vit)
  for (i in 1:nSim) {
    if (differing[i])
      rect(i, -6, i + 1, -5, col = rgb(0.3, 0.3, 0.3),
           border = NA)
    else rect(i, -6, i + 1, -5, col = rgb(0.9, 0.9, 0.9),
              border = NA)
  }
  
  