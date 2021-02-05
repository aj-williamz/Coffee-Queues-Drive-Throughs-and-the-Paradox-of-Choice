###PARAMETERS###

ArrRate = AD$distribution.parameters
ServRate = SD$distribution.parameters
NumSec = 2*60*60
IV= rev(c(0.005, 0.01, 0.02, 0.04, 0.05))

ValueOfTime = 24000/(525600*60)
NumDays = 5*50
OGSimOut = OriginalSim(ArrRate, ServRate, NumSec, NumDays, IV)
OGWait = OGSimOut[[3]]
#IV = rev(c(0.5, 0.7, 0.8, 0.9, 0.95))

AdjProfit = numeric(0)


###PROFITS###

for(i in 1:15){
AdjMenu = SimpleSim(i, IV, ValueOfTime, OGWait)
AdjProfit[i] = AdjMenu[[1]]
}
vals = round(c(OGSimOut[[1]], AdjProfit)) 
op <- par(mar = c(5,7,4,2) + 0.1)
bp = barplot(c(OGSimOut[[1]], AdjProfit), main="Revenue ($) vs. Item Removed", horiz=T,
        names.arg=c("OriginalMenu", items), las = 2, col = colors)
text(0, bp, vals,pos=4)


###Wait vs. Profit###
Adjwait = numeric(0)
for(i in 1:15){
  AdjMenu = SimpleSim(i, ImpatienceVector, ValueOfTime, OGWait)
  Adjwait[i] = AdjMenu[[3]]
}



plot(c(OGWait, Adjwait ), c(OGSimOut[[1]], AdjProfit), xlab = 'Total Wait Time', ylab = 'Revenue ($)', main = 'Total Wait Time vs. Revenue')
text(c(OGWait, Adjwait )[1:5], c(OGSimOut[[1]], AdjProfit)[1:5], labels=c('Original', items[1:4]), font=1)


###Wait vs.  Queue###
AdjQueue = numeric(0)
for(i in 1:15){
  AdjMenu = SimpleSim(i, ImpatienceVector, ValueOfTime, OGWait)
  AdjQueue[i] = AdjMenu[[4]]
}
plot(c(OGSimOut[[4]], AdjQueue ), c(OGSimOut[[1]], AdjProfit), xlab = 'Average Queue Length', ylab = 'Revenue ($)', main = 'Average Queue Length vs. Revenue')
text(c(OGSimOut[[4]], AdjQueue )[1:5], c(OGSimOut[[1]], AdjProfit)[1:5], labels=c('Original',items[1:4]), font=1)

l###Queue Distributions
Queues = list(0)
for(i in 1:15){
  AdjMenu = SimpleSim(i, ImpatienceVector, ValueOfTime, OGWait)
  Queues[[i]] = AdjMenu[[5]]
}

sum(Queues[[1]]>15)



par(mfrow = c(1,5))
hist(OGSimOut[[5]], freq = F, breaks = 10, xlab = "Density", ylab = "Queue Length", main = 'Original Menu')
hist(Queues[[1]], freq = F, breaks = 10,xlab = "Density", ylab = "Queue Length", main = 'No Coffee')
hist(Queues[[6]], freq = F, breaks = 10,xlab = "Density", ylab = "Queue Length", main = 'No Iced Espresso')
hist(Queues[[14]], freq = F, breaks = 10,xlab = "Density", ylab = "Queue Length", main = 'No Lattes (Other)')
hist(Queues[[3]], freq = F, breaks = 10,xlab = "Density", ylab = "Queue Length", main = 'No Sandwhiches')



