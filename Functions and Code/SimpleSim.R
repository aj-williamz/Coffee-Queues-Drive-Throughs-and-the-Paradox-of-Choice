SimpleSim = function(RemoveItem, ImpatienceVector, ValueOfTime, OGWait){
  ServRate = 1/(b*log2(45 - Quants[RemoveItem]))
  NumSec = 2*60*60 #(2 h, 5 d/w, 50 w/y, 24 h/d, 60 m/h, 60 s/m)
  
  ##Run the Simulation and Empty the Queue every 2 hours## 
  
  NumDays = 5*50
  
  WaitAvg = numeric(0)
  TotalWait = 0
  QueueAvg = numeric(0)
  NumJobs = 0
  queues = numeric(0)
  
  for(i in 1:NumDays){
    Sim[[i]] = Myssq3(ArrRate, ServRate, NumSec)
    WaitAvg = c(WaitAvg,Sim[[i]][[1]])
    TotalWait = TotalWait + Sim[[i]][[2]]
    QueueAvg = c(QueueAvg,Sim[[i]][[3]])
    NumJobs = NumJobs + Sim[[i]][[4]]
    queues = c(queues,Sim[[i]][[5]])
  }  
  
  IV= rev(c(0.5, 0.7, 0.8, 0.9, 0.95))
  LostProfit = 
    sum(sample(c(0,1), prob = c(IV[1], 1 - IV[1]), sum(queues >= 15 &  queues <= 16), replace = T))
  + sum(sample(c(0,1), prob = c(IV[2], 1 - IV[2]), sum(queues >= 17 &  queues <= 18), replace = T))
  + sum(sample(c(0,1), prob = c(IV[3], 1 - IV[3]), sum(queues >= 19 &  queues <= 20), replace = T))
  + sum(sample(c(0,1), prob = c(IV[4], 1 - IV[4]), sum(queues >= 21 &  queues <= 22), replace = T))
  + sum(sample(c(0,1), prob = c(IV[5], 1 - IV[5]), sum(queues >= 22), replace = T))
  
  P = Purchases(NumJobs - LostProfit)
  Profit = sum(P[P != Prices[RemoveItem]]) + abs(OGWait - TotalWait)*ValueOfTime
  
  return(list(Profit, mean(WaitAvg), TotalWait, mean(QueueAvg), queues))
}