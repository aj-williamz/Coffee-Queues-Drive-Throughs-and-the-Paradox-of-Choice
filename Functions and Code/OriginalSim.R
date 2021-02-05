OriginalSim = function(ArrRate, ServRate, NumSec, NumDays, IV){

Sim = list(0)
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


LostProfit = 
  sum(sample(c(0,1), prob = c(IV[1], 1 - IV[1]), sum(queues >= 15 &  queues <= 16), replace = T))
+ sum(sample(c(0,1), prob = c(IV[2], 1 - IV[2]), sum(queues >= 17 &  queues <= 18), replace = T))
+ sum(sample(c(0,1), prob = c(IV[3], 1 - IV[3]), sum(queues >= 19 &  queues <= 20), replace = T))
+ sum(sample(c(0,1), prob = c(IV[4], 1 - IV[4]), sum(queues >= 21 &  queues <= 22), replace = T))
+ sum(sample(c(0,1), prob = c(IV[5], 1 - IV[5]), sum(queues >= 22), replace = T))


OriginalProfit = sum(Purchases(NumJobs - LostProfit))


return(list(OriginalProfit, mean(WaitAvg), TotalWait, mean(QueueAvg), queues))
}

