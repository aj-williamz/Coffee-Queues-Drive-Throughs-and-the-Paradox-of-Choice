#ArrRate: Rate of exponential arrivals
#ServRate: Rate of exponential Service times 

Myssq3 = function(ArrRate, ServRate, NumSec){
ti = 0.0 #initial time 
tf = NumSec #total time of simulation (doors close) minus a bit to fix indexing
Inft = (100*tf) #Some arbitrary large number 

#Initialize job statistics 

tinn = 0  #time integrated number in node 
tinq = 0  #time integrated number in queue 
tins = 0  #time integrated number in service 

#Initialize Time Statistics 

tarr = -1       #next arrival time 
tcomp = -1    #next completion time 
tcurr = -1    #current time 
tnext = -1    #next event time 
tlast = -1    #last arrival time

#initialize vector of stored departure times

DepTimes = numeric(0)
QueueArea = numeric(0)
QueueVector = numeric(0)

#Initialize Counters for departed jobs and number of jobs in node 
i = 0 
n = 0 


#Functions to get next arrival and next service time 
#getComparr <- function(n)
#{
#     return(ComparrivalTimes[n])
#}


# Extract the "next service time" function for input into ssq3.
#getAdjCompService <- function(n)
#{
#      return(AdjCompService[n])
#}

#Set the first clock
tcurr = ti                  #set the clock
tarr = 0 #Schedule the first arrival 
tcomp = Inft                #The first event can't be a completion



while(tarr < tf || n > 0){

  tnext = min(tarr, tcomp)  #grab the next event, either arrival or completion 
  if(n > 0){
    tinn = tinn + (tnext - tcurr)*n
    tinq = tinq + (tnext - tcurr)*(n-1)
    QueueVector[n+i] = n-1
    QueueArea[n+i] = tinq
    tins = tins + (tnext - tcurr)
  }
  
  tcurr = tnext #advance the clock 
  
  if(tcurr == tarr){  #if we have an arrival,
    n = n + 1         #add one to the node 
    tarr = tarr + rexp(1, ArrRate) #get the next interarrivalarrival times
    if(tarr > tf){    #do not process jobs after the door has closed 
      tlast = tcurr   #end the clock as the last job arrives 
      tarr = Inft     #the next event must be a completion (which can happen after doors closed)
    }
    
    if(n == 1){
      tcomp = tcurr + rexp(1, ServRate)  #get completion time for first person into system
    }
    
  }else{
    i = i + 1
    if(tcomp < Inft){DepTimes[i] = tcomp} #store the dep
    n = n-1
    
    if(n > 0){
      tcomp = tcurr + rexp(1, ServRate)
    }else{
      tcomp = Inft
    }
  }
}
#Average Wait
#Cumulative Wait Time 
#Average Number in Queue
#Number of Jobs total
#Queue 



return(list(tinn/i, tinn, tinq/tcurr, i, QueueVector))
}
