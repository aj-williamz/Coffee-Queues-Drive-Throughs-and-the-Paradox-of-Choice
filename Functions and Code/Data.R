HMSArrivals = data.matrix(read.csv('ArrivalTimes', header = FALSE))
Arrivals = HMSArrivals%*%c(3600, 60, 1)


HMSDepartures = data.matrix(read.csv('DepartureTimes.txt', header = FALSE))
Departures = HMSDepartures%*%c(3600, 60, 1)


#Calculate Service Times
ServiceTimes = numeric(0)
for(i in 1:length(Arrivals)){
  ServiceTimes[i] = Departures[i] - max(Arrivals[i],Departures[i-1])
}
SD = gofTest(ServiceTimes, test = 'sw', distribution = 'exp')
hist(ServiceTimes, freq = FALSE, col = colors[c(1,2,7,8,11,12,13)])
#S = seq(from = 1, to = 350, by = 1)
lines(S, dexp(S, rate = SD$distribution.parameters))
legend(x = 100, y = 0.008, 
      legend = sprintf('X~exp(%f), W = %f',SD$distribution.parameters, SD$statistic))



#Fit Distribution To Interarrival

InterArrivals = diff(Arrivals)
InterArrivals = as.vector(InterArrivals)
AD = gofTest(InterArrivals, test = 'sw', distribution = 'exp')
hist(InterArrivals, freq = FALSE, col = rev(colors)[4:11])
S = seq(from = 1, to = 350, by = 1)
lines(S, dexp(S, rate = AD$distribution.parameters))
legend(x = 100, y = 0.008, 
       legend = sprintf('X~exp(%f), W = %f',AD$distribution.parameters, AD$statistic))

Prices = read.csv('Prices', header = F)
Prices = Prices$V1

Probs = read.csv('Probs', header = F)
Probs = Probs$V1


#Hicks Law
b = (1/SD$distribution.parameters)/log2(45)

Quants = read.csv('Quants', header = F)
Quants = Quants$V1

items = as.vector(read.csv("items", header = F))
items = items$V1

colors = read.csv("Colors", header = F)
colors = colors$V1

install_tinytex()