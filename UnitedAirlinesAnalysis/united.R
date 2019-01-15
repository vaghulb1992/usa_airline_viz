lib(plyr)
air <- read.csv("air.csv")
#Add a columnn for total delay
air$TotalDelay<-air$ArrDelay+air$DepDelay
#Aggregate TotalDelay by Carrier
agg_data<-aggregate(air$TotalDelay, by=list(air$UniqueCarrier), FUN=sum, na.rm=TRUE)
#Replace Column names
colnames(agg_data)<-c("UniqueCarrier", "TotalDelay")
#Get total number of flights per carrier
df<-count(air, "UniqueCarrier")
#Join the two dat frames
agg_data<-join(agg_data, df)
#Sort the data by Frequency
agg_data<-agg_data[order(agg_data$freq),]
agg_data$perFlightDelay<-agg_data$TotalDelay/agg_data$freq
plot(agg_data$freq, agg_data$perFlightDelay,
     xlab="Number of Flights",
     ylab="Average Delay per Flight (minutes)",
     xlim=c(0,1300000), ylim=c(0,30))
#text(agg_data$freq+50000, agg_data$perFlightDelay, agg_data$UniqueCarrier)
com_data<-subset(agg_data, agg_data$UniqueCarrier!=c("UA","MQ", "DL", "US"))
text(com_data$freq+50000, com_data$perFlightDelay, com_data$UniqueCarrier)
title("Total Flight Delay Comparision")
mtext('Benchmarking American Airlines', side=3, line=0)
aa_data<-agg_data[agg_data$UniqueCarrier=="UA",]
text(aa_data$freq +50000, aa_data$freq, "BA", col = "Red")
