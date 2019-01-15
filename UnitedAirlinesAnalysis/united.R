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
plot(agg_data$freq, agg_data$perFlightDelay)
text(agg_data$freq, agg_data$perFlightDelay, agg_data$UniqueCarrier)
