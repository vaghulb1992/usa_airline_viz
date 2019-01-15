# DATA-550
# Presentation 1 - Part II
# Team: Rajeev Roy & Vaghul Aditya Balaji

library(plyr)
library(gplots)

#################################################################################################################
# Finding United's competitors
#################################################################################################################

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

#################################################################################################################
# Delay distribution for United against its competitors
#################################################################################################################

delay_aggs <- aggregate(data.frame(air$DepDelay, air$ArrDelay, air$LateAircraftDelay, air$CarrierDelay, air$WeatherDelay, air$SecurityDelay), by = list(air$UniqueCarrier), FUN = sum, na.rm = TRUE)
colnames(delay_aggs) <- c("UniqueCarrier", "Departure Delay", "Arrival Delay", "Late Aircraft Delay", "Carrier Delay", "Weather Delay", "Security Delay")
rownames(delay_aggs) <- delay_aggs$UniqueCarrier
delay_aggs$UniqueCarrier <- NULL
delay_aggs <- delay_aggs[c('UA', 'DL', 'US', 'MQ'), ]
barplot(t(t(delay_aggs)), beside = TRUE, legend = TRUE, ylab = "Delay (in mins)", xlab = "Delay Type",
        axes = FALSE, axisnames = FALSE, main = "Delay distribution for competing airlines", ylim = c(0, 7000000),
        col = c("orangered4", "midnightblue", "goldenrod3", "chartreuse4"))
axis(side = 2, labels = FALSE)
options(scipen=999)
angleAxis(side = 2, labels = seq(0, 7000000, 1000000)/1000, at = seq(0, 7000000, 1000000), srt = 45, cex = 0.75)
angleAxis(side = 1, at = seq(4, 29, 5), labels = colnames(delay_aggs), srt = 45, cex = 0.75)
box()

#################################################################################################################
# Section III
#################################################################################################################

