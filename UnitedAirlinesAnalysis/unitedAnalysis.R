# DATA-550
# Presentation 1 - Part II
# Team: Rajeev Roy & Vaghul Aditya Balaji

library(plyr)
library(gplots)

#################################################################################################################
# Finding United's competitors
#################################################################################################################

# loading in the airline data
air <- read.csv("air.csv")

# Add a columnn for total delay
air$TotalDelay <- air$ArrDelay + air$DepDelay

# Aggregate TotalDelay by Carrier
agg_data <- aggregate(air$TotalDelay, by = list(air$UniqueCarrier), FUN = sum, na.rm = TRUE)

# Replace Column names
colnames(agg_data) <- c("UniqueCarrier", "TotalDelay")

# Join the two dat frames
agg_data <- join(agg_data, count(air, "UniqueCarrier"))

#Sort the data by Frequency
agg_data <- agg_data[order(agg_data$freq),]

# plotting frequency vs delay per flight
agg_data$perFlightDelay <- agg_data$TotalDelay/agg_data$freq
rownames(agg_data) <- agg_data$UniqueCarrier
agg_data$UniqueCarrier <- NULL
plot(agg_data$freq, agg_data$perFlightDelay,
     xlab = "Number of flights",
     ylab = "Average delay per Flight (in mins)",
     xlim = c(0, 1300000), ylim = c(0, 30), pch = " ")
text(agg_data[-which(rownames(agg_data) %in% c("UA","MQ", "DL", "US")), ]$freq + 5000,
     agg_data[-which(rownames(agg_data) %in% c("UA","MQ", "DL", "US")), ]$perFlightDelay,
     rownames(agg_data[-which(rownames(agg_data) %in% c("UA","MQ", "DL", "US")), ]), cex = 0.75, font = 2)
text(agg_data[which(rownames(agg_data) %in% c("UA","MQ", "DL", "US")), ]$freq + 5000,
     agg_data[which(rownames(agg_data) %in% c("UA","MQ", "DL", "US")), ]$perFlightDelay,
     rownames(agg_data[which(rownames(agg_data) %in% c("UA","MQ", "DL", "US")), ]), col = 2, font = 2)
rect(xleft = (agg_data[which(rownames(agg_data) == "US"), ]$freq - 45000), ybottom = (agg_data[which(rownames(agg_data) == "US"), ]$perFlightDelay) - 3,
     xright = (agg_data[which(rownames(agg_data) == "US"), ]$freq + 85000), ytop = (agg_data[which(rownames(agg_data) == "UA"), ]$perFlightDelay) + 3,
     lwd = 0.9, border = 2)
title("Catching the culprit - United Airlines")

#################################################################################################################
# Delay distribution for United against its competitors
#################################################################################################################

delay_aggs <- aggregate(data.frame(air$DepDelay, air$ArrDelay, air$LateAircraftDelay, air$CarrierDelay, air$WeatherDelay, air$SecurityDelay), by = list(air$UniqueCarrier), FUN = sum, na.rm = TRUE)
colnames(delay_aggs) <- c("UniqueCarrier", "Departure Delay", "Arrival Delay", "Late Aircraft Delay", "Carrier Delay", "Weather Delay", "Security Delay")
rownames(delay_aggs) <- delay_aggs$UniqueCarrier
delay_aggs$UniqueCarrier <- NULL
delay_aggs <- delay_aggs[c('UA', 'DL', 'US', 'MQ'), ]
for (i in 1:nrow(delay_aggs))
{
  carrier <- rownames(delay_aggs)[i]
  flight_freq <- agg_data[carrier, ]$freq
  delay_aggs[i, ] <- delay_aggs[i, ]/flight_freq
}
barplot(t(t(delay_aggs)), beside = TRUE, legend = TRUE, ylab = "Delay per flight (in mins)", xlab = "Delay Type",
        axes = FALSE, axisnames = FALSE, main = "Delay distribution for competing airlines", ylim = c(0, 14),
        col = c("orangered4", "midnightblue", "goldenrod3", "chartreuse4"))
axis(side = 2, labels = seq(0, 14, 2), at = seq(0, 14, 2), cex = 0.75, las = 2)
angleAxis(side = 1, at = seq(4, 29, 5), labels = colnames(delay_aggs), srt = 45, cex = 0.75)
box()

#################################################################################################################
# Departure delay per flight per airport for United against its competitors
#################################################################################################################

dep_by_airport <- aggregate(air$DepDelay, by=list(air$Origin, air$UniqueCarrier), FUN = sum, na.rm = TRUE)
colnames(dep_by_airport) <- c("Origin", "Carrier", "DepDelay")
counts <- count(air, c("Origin", "UniqueCarrier"))
colnames(counts) <- c("Origin","Carrier", "freq")
dep_by_airport <- join(dep_by_airport, counts)
dep_by_airport <- dep_by_airport[order(dep_by_airport$DepDelay, decreasing = "TRUE" ), ]
dep_by_airport$perFlightDelay <- dep_by_airport$DepDelay/dep_by_airport$freq
dep_airports <- head(dep_by_airport[dep_by_airport$Carrier == "UA", ], 5)
dep_airports <- as.character(dep_airports$Origin)

