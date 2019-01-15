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
# Departure and arrival delays at ORD and DEN airports for United airlines against its competitors
#################################################################################################################

# obtaining the departure delays per airport per airline
dep_by_airport <- aggregate(air$DepDelay, by=list(air$Origin, air$UniqueCarrier), FUN = sum, na.rm = TRUE)
colnames(dep_by_airport) <- c("Origin", "Carrier", "DepDelay")
counts <- count(air, c("Origin", "UniqueCarrier"))
colnames(counts) <- c("Origin","Carrier", "freq")
dep_by_airport <- join(dep_by_airport, counts)
dep_by_airport <- dep_by_airport[order(dep_by_airport$DepDelay, decreasing = "TRUE" ), ]
dep_by_airport$perFlightDelay <- dep_by_airport$DepDelay/dep_by_airport$freq
comp_airports <- as.character(head(dep_by_airport[dep_by_airport$Carrier == "UA", ], 2)$Origin)

# obtaining the arrival delays per airport per airline
arr_by_airport <- aggregate(air$ArrDelay, by=list(air$Dest, air$UniqueCarrier), FUN = sum, na.rm = TRUE)
colnames(arr_by_airport) <- c("Dest", "Carrier", "ArrDelay")
counts <- count(air, c("Dest", "UniqueCarrier"))
colnames(counts) <- c("Dest","Carrier", "freq")
arr_by_airport <- join(arr_by_airport, counts)
arr_by_airport <- arr_by_airport[order(arr_by_airport$ArrDelay, decreasing = "TRUE" ), ]
arr_by_airport$perFlightDelay <- arr_by_airport$ArrDelay/arr_by_airport$freq
comp_airports <- as.character(head(arr_by_airport[arr_by_airport$Carrier == "UA", ], 2)$Dest)

# barplot for ORD airport
ord_airlines <- as.character(head(subset(dep_by_airport, dep_by_airport$Origin == "ORD"), 5)$Carrier)
ord_dep_subset <- subset(dep_by_airport, dep_by_airport$Carrier %in% ord_airlines & dep_by_airport$Origin == "ORD")
ord_arr_subset <- subset(arr_by_airport, arr_by_airport$Carrier %in% ord_airlines & arr_by_airport$Dest == "ORD")
ord_data <- data.frame(DepDelay = ord_dep_subset[order(ord_dep_subset$Carrier), ]$perFlightDelay,
                       ArrDelay = ord_arr_subset[order(ord_arr_subset$Carrier), ]$perFlightDelay)
rownames(ord_data) <- ord_airlines[order(ord_airlines)]
barplot(t(t(ord_data)), beside = TRUE, legend = TRUE,  ylab = "Delay per flight (in mins)",
        xlab = "Delay Type", axes = FALSE, axisnames = FALSE, ylim = c(0, 24),
        col = c("orangered4", "midnightblue", "goldenrod3", "grey37", "chartreuse4"), 
        main = "ORD airport delay distribution per airline (Top 5)",
        args.legend = list(x="bottomright"))
axis(side = 2, labels = seq(0, 24, 2), at = seq(0, 24, 2), cex = 0.75, las = 2)
axis(side = 1, at = c(4, 10), labels = c("Departure", "Arrival"))
box()

# barplot for DEN airport
den_airlines <- as.character(head(subset(dep_by_airport, dep_by_airport$Origin == "DEN"), 5)$Carrier)
den_dep_subset <- subset(dep_by_airport, dep_by_airport$Carrier %in% den_airlines & dep_by_airport$Origin == "DEN")
den_arr_subset <- subset(arr_by_airport, arr_by_airport$Carrier %in% den_airlines & arr_by_airport$Dest == "DEN")
den_data <- data.frame(DepDelay = den_dep_subset[order(den_dep_subset$Carrier), ]$perFlightDelay,
                       ArrDelay = den_arr_subset[order(den_arr_subset$Carrier), ]$perFlightDelay)
rownames(den_data) <- den_airlines[order(den_airlines)]
barplot(t(t(den_data)), beside = TRUE, legend = TRUE,  ylab = "Delay per flight (in mins)",
        xlab = "Delay Type", axes = FALSE, axisnames = FALSE, ylim = c(0, 16),
        col = c("orangered4", "midnightblue", "goldenrod3", "grey37", "chartreuse4"), 
        main = "DEN airport delay distribution per airline (Top 5)",
        args.legend = list(x="topright"))
axis(side = 2, labels = seq(0, 16, 2), at = seq(0, 16, 2), cex = 0.75, las = 2)
axis(side = 1, at = c(4, 10), labels = c("Departure", "Arrival"))
box()

#################################################################################################################
#################################################################################################################
