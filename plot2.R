data <- read.csv("household_power_consumption.txt", sep = ";")
library(plyr)
data <- mutate(data, datetime = as.POSIXlt(paste(as.character(data$Date), as.character(data$Time), sep = " "), tz = "", format = "%d/%m/%Y %H:%M:%S"))
data <- subset(data, datetime >= as.POSIXlt("2007-02-01 00:00:00", tz = "", format = "%Y-%m-%d %H:%M:%S"))
data <- subset(data, datetime < as.POSIXlt("2007-02-03 00:00:00", tz = "", format = "%Y-%m-%d %H:%M:%S"))
data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
png("plot2.png", width = 480, height = 480)
plot(data$datetime, data$Global_active_power, pch = "", yaxt = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
lines(data$datetime, data$Global_active_power)
axis(side = 2, at = seq(0, 6, 2))
dev.off()