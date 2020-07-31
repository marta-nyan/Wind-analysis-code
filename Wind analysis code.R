
# This program carries out calculations/builds plots for describing and analysing monthly wind speed
# readings from the weather stations of Ijmuiden and Schiphol (Netherlands) for the period 1950 - 2003. 
### TASK 1 investigates the location and shape of average wind speed readings
## and seasonal and yearly patterns in the data.
### TASK 2 carries out two sample t-tests comparing data from different periods. 

## The program: 

### sets working directory
setwd("~/THA")
### imports the data
library(readr)
group_49 <- read.csv("group_49.csv")
View(group_49)
### creates two dataframes, one for each station
splitdata <- split(group_49, group_49$Site_No)
View(splitdata)
station_2 <- splitdata [[2]]
station_1 <- splitdata [[1]]

### substitutes the -1 values in the average with NA
station_1$Ave[which(station_1$Ave == '-1')] <- NA
View(station_1)
station_2$Ave[which(station_2$Ave == '-1')] <- NA
View(station_2)


####### TASK 1: describe the data
### station 1 is IJmuiden, station 2 is Schiphol 

# we investigate the location, spread and shape of wind speed average for both stations


### summary statistics for station 1 
summary (station_1$Ave) # summary statistics for the wind speed averages at location 1
### summary statistics for station 2 
summary (station_2$Ave) # summary statistics for the wind speed averages at station 2

#### The 6 number summaries suggests that the wind was, on average, slightly stronger at station 1.
### To further investigate this trend, we build back to back histograms to compare the datasets.


h1 = hist(station_1$Ave, freq = FALSE, plot = FALSE)
h2 = hist(station_2$Ave, freq = FALSE, plot = FALSE)
# we reflect the histogram about the x axis
h2$density = - h2$density

histmax = max(h1$density)
histmin = min(h2$density)
histmin
xaxis <- c(h1$breaks, h2$breaks)
xmax = max(xaxis)
xmin = min(xaxis)
plot(h1, freq = FALSE, ylim=c(histmin, histmax), col="azure2", xlim=c(xmin -1, xmax +1), 
     main = 'Wind speed in IJmuiden and Schipol, 1950-2003',
     cex.main = 2, xlab = 'Average wind speed (m/s)', cex.lab = 1.5)
lines(h2, freq = FALSE, col="gold")
legend (x = 8.3, y = 0.4, legend = c ("IJmuiden", "Schipol"), 
        col = c ("azure2","gold"), lty = rep (1, times = 2), lwd = 10, cex = 1.2)
# saves the plot into a png file
dev.copy (device = png, file = "hist.png", width = 40, height = 18.5, units = "cm", res = 300)
dev.off ()

### we note that the distribution of both datasets are slight positive skew. 
# This is confirmed by calculating the skewness (from the package moments):
library(moments)
skewness(station_1$Ave, na.rm = TRUE)
skewness(station_2$Ave)


### investigates spread of the data 
sd (station_1$Ave, na.rm = TRUE)
sd (station_2$Ave, na.rm = TRUE)


### We suggest that wind speed is dependent on the time of the year it was measured. 
### we build boxplots of average wind speed in every month for each station
# and we show them side by side

par (mfrow = c (1, 2))
boxplot (station_1$Ave ~ station_1$Month, xaxt = "n" , ylab = "m/s",main = "Monthly wind speed, IJmuiden",
         cex.main = 1.15, col = 'azure2', ylim = c(3,11), cex.lab = 1.5)
axis (side = 1, at = 1:12, labels = month.abb, cex.axis = 0.6)
boxplot (station_2$Ave ~ station_2$Month, xaxt = "n" , ylab = "m/s",main = "Monthly wind speed, Schiphol",
         cex.main = 1.15, col = 'gold', ylim = c(3,11), cex.lab = 1.5)
axis (side = 1, at = 1:12, labels = month.abb, cex.axis = 0.6)
par (mfrow = c (1, 1))
# saves the plot into a png file
dev.copy (device = png, file = "boxplots.png",  width = 30, height = 18.5, units = "cm", res = 300)
dev.off ()


## 5) to look at this seasonal trend across the years: we build a time series plot and we superimpose 
### a line connecting the mean for each year to show how the wind speed changed between 1950 and 2003.


timeseries_station_1 <- ts(station_1$Ave, start = c(1950,3), end = c(2003,1), freq = 12)
timeseries_station_2 <- ts(station_2$Ave, start = c(1950,3), end = c(2003,1), freq = 12)

# We build the respective matrices to be able to calculate the mean for each row

##### for statio_1: measures from 1950 are omitted bc they are all NAs and would be omitted by the lines function; 
## data from 2003 consist of a single value and the mean would be misleading, so it is omitted: 
# that is why the matrix can safely not include these values and be 12x52.
## for station 2 the same applies: data from 1950 and 2003 consist of less than 12 values, so the mean woud misleading
# and the values are omitted. The matrix is then 12x52.

timeseries_station_1_matrix <- t(matrix(timeseries_station_1 [-(1:10)][-(630:635)], nrow = 12, ncol = 52))
mean_station_1 <- rowMeans(timeseries_station_1_matrix)

timeseries_station_2_matrix <- t(matrix(timeseries_station_2 [-(1:10)][-(630:635)], nrow = 12, ncol = 52))
mean_station_2 <- rowMeans(timeseries_station_2_matrix)

### plots

par (mfrow = c (1, 2))

plot(timeseries_station_1, lwd = '0.1', ylim = c(2,12), ylab = 'm/s', main = 'Wind speed in IJmuiden, 1950 - 2003', 
     cex.main = 0.95)
legend (x = 1950, y = 12, legend = c ("yearly average"), 
        col = c ('red'), lty = 1, lwd = 3, cex = 0.7)
lines(mean_station_1, x = seq(1951, 2002) , col = 'red', lwd = 2, type = 'l', ylim = c(2,12))
plot(timeseries_station_2, lwd = '0.1', ylim = c(2,12), ylab = 'm/s', main = 'Wind speed in Schiphol, 1950 - 2003', 
     cex.main = 0.95)
lines(mean_station_2, x = seq(1951, 2002), col = 'red', lwd = 2, type = 'l', ylim = c(2,12))
par (mfrow = c (1, 1))
# dev.copy saves the plot
dev.copy (device = png, file = 'timeseries.png',  width = 30, height = 18.5, units = "cm", res = 300)
dev.off ()









####################################################################################################################

# TASK 2

#######################


## subset splits location 1 data to only include data entries with years above 1976

station_1_from1977 <- subset (station_1, station_1$Year > 1976) 
View(station_1_from1977)

## subset splits location 1 data to only include data entries with years from 1950 to 1976
station_1_from1950 <- subset (station_1, station_1$Year < 1977) 
View(station_1_from1950)

## splits location 2 data to only include data entries in the summer months
station2summer <- subset (station_2, station_2$Month > 5 & station_2$Month < 9) 
View(station2summer)

## splits location 2 data to only include data entries from the winter months
station2winter <- subset (station_2, station_2$Month < 3 | station_2$Month == 12) 
View(station2winter)

t.test(x=station2summer$Ave, y = station2winter$Ave,       
	       mu = 4.592, paired = FALSE, var.equal = FALSE,
	       conf.level = 0.95)

t.test(x=station_1_from1950$Ave, y = station_1_from1977$Ave,       
       mu = 4.592, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

t.test(x=station_2$Ave, y = station_1$Ave,       
       mu = 4.592, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

summermean <- mean(station2summer$Ave, na.rm=T)
wintermean <- mean(station2winter$Ave, na.rm=T)

t1 <- t.test(station2summer$Ave, station2winter$Ave)
t2 <- t.test(station_1_from1950, station_1_from1977)
t3 <- t.test(station_1, station_2)




######################################## Sink fuction ##################################################################
### save the 
sink(file = "statistics.txt", append = FALSE,
     split = FALSE)
summary(station_1$Ave)
summary(station_2$Ave)
skewness(station_1$Ave, na.rm = TRUE)
skewness(station_2$Ave)
sd (station_1$Ave, na.rm = TRUE)
sd (station_2$Ave, na.rm = TRUE)
t1 <- t.test(station2summer$Ave, station2winter$Ave)
t2 <- t.test(station_1_from1950, station_1_from1977)
t3 <- t.test(station_1, station_2)

sink()
########################################################################################################################


