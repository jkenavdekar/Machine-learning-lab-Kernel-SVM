set.seed(1234567890)
library(geosphere)
library(ggplot2)

stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

h_distance <- 600000
h_date <- 7
h_time <- 3

XX = subset(st, station_name == "Stockholm")
#a <- 59.342
#b <- 18.0575

a <-57.7084
b <- 11.9939

#date <- "2011-10-27"
date = "1987-07-28"

times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", 
           "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")


#Q1
gaussion_distance <- function(db_point, point_intreset)
{
  dist <- distHaversine(db_point, point_intreset)
  return (exp(-1 * (dist / (2 * h_distance))^2))
}

#Q2
gaussian_date <- function(db_date, point_of_intreset_date)
{
  diff_date = as.numeric(abs(as.Date(db_date) - as.Date(point_of_intreset_date)))%%365
  #diff_date[diff_date > 182] <- 365 - diff_date
  diff_date = ifelse(diff_date > 182, 365 - diff_date, diff_date)
  return (exp(-1 * (diff_date / (2 * h_date))^2))
}

#Q3
gaussian_hours <- function(db_time, point_of_intreset_date)
{
  diff_date <- as.numeric(difftime(point_of_intreset_date,db_time,unit = "hours"))
  return (exp(-1 * (diff_date / (2 * h_time))^2))
}

point_intreset <- c(a,b)
data_dist = st[,c("longitude", "latitude")]
gaussion_distacne_v <- gaussion_distance(data_dist, point_intreset)
ggplot(st, aes(x = distHaversine(data_dist, point_intreset), y = gaussion_distacne_v), 
       colour="blue") + geom_point()


gaussion_date_v <- gaussian_date(st$date, date)

time_conv <- data.frame(time=as.POSIXct(paste(Sys.Date(), st$time), format="%Y-%m-%d %H:%M:%S"))
times <- strptime( paste( Sys.Date(),times), "%Y-%m-%d %H:%M:%S")

temp <- vector(length=length(times))
temp2 <- vector(length=length(times))

for (i in 1:length(times))
{
  gausian_hours <- gaussian_hours(time_conv$time, times[i])

  sum_of_k <- (gaussion_distacne_v + gaussion_date_v + gausian_hours)
  temp[i] <- sum((st$air_temperature * sum_of_k)) / sum(sum_of_k)

  multiply_of_k <- (gaussion_distacne_v * gaussion_date_v * gausian_hours)
  temp2[i] <- sum((st$air_temperature * multiply_of_k)) / sum(multiply_of_k)
}

#plot of additive kernels
plot(x=times,y=temp, type = "o" ,
     main = "Temperature using Additive kernels" ,
     xlab = "Time" ,
     ylab = "Temperature")

#plot of multiplicative kernels
plot(x=times,y=temp2, type = "o",
     main = "Temperature using Multiplicative kernels" ,
     xlab = "Time" ,
     ylab = "Temperature")
