library(neuralnet)
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv",fileEncoding = "Latin1")
temps <- read.csv("temps50k.csv",fileEncoding = "Latin1")
st <- merge(stations,temps,by="station_number")
h_distance <- 1000000# These three values are up to the students
h_date <- 12
h_time <- 7
a <- 58.4274 
b <- 14.826
date <- "2015-07-12" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00","10:00:00",
           "12:00:00" ,"14:00:00", "16:00:00","18:00:00",
           "20:00:00","22:00:00","24:00:00")
temp <- vector(length=length(times))


st$distance = st$latitude
for (i in 1:50000)
{
  st$distance[i] = distHaversine(c(st$latitude[i], st$longitude[i]), c(a, b))
}
exp(-(dist / h_distance)^2)
kd1 = exp(-1 * (st$distance / (2*500000))^2)
ggplot(st, aes(x = distance, y = kd1), colour="blue") + geom_point()



#Q2
date <- "2018-01-28"
st$datediff = as.numeric(abs(as.Date(st$date) - as.Date(date)))%%365
#st$datediff[st$datediff > 182] <- 365 - st$datediff
for(i in 1:50000)
{
  if(st$datediff[i] > 182)
    st$datediff[i] = 365 - st$datediff[i]
}
#st$datediff[st$datediff > 182] <- 365
kd2 = exp(-1 * (as.numeric(st$datediff)**2) / (2*15**2))
ggplot(st, aes(x = datediff, y = kd2), colour="blue") + geom_point()



#Q3
time =  "10:00:00"
st$timediff = 0
for (i in 1:50000)
{
  c1 = as.numeric(substr(time,1,2))
  c2 = as.numeric(substr(st$time[i],1,2))
  st$timediff[i] = abs(c1 - c2)
}
kd3 = exp(-1 * (st$timediff**2) / (2*3**2))
ggplot(st, aes(x = timediff, y = kd3), colour="blue") + geom_point()


#make prediction
st[, 11] = scale(st[, 11])
#sum of all 3 kernels
ksum = as.matrix(kd1) + as.matrix(kd2) + as.matrix(kd3)
ypred = ksum * as.matrix(st$air_temperature)
ypred = ypred / ksum
ggplot(st, aes(x = distance, y = ksum), colour="blue") + geom_point()


#Mult of all 3 kernels
kprod = as.matrix(kd1) * as.matrix(kd2) * as.matrix(kd3)
ypred = kprod * as.matrix(st$air_temperature)
ypred = ypred / kprod
ggplot(st, aes(x = distance, y = kprod), colour="blue") + geom_point()



