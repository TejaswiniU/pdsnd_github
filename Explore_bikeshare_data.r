
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(chi)

head(wash)

names(chi)

names(wash)

library(ggplot2)

workday=subset(ny$Start.Time,!is.na(ny$Start.Time))
Date = as.Date(workday)
Data_C = as.numeric(as.factor(months(Date)))
ggplot(aes(Data_C),data=ny)+geom_histogram(binwidth=1)+
ggtitle('Most common month') +
  labs(x = "Month") +scale_x_continuous(breaks=seq(1,12,1))


summary(Data_C)

chicago.sum = summary(chi$Start.Station)
paste('Most popular Start Station :')
head(chicago.sum)
paste('Least popular Start Station :')
tail(chicago.sum)
x<-subset(chicago.sum,chicago.sum>100&chicago.sum<3500)
barplot(x,xlab="Station",col="blue",ylab="Count",
main="Popular Start stations",border="black")


summary(x)

timeDuration_City <- function(chi,ny,wash,a,b,c){
    chi.avg = median(subset(chi$Trip.Duration,!is.na(chi$Trip.Duration)))
    ny.avg = median(subset(ny$Trip.Duration,!is.na(ny$Trip.Duration)))
    wash.avg = median(subset(wash$Trip.Duration,!is.na(wash$Trip.Duration)))
    x<-c(chi.avg,ny.avg,wash.avg)
    y<-c(a,b,c)
barplot(x,names.arg=y,xlab="City",ylab="Time Duration",col="blue",
main=paste("Average Trip duration at",paste(a,","),paste(b,","),c,sep=" "),border="black")
   
    #summary(subset(ny$Trip.Duration,!is.na(ny$Trip.Duration)),subset(wash$Trip.Duration,!is.na(wash$Trip.Duration)))
    return(x)
}

y = c(timeDuration_City(chi,ny,wash, 'Chicago',  'New York','Washington'))
x=c('Chicago',  'New York','Washington')

paste(paste('City ',c(1,2,3),sep=" "),y,sep=": ")
  
summary(chi$Trip.Duration)
summary(subset(ny$Trip.Duration,!is.na(ny$Trip.Duration)))
summary(wash$Trip.Duration)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
