#data_2010EPI <-read.csv(file.choose(), header = T)
data_2010EPI <- read.csv(file = '/Users/pebend/Downloads/2010EPI_data.csv', header = T) #from my own system
data_2010EPI

#changing the first row so that variable names are not in first row of values
names(data_2010EPI) <- as.matrix(data_2010EPI[1, ])
data_2010EPI <- data_2010EPI[-1, ]
data_2010EPI[] <- lapply(data_2010EPI, function(x)
  type.convert(as.character(x)))
data_2010EPI
View(data_2010EPI)

attach(data_2010EPI) # sets the ???default??? object
# fix(data_2010EPI) # launches a simple data editor ??? test it!
data_2010EPI # prints out values EPI_data$data_2010EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array
data_2010EPI
#summary
summary(data_2010EPI) 	# stats
dim(data_2010EPI)
str(data_2010EPI)
head(data_2010EPI)

data_2010EPI$EPI
DALY

plot(Population07,EPI,xlab="Population07",ylab="EPI") 
plot(Population07,Landarea,xlab="Population07",ylab="Landarea") 

fivenum(EPI, na.rm=TRUE)
# # help(stem)
stem(EPI)		 # stem and leaf plot
# # help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
# help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=???????SJ???????
# # help(rug)
rug(EPI) 

# #distributions
# #Cumulative Density Function
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
# #Quantile-Quantile?
par(pty="s") 
qqnorm(EPI); qqline(EPI)
# #Simulated data from t-distribution:
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
# #Make a Q-Q plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
# 
# #filter
# #Landlock
EPILand<-EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
#
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)


tf2 <- is.na(DALY) # records True values if the value is NA
E2 <- DALY[!tf] # filters out NA values, new array
DALY
summary(DALY)
fivenum(DALY, na.rm=TRUE)
boxplot(DALY)
stem(DALY)		 # stem and leaf plot
hist(DALY)
hist(DALY, seq(0., 100., 1.0), prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.)) # or try bw=???????SJ???????
rug(DALY) 
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(DALY); qqline(DALY)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

