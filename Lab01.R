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

attach(data_2010EPI) # sets the ‘default’ object
# fix(data_2010EPI) # launches a simple data editor – test it!
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
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=â€œSJâ€
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
lines(density(DALY,na.rm=TRUE,bw=1.)) # or try bw=â€œSJâ€
rug(DALY) 
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(DALY); qqline(DALY)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

tf2 <- is.na(WATER_H) # records True values if the value is NA
E2 <- WATER_H[!tf] # filters out NA values, new array
WATER_H
summary(WATER_H)
fivenum(WATER_H, na.rm=TRUE)
boxplot(EPI,WATER_H)
stem(WATER_H)		 # stem and leaf plot
hist(WATER_H)
hist(WATER_H, seq(0., 100., 1.0), prob=TRUE)##########################
lines(density(WATER_H,na.rm=TRUE,bw=1.)) # or try bw=â€œSJâ€
rug(WATER_H) 
plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(WATER_H); qqline(WATER_H)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(EPI, WATER_H)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

tf2 <- is.na(WATER_E) # records True values if the value is NA
E2 <- WATER_E[!tf] # filters out NA values, new array
WATER_E
summary(WATER_E)
fivenum(WATER_E, na.rm=TRUE)
boxplot(EPI,WATER_E)
stem(WATER_E)		 # stem and leaf plot
hist(WATER_E)
hist(WATER_E, seq(0., 100., 1.0), prob=TRUE)##########################
lines(density(WATER_E,na.rm=TRUE,bw=1.)) # or try bw=â€œSJâ€
rug(WATER_E) 
plot(ecdf(WATER_E), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(WATER_E); qqline(WATER_E)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(EPI, WATER_E)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

tf2 <- is.na(ECOSYSTEM) # records True values if the value is NA
E2 <- ECOSYSTEM[!tf] # filters out NA values, new array
ECOSYSTEM
summary(ECOSYSTEM)
fivenum(ECOSYSTEM, na.rm=TRUE)
boxplot(ECOSYSTEM)
stem(ECOSYSTEM)		 # stem and leaf plot
hist(ECOSYSTEM)
hist(ECOSYSTEM, seq(0., 100., 1.0), prob=TRUE)
lines(density(ECOSYSTEM,na.rm=TRUE,bw=1.)) # or try bw=â€œSJâ€
rug(ECOSYSTEM) 
plot(ecdf(ECOSYSTEM), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(ECOSYSTEM); qqline(ECOSYSTEM)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


tf2 <- is.na(ENVHEALTH) # records True values if the value is NA
E2 <- ENVHEALTH[!tf] # filters out NA values, new array
ENVHEALTH
summary(ENVHEALTH)
fivenum(ENVHEALTH, na.rm=TRUE)
boxplot(ENVHEALTH)
stem(ENVHEALTH)		 # stem and leaf plot
hist(ENVHEALTH)
hist(ENVHEALTH, seq(0., 100., 1.0), prob=TRUE)
lines(density(ENVHEALTH,na.rm=TRUE,bw=1.)) # or try bw=â€œSJâ€
rug(ENVHEALTH) 
plot(ecdf(ENVHEALTH), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(ENVHEALTH); qqline(ENVHEALTH)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

tf2 <- is.na(ECOSYSTEM) # records True values if the value is NA
E2 <- AIR_H[!tf] # filters out NA values, new array
AIR_H
summary(AIR_H)
fivenum(AIR_H, na.rm=TRUE)
boxplot(AIR_H)
stem(AIR_H)		 # stem and leaf plot
hist(AIR_H)
hist(AIR_H, seq(0., 100., 1.0), prob=TRUE)
lines(density(AIR_H,na.rm=TRUE,bw=1.)) # or try bw=â€œSJâ€
rug(AIR_H) 
plot(ecdf(AIR_H), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(AIR_H); qqline(AIR_H)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

tf2 <- is.na(ECOSYSTEM) # records True values if the value is NA
E2 <- AIR_E[!tf] # filters out NA values, new array
AIR_E
summary(AIR_E)
fivenum(AIR_E, na.rm=TRUE)
boxplot(AIR_E)
stem(AIR_E)		 # stem and leaf plot
hist(AIR_E)
hist(AIR_E, seq(0., 100., 1.0), prob=TRUE)
lines(density(AIR_E,na.rm=TRUE,bw=1.)) # or try bw=â€œSJâ€
rug(AIR_E) 
plot(ecdf(AIR_E), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(AIR_E); qqline(AIR_E)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
