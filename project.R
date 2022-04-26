content <- read.csv(file = '/Users/pebend/Desktop/Data/ccmp_data.csv', header = T) #from my own system
content
View(content)

install.packages("dplyr")    # alternative installation of the %>%
library(dplyr)  
library(rpart)
library(rpart.plot)

attach(content)
head(content)
summary(content)
drop <- c("ID","authors","title","journal", "DOI","publication_type","forecasting","paleoecology","species_latin","nb_species", "multi_trophic")
marine = content[,!(names(content) %in% drop)]
dim(marine)
dim(content)
barplot(table(marine$group), main="Seabird or Mammal", xlab="Marine Group",ylab="Number of Animals")
Seabirds <- filter(marine, group == "Seabirds")
MarineMammals <- filter(marine, group == "Marine mammals")

Seabirds <- filter(Seabirds, Seabirds$body_mass_g <6000)
Seabirds <- filter(Seabirds, Seabirds$age1stb <12)
Seabirds <- filter(Seabirds, Seabirds$publication_year >1995)

MarineMammals <-filter(MarineMammals, MarineMammals$body_mass_g < 50000, MarineMammals$body_mass_g > 40000)
MarineMammals <- filter(MarineMammals, MarineMammals$age1stb <8)


dim(Seabirds)
dim(MarineMammals)

#family1 <- filter(marine, family == "Diomedeidae" | family == "Alcidae" | family == "Kogiidea" |family =="Otariidae" | family == "Phocoenidae" | family == "Sulidae")
barplot(table(marine$family), main="Animal Family", xlab="Number of Animals within family", horiz = TRUE,
        cex.names=0.7,las=1)
barplot(table(Seabirds$family), main="Seabird Families", xlab="Number of Animals within family", horiz = TRUE,
        cex.names=0.7,las=1)
barplot(table(MarineMammals$family), main="Marine Mammal Families", xlab="Number of Animals within family", horiz = TRUE,
        cex.names=0.7,las=1)

barplot(table(marine$oceanic_region), main="General Region", xlab = "Number of animals", horiz = TRUE,
        cex.names=0.7,las=1)
barplot(table(Seabirds$region), main="Sea birds Region",xlab = "Number of animals")
barplot(table(MarineMammals$region), main="Marine Mammals Region",xlab = "Number of animals")
barplot(table(Seabirds$change_or_variability), main=" Seabirds Climate Change or Other Factor", xlab="Impacting Factor",ylab = "Number of animals")
barplot(table(MarineMammals$change_or_variability), main="Mammals Climate Change or Other Factor", xlab="Impacting Factor",ylab = "Number of animals")
barplot(table(marine$change_or_variability), main="Marine Animals Climate Change or Other Factor", xlab="Impacting Factor",ylab = "Number of animals")

boxplot(Seabirds$body_mass_g, main = "Seabird Body Mass")
# Seabirds <- filter(Seabirds, Seabirds$body_mass_g <6000)
# boxplot(Seabirds$body_mass_g, main = " Animal Body Mass")
boxplot(Seabirds$age1stb, main = "Seabird Ages")
# Seabirds <- filter(Seabirds, Seabirds$age1stb <12)
# boxplot(Seabirds$age1stb, main = " Animal Ages")
boxplot(Seabirds$publication_year, main = "Seabird Study Period Years")
boxplot(Seabirds$sst_median, main = "Seabird Sea Surface Thermal Tolerance")


boxplot(MarineMammals$body_mass_g, main = "Mammal Body Mass")
boxplot(MarineMammals$age1stb, main = "Mammal Ages")
# MarineMammals <- filter(MarineMammals, MarineMammals$age1stb <8)
# boxplot(MarineMammals$age1stb, main = "Animal Ages")
boxplot(MarineMammals$publication_year, main = "Mammal Study Period Years")
boxplot(MarineMammals$sst_median, main = "Mammal Sea Surface Thermal Tolerance")


hist(Seabirds$body_mass_g, seq(0,6000, 100), prob=TRUE)
hist(Seabirds$age1stb, seq(0,12, 1), prob=TRUE)
hist(Seabirds$publication_year, seq(1995,2020, 1), prob=TRUE)
hist(Seabirds$sst_median, seq(-2,30, 1), prob=TRUE)

hist(MarineMammals$body_mass_g, seq(40000,50000, 100), prob=TRUE)
hist(MarineMammals$age1stb, seq(0,12, 1), prob=TRUE)
hist(MarineMammals$publication_year, seq(1995,2020, 1), prob=TRUE)
hist(MarineMammals$sst_median, seq(-2,15, 1), prob=TRUE)

install.packages(car)
library(car)

#produce added variable plots

model <- lm(Seabirds$sst_median~ Seabirds$publication_year+ Seabirds$body_mass_g+Seabirds$age1stb, data = Seabirds)
print(model)
summary(model)
avPlots(model)
plot(model)

#cook's distance
model2 <- lm(MarineMammals$sst_median~ MarineMammals$publication_year+ MarineMammals$body_mass_g+MarineMammals$age1stb, data = MarineMammals)
print(model2)
summary(model2)
avPlots(model2)
plot(model2)


library(ggplot2)
Seabirds_temp <-Seabirds[,c(1,43,5)]
ggplot(Seabirds_temp,aes(x = publication_year, y = sst_median, col= region)) + geom_point()
set.seed(300)
k.max <- 12
wss<- sapply(1:k.max,function(k){kmeans(Seabirds_temp[,1:2],k,nstart = 20,iter.max = 20)$tot.withinss})
wss 
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(Seabirds_temp[,1:2],3,nstart = 20)
table(icluster$cluster,Seabirds_temp$region)
#reduced negative results disproving hypothesis 

MarineMammals_temp <-MarineMammals[,c(1,43,5)]
ggplot(MarineMammals_temp,aes(x = MarineMammals_temp$publication_year, y = MarineMammals_temp$sst_median, col= region)) + geom_point()
set.seed(300)
k.max <- 12
wss<- sapply(1:k.max,function(k){kmeans(MarineMammals_temp[,1:2],k,nstart = 20,iter.max = 20)$tot.withinss})
wss 
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(MarineMammals_temp[,1:2],3,nstart = 20)
table(icluster$cluster,MarineMammals_temp$region)



