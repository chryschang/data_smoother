#Plot Actot, Fluortot and Tair from SCOPE output.

library(readr)   #import and read csv files
library(ggplot2) #generate plots
library(dplyr)   #data manipulation: filter, summarize, mutate
library(zoo)     #used for rolling average calculations

fluxes <- read_csv("path/to/fluxes.csv")
surftemp <- read_csv("path/to/surftemp.csv")

#identify variables
DOY <- as.numeric(fluxes$t)                 #use as.numeric to import as float
A <- as.numeric(fluxes$Actot)
Fluo <- as.numeric(fluxes$fluortot)
Tair <- as.numeric(surftemp$Ta)

#create new table and populate
new_table <- cbind()
new_table$t <- as.numeric(paste(DOY))                   #t is the fractional Julian date
new_table$DOY <- as.numeric(paste(floor(DOY)))          #DOY is the rounded integer Julian date for use later
new_table$A <- as.numeric(paste(A))
new_table$Tair <- as.numeric(paste(Tair))
new_table$Fluo <- as.numeric(paste(Fluo))

#convert new_table to dataframe
df = as.data.frame(new_table)
df = df[-1,]                                #remove first row of NAs

doy.means <- df %>% group_by(DOY) %>% summarize(meanT = mean(Tair, na.rm=TRUE), meanA =  mean(A, na.rm=TRUE), meanF = mean(Fluo, na.rm=TRUE)) #using pipes to simplify above steps and save to variable

#individual variable plots
qplot(doy.means[1],doy.means[2], xlab="Julian Day", ylab="Mean air temperature (degC)", main="Daily Mean Air Temperature")
qplot(doy.means[1],doy.means[3], xlab="Julian Day", ylab="Mean Actot (umol CO2 m-2 s-1)", main="Daily Mean Assimilation")
qplot(doy.means[1],doy.means[4], xlab="Julian Day", ylab="Mean Fluorescence (W m-2)", main="Daily Mean Fluorescence")

#overlaid plots with 3 described axes
par(mar=c(5, 12, 4, 4) + 0.1)                                #create a left margin for the graph.
plot(doy.means$DOY, doy.means$meanT, axes=F, ylim=c(-10,40), xlab="", ylab="") #first variable. silence the x and y labels for manual insertion later
points(doy.means$DOY, doy.means$meanT,pch=20,col="red")         #Tair points
axis(2,col="red",lwd=2)                                         #Tair axis
mtext(2,text="Tair (degC)", line=2)                             #Tair axis header. line indicates the spacing

par(new=T)
plot(doy.means$DOY, doy.means$meanA, axes=F, ylim=c(0,20), xlab="", ylab="")
points(doy.means$DOY, doy.means$meanA,pch=20,col="green")               #A points
axis(2, col="green",lwd=2, line=3.5)                                    #A axis
mtext(2,text="Assimilation (umol CO2 m-2 s-1)", line=5.5)               #A axis header

par(new=T)
plot(doy.means$DOY, doy.means$meanF, axes=F, ylim=c(0,1), xlab="", ylab="")
points(doy.means$DOY, doy.means$meanF,pch=20,col="blue")         #Fluo points
axis(2, col="blue",lwd=2, line=7)                                #Fluo axis
mtext(2,text="Fluorescence (W m-2)", line=9)                     #Fluo axis header

axis(1,pretty(range(doy.means$DOY),10))                       #x axis
mtext("Julian Day",side=1,col="black",line=2)                 #x axis header

legend(x=0,y=1,legend=c("Tair","Assimilation","Fluorescence"),pch=20, col=c("red","green","blue")) #legend
title("Mean Daily Values - MOz 2005")

#and now for the smoothing part...

temp.doy <- doy.means[[1]]
temp.t <- doy.means[[2]]
temp.a <- doy.means[[3]]
temp.f <- doy.means[[4]]

temp.zt <- zoo(temp.t, temp.doy)
temp.za <- zoo(temp.a, temp.doy)
temp.zf <- zoo(temp.f, temp.doy)
mt <- rollmean(temp.zt, 7, fill=list(NA, NULL, NA)) #7 day rolling average
ma <- rollmean(temp.za, 7, fill=list(NA, NULL, NA)) 
mf <- rollmean(temp.zf, 7, fill=list(NA, NULL, NA)) 
doy.means$mt <- as.numeric(paste(coredata(mt))) #insert to dataframe
doy.means$ma <- as.numeric(paste(coredata(ma)))
doy.means$mf <- as.numeric(paste(coredata(mf)))

#single plots
qplot(doy.means[1],doy.means[5], xlab="Julian Day", ylab="Mean air temperature (degC)", main="Air temperature, 5 day avg")
qplot(doy.means[1],doy.means[6], xlab="Julian Day", ylab="Mean assimilation (umol CO2 m-2 s-1)", main="Assimilation, 5 day avg")
qplot(doy.means[1],doy.means[7], xlab="Julian Day", ylab="Mean fluorescence (W m-2)", main="Fluorescence, 5 day avg")

#simple quick overlaid plots
ggplot(data = doy.means, aes(DOY)) + geom_line(aes(y=mt, colour="meanT")) + geom_line(aes(y=ma, colour="meanA")) + geom_line(aes(y=mf, colour="meanF"))

#overlaid plots with 3 described axes
par(mar=c(5, 12, 4, 4) + 0.1)                                #create a left margin for the graph.
plot(doy.means$DOY, doy.means$mt, axes=F, ylim=c(-10,40), xlab="", ylab="") #first variable. silence the x and y labels for manual insertion later
points(doy.means$DOY, doy.means$mt,pch=20,col="red")         #Tair points
axis(2,col="red",lwd=2)                                      #Tair axis
mtext(2,text="Tair (7-day avg; degC)", line=2)               #Tair axis header. line indicates the spacing

par(new=T)
plot(doy.means$DOY, doy.means$ma, axes=F, ylim=c(0,20), xlab="", ylab="")
points(doy.means$DOY, doy.means$ma,pch=20,col="green")               #A points
axis(2, col="green",lwd=2, line=3.5)                                 #A axis
mtext(2,text="Assimilation (7-day avg; umol CO2 m-2 s-1)", line=5.5) #A axis header

par(new=T)
plot(doy.means$DOY, doy.means$mf, axes=F, ylim=c(0,1), xlab="", ylab="")
points(doy.means$DOY, doy.means$mf,pch=20,col="blue")         #Fluo points
axis(2, col="blue",lwd=2, line=7)                             #Fluo axis
mtext(2,text="Fluorescence (7-day avg; W m-2)", line=9)       #Fluo axis header

axis(1,pretty(range(doy.means$DOY),10))                       #x axis
mtext("Julian Day",side=1,col="black",line=2)                 #x axis header

legend(x=0,y=1,legend=c("Tair","Assimilation","Fluorescence"),pch=20, col=c("red","green","blue")) #legend
title("7-day Running Averages - MOz 2005")
