#Read the files
Boeing = read.csv("BoeingStock.csv")
CocoCola = read.csv("CocaColaStock.csv")
GE = read.csv("GEStock.csv")
IBM = read.csv("IBMStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")

Boeing$Date = as.character(Boeing$Date)
Boeing$Date = as.Date(Boeing$Date,"%m/%d/%y")

CocoCola$Date = as.character(CocoCola$Date)
CocoCola$Date = as.Date(CocoCola$Date,"%m/%d/%y")

GE$Date = as.character(GE$Date)
GE$Date = as.Date(GE$Date,"%m/%d/%y")

IBM$Date = as.character(IBM$Date)
IBM$Date = as.Date(IBM$Date,"%m/%d/%y")

ProcterGamble$Date = as.character(ProcterGamble$Date)
ProcterGamble$Date = as.Date(ProcterGamble$Date,"%m/%d/%y")

#how many observations in each dataset: USE(str)
str(Boeing)

#what dates are the obsservation for?
summary(Boeing)
summary(CocoCola)
summary(GE)
summary(IBM)
summary(ProcterGamble)

#standard variation of p&g
sd(ProctorGamble$StockPrice)
var(ProcterGamble$StockPrice)

#Around what year did Coco cola has its highest stock price in that time period

plot(CocoCola$Date, CocoCola$StockPrice, type = "l")
# Can we make the color different
plot(CocoCola$Date, CocoCola$StockPrice, type = "l", col = "blue")
#Create a line graph for p&g
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "red")

#lines representing where the stocks have crashed
abline(v=as.Date(c("2004-03-01")), lwd=2,col="darkgreen")
abline(v=as.Date(c("1986-05-01")), lwd=2,col="darkgreen")

# to look at specific dates, that is, from 1996 to 2001
#Visualizing stock dynamics 1995-2005
plot(CocoCola$Date[301:432], CocoCola$StockPrice[301:432], type = "l", col = "red", ylim = c(0,210))
#make the y limits from 40 - 100
plot(CocoCola$Date[301:432], CocoCola$StockPrice[301:432], type = "l", col = "red", ylim = c(40,90))

#which stock crashed the most during dotcom crisis
plot(CocoCola$Date[301:432], CocoCola$StockPrice[301:432], type = "l", col = "red", ylim = c(0,210))

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col = "blue")
lines(IBM$Date[301:432],IBM$StockPrice[301:432],col = "green")
lines(GE$Date[301:432],GE$StockPrice[301:432],col = "purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col = "orange")

#You can add a vertical line with x axis labels and title
abline(v=as.Date(c("2000-03-01")), lwd=2)
#do the same plot with x axis, y axis labels and titles
plot(CocoCola$Date[301:432], CocoCola$StockPrice[301:432], type = "l", col = "red", ylim = c(0,210),
     main = "Plots for stocks between 1995-2005", sub = "",xlab = "Year",ylab = "Stock Prices")

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col = "blue")
lines(IBM$Date[301:432],IBM$StockPrice[301:432],col = "green")
lines(GE$Date[301:432],GE$StockPrice[301:432],col = "purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col = "orange")

#you can add vertical line to the plot @ March 2000 by typing the following cmd
abline(v=as.Date(c("2000-03-01")), lwd=2)

#lets see if stocks tend to be higher or lower during certain months
tapply(IBM$StockPrice, months(IBM$Date), mean)
#sort the data
sort(tapply(IBM$StockPrice, months(IBM$Date), mean))

# do the same for all other data
sort(tapply(GE$StockPrice, months(GE$Date), mean))
sort(tapply(CocoCola$StockPrice, months(CocoCola$Date), mean))
sort(tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean))
sort(tapply(Boeing$StockPrice, months(Boeing$Date), mean))

MeanIBMStockPriceByMonth = tapply(IBM$StockPrice, months(IBM$Date), mean)
