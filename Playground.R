# Descriptive statistics of rgdpl2
library(plyr)
ls(pwt71)
head(pwt71)
pwt71 <- na.omit(pwt71)
head(pwt71)
gdpgrowth9 <- ddply(pwt71,~isocode,summarise,mean=mean(gdpgrowth, na.rm = TRUE), min.yr=min(year), max.yr=max(year))
gdpgrowth9
hist(gdpgrowth9$mean, breaks = 20)
hist(gdpgrowth9$max.yr)
hist(gdpgrowth9$min.yr)
min(gdpgrowth9$max.yr)
max(gdpgrowth9$min.yr)

## Calculate gdp growth value from rgdpl
# Here the pwt71 data is split into country groups with rgdpl
countries2 <- split(pwt71[,3], pwt71$isocode)
# See data structure
countries2[1]
# Create function that calculates growth rate
growth <- function(x){
  x / x[21] 
}
# Use lapply
#rgdpl.growth <- lapply(countries, function(x) x / x[1])
rgdpl.growth <- lapply(countries2, growth)
# This first value should be this
(981.9174-1040.6090)/1040.6090
rgdpl.growth[1]

# Re-assembling the data
pwt71$growth <- unsplit(rgdpl.growth, pwt71$isocode)
head(pwt71, n=25)


countries2[[1]][-1]
countries2[[-1]][1]
countries[-1]

# Creating very simple function
growth3 <- c(981.9174, 1040.6090)
growth3
gro

growth2 <- function(x) (x - x[x-1])/x


########
# German Rodriguez http://data.princeton.edu/eco572/grdt.html
# Growth Rates and Doubling Time
# Translation from Stata to R by Jeanne Spicer

# Read data from website, run this line alone as there may be a delay 
uspop<-read.table("http://web.pop.psu.edu/~spicer/uspop.csv", header=TRUE)
attach(uspop)
pm<-pop/1000000
# To write to png, uncomment lines with png() and dev.off () functions below
# png("mydoubleplot.png", width=800)
par(mfrow=c(2,2))
plot(year,pm, type="l",  ylab="Population (millions)")
# plot(year,log(pm), type="l")
plot(year, pm, log="y", type = "l")
title(main="US Pop", outer = T, line=-3) 
#dev.off()

pop[21:22] 
pop[22]-pop[21]
pop[22]/pop[21] -1

pop[-1]
# create a vector with a null first element then use list arithmetic to compute growthrate 
growthrate<-c(NA, log(pop[-1]/pop[-22])/10)
growthrate

ratio<-pop[22]/pop[21]
k<-c(1,2,4,6,12,52,365)
r= k*(ratio^(1/(10*k))-1)
print(cbind(k,r))
log(ratio)/10


# To write to png, uncomment lines with png() and dev.off () functions below
#png("usgrdt.png", width=800)
par(mfrow=c(1,2))
midyear<-c(NA,(year[-1]+year[-22])/2)
plot(midyear,growthrate,type="l", main="Growth Rate")
doub<-c(NA,log(2)/growthrate[-1])

plot(midyear,doub, type="l",main="Doubling Time")
title(main="US 1790-2000\n", outer = T, line=-3) 
#dev.off()

###### Vaekst og inigdp reg
summary(lm(gdpgrowth ~ inigdp, data = IQM_pro_data))
summary(lm(gdpgrowth ~ gdp.growth, data = IQM_pro_data))
cor(IQM_pro_data$gdpgrowth, IQM_pro_data$gdp.growth, use = "na.or.complete", 
    method = c("pearson", "kendall", "spearman"))

