## @knitr intro
setwd("/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/CoE_thesis_repository")
library(plm)
library(estout)
library(foreign)
## @knitr POLCON
#### POLCON: POLCONIII, POLCONV, POLCONJ, law.order ####
POLCON <- read.delim("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Henisz/POLCON_2010_tabsep.txt")
ls(POLCON)
POLCON <- 
  POLCON[c("CTRYNM", "Year", "POLCONIII", "POLCONV", "POLCONVJ", 
           "Law...Order..from.ICRG.", "XCONST..from.Polity.")] # Dropping irrelevant variables
ls(POLCON) # Checking which variables I have
# write.csv(POLCON, file = "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Henisz/POLCON.csv", row.names = FALSE) # Writing data set, because the original is very large
POLCON <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Henisz/POLCON.csv")
ls(POLCON)
head(POLCON,n=200)
# Descriptive statistics of llaw.order
library(plyr)
law.order1 <- ddply(POLCON,~CTRYNM,summarise,mean=mean(Law...Order..from.ICRG., 
                                                        na.rm = TRUE), 
                     min.yr=min(Year), 
                     max.yr=max(Year))
law.order1
length(na.omit(POLCON$Law...Order..from.ICRG.)) 
length(na.omit(POLCON$POLCONIII))

# What are the number of observations per country?
summary(POLCON$CTRYNM)
library(foreign)

# Descriptive statistics of llaw.order
POLCON$llaw.order <- log(POLCON$Law...Order..from.ICRG.) # make variable
library(plyr)
llaw.order1 <- ddply(POLCON,~CTRYNM,summarise,mean=mean(llaw.order, 
                                                       na.rm = TRUE), 
                    min.yr=min(Year), 
                    max.yr=max(Year))
llaw.order1


mystats <- function(x, na.omit=TRUE){
  if (na.omit)
  x <- x[!is.na(x)]
  m <- mean(x)
  med <- median (x)
  n <- length(x)
  s <- sd(x)
  min <- min(x)
  max <- max(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, median=med, max=max, min=min, stdev=s))
}
POLCON_vars <- POLCON[c("POLCONIII", "POLCONV", "POLCONVJ")]
POLCON_desc <- sapply(POLCON_vars, mystats, na.omit=TRUE)
is.matrix(POLCON_desc)
POLCON_desc <- t(POLCON_desc) # Transposing the matrix
class(POLCON_desc)
str(POLCON_desc)

## descriptive statistics by country
#install.packages("doBy")                   
library(doBy)
# Data for POLCONIII
POLCON_desc2 <- summaryBy(POLCONIII~CTRYNM, data=POLCON, FUN=mean, na.rm = TRUE)
POLCON_desc2
POLCON_desc3 <- mystats(POLCON_desc2$POLCONIII.mean)
POLCON_desc3
class(POLCON_desc3) # Should be matrix
# Converting to matrix and trying to name
POLCON_desc3 <- as.matrix(POLCON_desc3)
dimnames(POLCON_desc3) <- list(c("N", "Mean", "Median", "Max", "Min", "Std.dev."),c("POLCONIII"))
POLCON_desc3
# Transposing
POLCON_desc3 <- t(POLCON_desc3)
POLCON_desc3

# For POLCONV
POLCON_desc4 <- summaryBy(POLCONV~CTRYNM, data=POLCON, FUN=mean, na.rm = TRUE)
POLCON_desc4
# Remove countries with missing values
POLCON_desc4 <- POLCON_desc4[complete.cases(POLCON_desc4), ]
POLCON_desc4 <- mystats(POLCON_desc4$POLCONV.mean)
POLCON_desc4
class(POLCON_desc4) # Should be matrix
# Converting to matrix and trying to name
POLCON_desc4 <- as.matrix(POLCON_desc4)
dimnames(POLCON_desc4) <- list(c("N", "Mean", "Median", "Max", "Min", "Std.dev."),c("POLCONV"))
POLCON_desc4
# Transposing
POLCON_desc4 <- t(POLCON_desc4)
POLCON_desc4

# For POLCONVJ
POLCON_desc5 <- summaryBy(POLCONVJ~CTRYNM, data=POLCON, FUN=mean, na.rm = TRUE)
POLCON_desc5
# Remove countries with missing values
POLCON_desc5 <- POLCON_desc5[complete.cases(POLCON_desc5), ]
POLCON_desc5 <- mystats(POLCON_desc5$POLCONVJ.mean)
POLCON_desc5
class(POLCON_desc5) # Should be matrix
# Converting to matrix and trying to name
POLCON_desc5 <- as.matrix(POLCON_desc5)
dimnames(POLCON_desc5) <- list(c("N", "Mean", "Median", "Max", "Min", "Std.dev."),c("POLCONVJ"))
POLCON_desc5
# Transposing
POLCON_desc5 <- t(POLCON_desc5)
POLCON_desc5

# Merge all POLCON measures
POLCON_desc3
POLCON_desc6 <- rbind(POLCON_desc3, POLCON_desc4)
POLCON_desc7 <- rbind(POLCON_desc6, POLCON_desc5)
POLCON_desc7

## Making table with Henisz results
heniszPOLCON <- matrix(c(121,0.25,0.00,0.88,0.00,0.33, 44,0.29,0.26,0.80,0.00,0.27), nrow = 2,
                       ncol = 6, byrow = TRUE, 
                       dimnames = list(c("POLCON", "POLCONJ"),
                                       c("N", "Mean", "Median", "Max", "Min", "Std.dev.")))
heniszPOLCON

# Getting overview of years
ls(POLCON)
#POLCON <- na.omit(POLCON)
POLCCONdesc8 <- ddply(POLCON,~CTRYNM,summarise,
                   POLCONVJ=mean(POLCONVJ),
                   POLCONIII=mean(POLCONIII),
                   POLCONV=mean(POLCONV),
                   min.yr=min(Year), 
                   max.yr=max(Year)
)
POLCCONdesc8


## @knitr POLCON_tab
library(xtable)
print(xtable(POLCON_desc, label='POLCON_tab',caption='Descriptive statistics of the variables on political constraints', table.placement = h)) # Output as LaTeX.
print(xtable(POLCON_desc7, label='POLCON_tab2',caption='Descriptive statistics of the variables on political constraints on a country level', table.placement = h)) # Output as LaTeX.
print(xtable(heniszPOLCON, label='heniszPOLCON',caption='Descriptive statistics of the variables on political constraints as obtained by Henisz (2000) on a country level', table.placement = h)) # Output as LaTeX.
                      
## @knitr law_order
#### ICRG measure from QoG: icrgQoG ####
ls()
# icrg_qog <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/QoG/1358067_qog_tsd_csv_v6apr11.csv", sep=";")
# icrg_qog <- icrg_qog[c("ccodewb", "year", "icrg_qog")] # Dropping irrelevant variables
# write.csv(icrg_qog, file = "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/QoG/icrg_qog.csv") # Writing data set, because the original is very large
icrgQoG <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/QoG/icrg_qog.csv")
icrgQoG$X <- NULL # Deleting column X, don't know where it comes from
icrgQoG <- rename(icrgQoG, c(icrg_qog ="icrgQoG")) # Renaming variables
icrgQoG$l.icrgQoG <- log(icrgQoG$icrgQoG) # Log transformation
IQM_pro_data <- merge(POLCON, icrgQoG, by.x=c("CTRYNM", "Year"), by.y=c("ccodewb", "year")) # Merging data with main data set
is.numeric(IQM_pro_data$icrgQoG) # Checking data is numeric
ls(IQM_pro_data)

summary(plm(gdpgrowth ~ linigdp + yr.sch.secM + yr.sch.secF + lexpec + lfert +  kg + lbmp + ToT + 
              ki + POLCONV, 
            data = IQM_pro_data, 
            model = "pooling"))

## @knitr democ
#### Polity: democ ####
ls()
polIV <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Polity IV/p4v2011.csv")
ls(polIV)
polIV <- polIV[c("scode", "year", "democ")] # Dropping irrelevant variables
ls(polIV)
library(psych)
# describe(polIV$democ)
# Deleting -66, -77 and -88 observations
polIV <- subset(polIV, democ > -65, select=1:3)
polIV$democ2 <- polIV$democ^2
ls(polIV)
# print(xtable(describe(polIV$democ, skew = FALSE)))
# hist(polIV$democ, breaks = 10, col = "red", xlab="Democracy Score", main="Histogram of democ")
ls(IQM_pro_data)
IQM_pro_data <- merge(IQM_pro_data, polIV, by.x=c("CTRYNM", "Year"), by.y=c("scode", "year")) # Merging data with main data set
ls(IQM_pro_data)

## @knitr PWT
#### Penn World Table: ki, kg, rgdl ####

## Reading PWT data set
# pwt71 <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/pwt71_11302012version/pwt71_wo_country_names_wo_g_vars.csv")
# head(pwt71)
# # write to Stata file
# library(foreign)
# write.dta(pwt71, "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/pwt71a.dta", version=10)
# # This command runs the Stata do file in the specified folder
# system("PATH=$PATH:/Applications/Stata/Stata.app/Contents/MacOS/:. ; Stata -e do /Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/CoE_thesis_repository/Master_thesis.do") # It is needed to calculate the growth rate
# # Read Stata file
pwt71 <- read.dta("/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/pwt71.dta")
ls(pwt71)
head(pwt71)
# Dropping irrelevant variables
str(pwt71)
pwt71 <- pwt71[c("isocode", "year", "rgdpl", "kg", "ki", "rgdpl2", "gdpgrowth")]
ls(pwt71)
pwt71 <- na.omit(pwt71) # Removing missing observations
head(pwt71, n=25)

# Descriptive statistics for rgdpl
rgdpl <- summaryBy(rgdpl~isocode, data=pwt71, FUN=mean, na.rm = TRUE)
rgdpl
rgdpl <- mystats(rgdpl$rgdpl.mean)
rgdpl
class(rgdpl) # Should be matrix
# Converting to matrix and trying to name
rgdpl <- as.matrix(rgdpl)
dimnames(rgdpl) <- list(c("N", "Mean", "Median", "Max", "Min", "Std.dev."),c("rgdpl"))
rgdpl
# Transposing
rgdpl <- t(rgdpl)
rgdpl

# Descriptive statistics of gdpgrowth
library(plyr)
ls(pwt71)
head(pwt71)
pwt71 <- na.omit(pwt71)
head(pwt71)
gdpgrowth9 <- ddply(pwt71,~isocode,summarise,mean=mean(gdpgrowth, 
                                                       na.rm = TRUE), 
                    max.yr=max(year), 
                    min.yr=min(year))
gdpgrowth9
gdpgrowth9$mean[gdpgrowth9$isocode=='SLE']
pdf('/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/CoE_thesis_repository/figure/growth_hist.pdf')
hist(gdpgrowth9$mean, breaks = 20, main=NULL, xlab="GDP growth")
dev.off()
mystats2 <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  med <- median (x)
  n <- length(x)
  s <- sd(x)
  min <- min(x)
  max <- max(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, median=med, min=min, max=max, stdev=s))
}
gdpgrowth9stats <- mystats2(gdpgrowth9$mean)
gdpgrowth9stats <- as.data.frame(gdpgrowth9stats)
gdpgrowth9stats <- t(gdpgrowth9stats)
rownames(gdpgrowth9stats) <- "GDP growth"
print(xtable(gdpgrowth9stats, label='gdpgrowth9stats',
             caption='Descriptive statistics of growth variable before trimming', 
             table.placement = h),
      file='gdpgrowth9stats.tex') # Output as LaTeX.


## Create variable with initial gdp value
# Create new variable that is missing if rgdpl is missing and otherwise year to use for function that looks up earliest year that is not missing
pwt71$yearNA <- ifelse(is.na(pwt71$rgdpl), pwt71$yearNA <- NA, pwt71$year)
# Show data
head(pwt71, n=10)
ls(pwt71)
# Here the pwt71 data is split into country groups along with variable year and rgdpl
countries <- split(pwt71[c("rgdpl", "year", "yearNA")], pwt71$isocode)
# Show the first country in the list countries
countries[1]
# Create function that finds gdp in earliest year that is not missing
vlookup7 <- function(df){
  df[df[2] == min(df[2], na.rm=TRUE), 1]
}
# Use lapply
inigdp <- lapply(countries, vlookup7)
inigdp[1]
# Remove NAs
inigdp <- lapply(inigdp, function(x) x[!is.na(x)])
inigdp
# Re-assembling the data
pwt71$inigdp <- unsplit(inigdp, pwt71$isocode)
head(pwt71, n=10)
# Deleting the variable yearNA
pwt71$yearNA <- NULL
# Creating log
pwt71$linigdp <- log(pwt71$inigdp)
head(pwt71, n=10)

## Correlation between inigdp and gdpgrowth
# Create country values
gdpgrowth11 <- ddply(pwt71,~isocode,summarise,mean.growth=mean(gdpgrowth, na.rm = TRUE),mean.linigdp=mean(linigdp))
gdpgrowth11
pdf('/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/CoE_thesis_repository/figure/growth_inigdp.pdf')
plot(gdpgrowth11$mean.linigdp, gdpgrowth11$mean.growth)
dev.off()


# Merging Penn World Table and POLCON
IQM_pro_data <- merge(IQM_pro_data, pwt71, by.x=c("CTRYNM", "Year"), by.y=c("isocode", "year")) 
ls(IQM_pro_data)

## Read other gdp growth variable
gdp.growth <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Lost_decades_macro_time_series_6_2001_gdp_growth.csv", check.names=FALSE, na.strings = "..", sep = ";", colClasses=c(rep("character", 2),rep("numeric",39)), skip = 2, nrows = 208)
gdp.growth$Country_name <- NULL
library(reshape)
gdp.growth <- melt(gdp.growth, id=c("country_code")) # Reshaping data
gdp.growth <- rename(gdp.growth, c(value="gdp.growth", variable="year")) # Renaming variables
gdp.growth$gdp.growth <- as.numeric(gdp.growth$gdp.growth)
is.numeric(gdp.growth$gdp.growth)
gdp.growth$lgdp.growth <- log(gdp.growth$gdp.growth)
is.numeric(gdp.growth$lgdp.growth)
# Merging data with main data set
IQM_pro_data <- merge(IQM_pro_data, gdp.growth, by.x=c("CTRYNM", "Year"), by.y=c("country_code", "year"))
is.numeric(IQM_pro_data$gdp.growth)
ls(IQM_pro_data)
# Comparing gdp variables
cor(IQM_pro_data$gdp.growth, IQM_pro_data$gdpgrowth, use='complete.obs')

## Read Stata file with balanced data set
pwt71a <- read.dta("/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/pwt71b.dta")
# Descriptive statistics of gdpgrowth with new data set
library(plyr)
ls(pwt71a)
head(pwt71a)
gdpgrowth10 <- ddply(pwt71a,~isocode,summarise,mean=mean(gdpgrowth, na.rm = TRUE), 
                     max.yr=max(year), min.yr=min(year))
gdpgrowth10
pdf('/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/CoE_thesis_repository/figure/growth_hist2.pdf')
hist(gdpgrowth10$mean, breaks = 20, main=NULL, xlab="GDP growth")
dev.off()
gdpgrowth10stats <- mystats2(gdpgrowth10$mean)
gdpgrowth10stats <- as.data.frame(gdpgrowth10stats)
gdpgrowth10stats <- t(gdpgrowth10stats)
rownames(gdpgrowth10stats) <- "GDP growth"
print(xtable(gdpgrowth10stats, label='gdpgrowth10stats',
             caption='Descriptive statistics of growth variable after trimming', 
             table.placement = h),
      file='gdpgrowth10stats.tex') # Output as LaTeX.

## Testing OLS models with data I already have
ls(IQM_pro_data)
library(plm)
summary(pooled01 <- plm(gdpgrowth ~ linigdp + kg + ki + POLCONV, 
                       data = IQM_pro_data, 
                       model = "pooling"))
# Checking whether I get the same result wit the simple lm procedure - I do.
summary(lm01 <- lm(gdpgrowth ~ linigdp + POLCONV, data = IQM_pro_data)) 
summary(pooled02 <- plm(gdpgrowth ~ linigdp + POLCONIII, 
                        data = IQM_pro_data, 
                        model = "pooling"))

# Descriptive statistics for kg
kg <- pwt71[c("kg")]
is.numeric(kg)
kg_desc <- sapply(kg, mystats, na.omit=TRUE)
kg_desc <- t(kg_desc) # Transposing the matrix
kg_desc
## @knitr kg
library(xtable)
print(xtable(kg_desc, label='kg_tab',caption='Descriptive statistics of Government Consumption', table.placement = h)) # Output as LaTeX.

## @knitr rgdpl
print(xtable(rgdpl, label='rgdpl',caption='Descriptive statistics of rgdpl', sanitize.text.function = function(x){x}, table.placement = h)) # Output as LaTeX.

## @knitr educ
#### Barro-Lee: yr.sch.secF, yr.sch.secM ####
# Data for total population
library(foreign)
educMF <- read.dta("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/BL2013_MF1599_v1.3.dta_full.dta") 
ls(educMF)
# Dropping irrelevant variables
educMF <- educMF[c("WBcode", "year", "yr_sch_sec_full")] 
# Data for females
educF <- read.dta("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/BL2013_F1599_v1.3.dta_full.dta") 
# Dropping irrelevant variables
educF <- educF[c("WBcode", "year", "yr_sch_sec_full")] 
head(educF)
# Package needed for rename() function
# install.packages("reshape")
library(reshape) 
# Renaming the variable of interest to distinguish between female and total population in merged data set
educF <- rename(educF, c(yr_sch_sec_full="yr.sch.secF"))
# Merging education data sets
educ <- merge(educMF, educF, by=c("WBcode", "year"))
# Seeing data
head(educ)
# Calculating average for males
educ$yr.sch.secM <- (2*educ$yr_sch_sec - educ$yr.sch.secF) 
# Dropping variable for total population (only male and female left)
educ$yr_sch_sec <- NULL 
ls(IQM_pro_data)

datadesc1 <- ddply(IQM_pro_data,~CTRYNM,summarise,
                   mean.growth=mean(gdpgrowth),
                   inigdp=mean(inigdp),
                   POLCONVJ=mean(POLCONVJ),
                   POLCONIII=mean(POLCONIII),
                   POLCONV=mean(POLCONV),
                   min.yr=min(Year), 
                   max.yr=max(Year))
datadesc1
# Merging the two above with Barro-Lee data set. 
  # NB: Contains only data for every fifth year, so the data set is drastically reduced.
IQM_pro_data <- merge(IQM_pro_data, educ, 
                      by.x=c("CTRYNM", "Year"), 
                      by.y=c("WBcode", "year"), 
                      all=TRUE) 
summary(IQM_pro_data)

## Testing OLS models with data I already have
ls(IQM_pro_data)
library(plm)
summary(plm(gdpgrowth ~ linigdp + yr.sch.secM + yr.sch.secF + kg + ki + POLCONV, 
                        data = IQM_pro_data, 
                        model = "pooling"))
summary(plm(gdpgrowth ~ linigdp + yr.sch.secM + yr.sch.secF + kg + ki + POLCONIII, 
                        data = IQM_pro_data, 
                        model = "pooling"))

## @knitr lexpec
#### Life expectancy: lexpec ####
lexpec <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/SP.DYN.LE00.IN_Indicator_MetaData_en_EXCEL.csv", check.names=FALSE)
lexpec$"Country Name" <- NULL # Dropping irrelevant variables
# install.packages("reshape")
library(reshape)
lexpec <- melt(lexpec, id=c("Country Code")) # Reshaping data
lexpec <- rename(lexpec, c(value="lexpec", variable="year", "Country Code"="country.code")) # Renaming variables
lexpec$llexpec <- log(lexpec$lexpec) # Here the variable is log-transformed
ls(IQM_pro_data)
IQM_pro_data <- merge(IQM_pro_data, lexpec, by.x=c("CTRYNM", "Year"), by.y=c("country.code", "year")) # Merging data with main data set
ls(IQM_pro_data)

## Testing OLS models with data I already have
ls(IQM_pro_data)
library(plm)
summary(plm(gdpgrowth ~ linigdp + yr.sch.secM + yr.sch.secF + kg + llexpec +
              ki + POLCONIII, 
            data = IQM_pro_data, 
            model = "pooling"))

## @knitr fert
#### Fertility rates: fert ####
fert <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/SP.DYN.TFRT.IN_Indicator_MetaData_en_EXCEL.csv", check.names=FALSE)
fert$"Country Name" <- NULL # Dropping irrelevant variables
fert <- melt(fert, id=c("Country Code")) # Reshaping data
fert <- rename(fert, c(value="fert", variable="year", "Country Code"="country.code")) # Renaming variables
fert$lfert <- log(fert$fert) # Here the variable is log-transformed
IQM_pro_data <- merge(IQM_pro_data, fert, by.x=c("CTRYNM", "Year"), by.y=c("country.code", "year")) # Merging data with main data set
ls(IQM_pro_data)

## Testing OLS models with data I already have
ls(IQM_pro_data)
library(plm)
summary(plm(gdpgrowth ~ linigdp + yr.sch.secM + yr.sch.secF + kg + 
              llexpec + lfert +
              ki + POLCONIII, 
            data = IQM_pro_data, 
            model = "pooling"))

## @knitr bmp
#### Black market premium: bmp ####
bmp <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Black_market_premium.csv", check.names=FALSE, na.strings = "..", sep = ";", colClasses=c(rep("character", 2),rep("numeric",40)))
bmp$"Country Name" <- NULL # Dropping irrelevant variables
bmp <- melt(bmp, id=c("Country Code")) # Reshaping data
bmp <- rename(bmp, c(value="bmp", variable="year", "Country Code"="country.code")) # Renaming variables
is.numeric(bmp$bmp)
# as.numeric(bmp$year) # Trying to make the variable numeric.
is.numeric(bmp$year)
bmp_vars <- bmp[c("bmp")]
is.numeric(bmp_vars$bmp)
bmp_desc <- sapply(bmp_vars, mystats, na.omit=TRUE)
bmp_desc <- t(bmp_desc) # Transposing the matrix
bmp_desc
# hist(IQM_pro_data$bmp)

bmp$lbmp <- log(bmp$bmp) # Here the variable is log-transformed
summary(bmp$lbmp, na.rm = TRUE)
bmp_vars <- bmp[c("bmp", "lbmp")]
is.numeric(bmp_vars$bmp)
bmp_desc <- sapply(bmp_vars, mystats, na.omit=TRUE)
bmp_desc <- t(bmp_desc) # Transposing the matrix
is.numeric(bmp$lbmp)

IQM_pro_data <- merge(IQM_pro_data, bmp, by.x=c("CTRYNM", "Year"), by.y=c("country.code", "year")) # Merging data with main data set
is.numeric(IQM_pro_data$bmp) # Checking data is numeric
ls(IQM_pro_data)

# Descriptive statistics
bmp1 <- ddply(IQM_pro_data,~CTRYNM,summarise,mean=mean(bmp, 
                                                       na.rm = TRUE), 
                    max.yr=max(Year), 
                    min.yr=min(Year))
bmp1

## Testing OLS models with data I already have
ls(IQM_pro_data)
library(plm)
summary(plm(gdpgrowth ~ linigdp + yr.sch.secM + yr.sch.secF + kg + 
              llexpec + lfert +
              ki + POLCONIII, 
            data = IQM_pro_data, 
            model = "pooling"))

# ## @knitr test-plot
# hist(IQM_pro_data$bmp)

## @knitr bmp_tab
library(xtable)
print(xtable(bmp_desc, label='bmp_tab',caption='Descriptive statistics of the variables on black market premium', table.placement = h)) # Output as LaTeX.
hist(bmp$lbmp, main=NULL, xlab="Value of Black Market Premium")

## @knitr ToT
#### Terms of trade: ToT ####
ToT <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/ToT.csv", check.names=FALSE, sep = ";", na.strings = "..")
ToT$"Country Name" <- NULL # Dropping irrelevant variables
ToT <- melt(ToT, id=c("Country Code")) # Reshaping data
ToT <- rename(ToT, c(value="ToT", variable="year", "Country Code"="country.code")) # Renaming variables
IQM_pro_data <- merge(IQM_pro_data, ToT, by.x=c("CTRYNM", "Year"), by.y=c("country.code", "year")) # Merging data with main data set
is.numeric(IQM_pro_data$ToT) # Checking data is numeric
ls(IQM_pro_data)
summary(plm(gdpgrowth ~ linigdp + yr.sch.secM + yr.sch.secF + lexpec + lfert +  kg + lbmp + ToT + 
              ki + POLCONV, 
            data = IQM_pro_data, 
            model = "pooling"))



## Testing OLS models with data I already have
ls(IQM_pro_data)
library(plm)
summary(plm(gdpgrowth ~ linigdp + yr.sch.secM + yr.sch.secF + kg + 
              llexpec + lfert + lbmp + ToT +
              ki + POLCONIII, 
            data = IQM_pro_data, 
            model = "pooling"))

#### QoG ####
# Set data wd
setwd("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data")
QoG_basic_ts <- 
  read.csv("QoG/1373433_qog_basic_ts_csv_120608.csv", header=T)
ls(QoG_basic_ts)
QoG_basic_ts <- QoG_basic_ts[c("ccodewb", "year", "ht_region")]
ls(QoG_basic_ts)
write.csv(QoG_basic_ts, 
          file = "QoG/1373433_qog_basic_ts_csv_120608_ed.csv",
          row.names = FALSE)
QoG_basic_ts <- read.csv("QoG/1373433_qog_basic_ts_csv_120608_ed.csv")
ls(QoG_basic_ts)




# Merging data with main data set
IQM_pro_data <- 
  merge(IQM_pro_data, QoG_basic_ts, 
        by.x=c("CTRYNM", "Year"), 
        by.y=c("ccodewb", "year")) 
head(IQM_pro_data)

#### World Bank ####
head(
  WB <- 
    read.csv("World Bank/countries.csv", header=T)
)

# Merging data with main data set
head(
  IQM_pro_data <- 
    merge(IQM_pro_data, WB, 
        by.x=c("CTRYNM"), 
        by.y=c("Code"),
        all.x = TRUE)
)

# Create dummy variables
IQM_pro_data <- within(IQM_pro_data,{
  south.asia <- NA
  south.asia[IQM_pro_data$Region != "South Asia"] <- 0
  south.asia[IQM_pro_data$Region == "South Asia"] <- 1
})
IQM_pro_data <- within(IQM_pro_data,{
  europe.central.asia <- NA
  europe.central.asia[IQM_pro_data$Region != "Europe & Central Asia"] <- 0
  europe.central.asia[IQM_pro_data$Region == "Europe & Central Asia"] <- 1
})
IQM_pro_data <- within(IQM_pro_data,{
  middle.east.north.africa <- NA
  middle.east.north.africa[IQM_pro_data$Region != "Middle East & North Africa"] <- 0
  middle.east.north.africa[IQM_pro_data$Region == "Middle East & North Africa"] <- 1
})
IQM_pro_data <- within(IQM_pro_data,{
  east.asia.pacific <- NA
  east.asia.pacific[IQM_pro_data$Region != "East Asia & Pacific"] <- 0
  east.asia.pacific[IQM_pro_data$Region == "East Asia & Pacific"] <- 1
})
IQM_pro_data <- within(IQM_pro_data,{
  sub.saharan.africa <- NA
  sub.saharan.africa[IQM_pro_data$Region != "Sub-Saharan Africa"] <- 0
  sub.saharan.africa[IQM_pro_data$Region == "Sub-Saharan Africa"] <- 1
})
IQM_pro_data <- within(IQM_pro_data,{
  latin.america.caribbean <- NA
  latin.america.caribbean[IQM_pro_data$Region != "Latin America & Caribbean"] <- 0
  latin.america.caribbean[IQM_pro_data$Region == "Latin America & Caribbean"] <- 1
})
head(IQM_pro_data)
  
  
  


## @knitr datadesc
#### Data description ####
ls()
# Creating data set only with numeric variables for descriptive statistics
ls(IQM_pro_data) # Show variables
IQM_pro_data_vars <- IQM_pro_data
ls(IQM_pro_data_vars) # Show variables
IQM_pro_data_vars$CTRYNM <- NULL # Deleting country name
IQM_pro_data_vars$Year <- NULL # Deleting year
ls(IQM_pro_data_vars)

### Trying only to make statistics that I need
IQM_my_desc <- sapply(IQM_pro_data_vars, mystats2, na.omit=TRUE)
is.matrix(IQM_my_desc)
IQM_my_desc
tIQM_my_desc <- t(IQM_my_desc) # Transposing the matrix
tIQM_my_desc
# rownames(IQM_my_desc) <- c("POLCONIII", "POLCONV", "POLCONVJ","Law and Order from ICRG", "Real Per Capita GDP Growth", "Government Consumption (% GDP)", "Total Investment (% GDP)", "Alternative measure of GDP growth", "Log(Life Expectancy)", "Log(Fertility Rate)", "Black Market Premium", "Terms of trade", "Log(ICRG Risk Measure)", "Democracy Index (PolityIV)") # Giving new names to variables to table.

## @knitr all_var
print(xtable(tIQM_my_desc, 
             label='tabsmall',
             caption='Descriptive statistics of the variables used', 
             digits=2, 
             sanitize.text.function = function(x){x}, 
             table.placement = h), 
      floating.environment='sidewaystable', 
      digits = 2) # Output as LaTeX.

## @knitr new-label
#### Stata data ####
# install.packages("foreign")
library(foreign) # Package needed for the write.dta() function
ls(IQM_pro_data)
write.dta(IQM_pro_data, "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/IQM_pro_data.dta", version=10)
write.csv(IQM_pro_data, "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/thesis_data.csv")
# # This command runs the Stata do file in the specified folder
# system("PATH=$PATH:/Applications/Stata/Stata.app/Contents/MacOS/:. ; Stata -e do /Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/CoE_thesis_repository/Master_thesis.do") # It is needed to calculate the growth rate
# Read Stata data
# Statadata <- read.dta("/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Statadata.dta")
# ls(Statadata)
# head(Statadata)
# # Remove unnecessary variables
# Statadata <- Statadata[c("CTRYNM", "Year", "gdpgrowth")]
# # Merge Statadata with main dataset
# IQM_pro_data <- merge(Statadata, IQM_pro_data, by.x=c("CTRYNM", "Year"), by.y=c("CTRYNM", "Year")) 
ls(IQM_pro_data)

# Maps
library(maps)
map(database = "world")

## @knitr saving
# Checking dataset
head(IQM_pro_data, n=100)
# Saving data set
write.csv(IQM_pro_data, file = "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/IQM_pro_data.csv", row.names = FALSE) 

## @knitr analysis
#### Analysis ####
# install.packages("plm")
library(plm) # Package for panel data model, see Croissant and Millo (2008)

# Make human capital variable (many codes so done in other file)
setwd("/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis")
source("CoE_thesis_repository/human.capital.R",
       local = FALSE, echo = TRUE)

# First we make an object with all the variable names to avoid repetition
var1 <- c("linigdp + yr.sch.secM + yr.sch.secF + llexpec + lfert + kg + ToT + ki + south.asia + east.asia.pacific + europe.central.asia + latin.america.caribbean + middle.east.north.africa + sub.saharan.africa + llaw.order")
model1 <- paste("gdpgrowth ~ ",var1,sep = "")
model1 <- as.formula(model1)

var1 <- c("linigdp", "yr.sch.secM", "yr.sch.secF", "llexpec",
          "lfert", "kg", "ToT", "ki", "south.asia",
          "east.asia.pacific",
          "latin.america.caribbean", "middle.east.north.africa",
          "sub.saharan.africa", "llaw.order")
model1 <- paste(var1, sep="")
model1 <- as.formula(paste("gdpgrowth ~", paste(model1, collapse = "+")))
model1

summary(test1 <- plm(model1, data = IQM_pro_data, effect="time"))

# pooled.time regression (time effects)
summary(pooled.time1 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + 
                         south.asia + east.asia.pacific +
                         latin.america.caribbean + middle.east.north.africa + 
                         sub.saharan.africa + llaw.order,  
                       data = IQM_pro_data, 
                       model = "pooling",
                       effect="time"))
summary(pooled.time2 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                       ki +  
                         south.asia + east.asia.pacific +
                         latin.america.caribbean + middle.east.north.africa + 
                              sub.saharan.africa + 
                              democ + democ2, 
                       data = IQM_pro_data, 
                       model = "pooling",
                       effect="time"))
summary(pooled.time3 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + POLCONIII + 
                         south.asia + east.asia.pacific +
                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                       data = IQM_pro_data, 
                       model = "pooling",
                       effect="time"))
summary(pooled.time4 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + POLCONV + 
                         south.asia + east.asia.pacific +
                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                       data = IQM_pro_data, 
                       model = "pooling",
                       effect="time"))
summary(pooled.time5 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + POLCONVJ + 
                         south.asia + east.asia.pacific +
                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                       data = IQM_pro_data, 
                       model = "pooling",
                       effect="time"))
summary(pooled.time6 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + l.icrgQoG + 
                         south.asia + east.asia.pacific +
                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                       data = IQM_pro_data, 
                       model = "pooling",
                       effect="time"))

# fixed.time effects (time)
summary(fixed.time1 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                        yr.sch.secF + llexpec + lfert + kg + ToT +
                        ki + 
                        south.asia + east.asia.pacific +
                        latin.america.caribbean + middle.east.north.africa + 
                        sub.saharan.africa + llaw.order, 
                       data = IQM_pro_data, 
                       model = "within",
                       effect="time"))
summary(fixed.time2 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + 
                         south.asia + east.asia.pacific  +
                         latin.america.caribbean + middle.east.north.africa + 
                             sub.saharan.africa +  democ + democ2, 
                       data = IQM_pro_data, 
                       model = "within",
                       effect="time"))
summary(fixed.time3 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + POLCONIII + 
                         south.asia + east.asia.pacific  +
                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                       data = IQM_pro_data, 
                       model = "within",
                       effect="time"))
summary(fixed.time4 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + POLCONV + 
                         south.asia + east.asia.pacific  +
                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                       data = IQM_pro_data, 
                       model = "within",
                       effect="time"))
summary(fixed.time5 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + POLCONVJ + 
                         south.asia + east.asia.pacific + 
                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                       data = IQM_pro_data, 
                       model = "within",
                       effect="time"))
summary(fixed.time6 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
                         yr.sch.secF + llexpec + lfert + kg + ToT +
                         ki + l.icrgQoG + 
                         south.asia + east.asia.pacific  +
                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                       data = IQM_pro_data, 
                       model = "within",
                       effect="time"))

# fixed effects (twoways)
# summary(fixed.twoways1 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + 
#                              south.asia + east.asia.pacific +
#                              latin.america.caribbean + middle.east.north.africa + 
#                              sub.saharan.africa + llaw.order, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways2 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + democ + democ2 + 
#                              south.asia + east.asia.pacific  +
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways3 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + POLCONIII + 
#                              south.asia + east.asia.pacific  +
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways4 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + POLCONV + 
#                              south.asia + east.asia.pacific  +
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways5 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg +
#                              ki + POLCONVJ + 
#                              south.asia + east.asia.pacific + 
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))
# summary(fixed.twoways6 <- plm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                              yr.sch.secF + llexpec + lfert + kg + ToT +
#                              ki + l.icrgQoG + 
#                              south.asia + east.asia.pacific  +
#                              latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                            data = IQM_pro_data, 
#                            model = "within",
#                            effect="twoways"))



# Test of region dummies - virker ikke
# summary(pooled.time1r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + llaw.order, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time2r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + democ + democ2, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time3r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + POLCONIII, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time4r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + POLCONV, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time5r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + POLCONVJ, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# summary(pooled.time6r <- plm(gdpgrowth ~ inigdp + yr.sch.secM + 
#                          yr.sch.secF + llexpec + lfert + kg + ToT +
#                          ki + l.icrgQoG, 
#                        data = IQM_pro_data, 
#                        model = "pooling"))
# 
# anova(pooled.time1,pooled.time1r)
# anova(pooled.time2,pooled.time2r)
# anova(pooled.time3,pooled.time3r)
# anova(pooled.time4,pooled.time4r)
# anova(pooled.time5,pooled.time5r)
# anova(pooled.time6,pooled.time6r)

## @knitr GLS.time
summary(GLS.time2 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + 
                             south.asia + east.asia.pacific  +
                             latin.america.caribbean + middle.east.north.africa + 
                             sub.saharan.africa +  democ + democ2, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(GLS.time3 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + POLCONIII + 
                             south.asia + east.asia.pacific  +
                             latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(GLS.time4 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + POLCONV + 
                             south.asia + east.asia.pacific  +
                             latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(GLS.time5 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + POLCONVJ + 
                             south.asia + east.asia.pacific + 
                             latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(GLS.time1 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + 
                             south.asia + east.asia.pacific +
                             latin.america.caribbean + middle.east.north.africa + 
                             sub.saharan.africa + llaw.order, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))
summary(GLS.time6 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                             yr.sch.secF + llexpec + lfert + kg + ToT +
                             ki + l.icrgQoG + 
                             south.asia + east.asia.pacific  +
                             latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                           data = IQM_pro_data, 
                           model = "within",
                           effect="time"))


## @knitr GLS.individual
summary(GLS.individual2 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                                  yr.sch.secF + llexpec + lfert + kg + ToT +
                                  ki + 
                                  south.asia + east.asia.pacific  +
                                  latin.america.caribbean + middle.east.north.africa + 
                                  sub.saharan.africa +  democ + democ2, 
                                data = IQM_pro_data, 
                                model = "within",
                                effect="individual"))
summary(GLS.individual3 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                                  yr.sch.secF + llexpec + lfert + kg + ToT +
                                  ki + POLCONIII + 
                                  south.asia + east.asia.pacific  +
                                  latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                                data = IQM_pro_data, 
                                model = "within",
                                effect="individual"))
summary(GLS.individual4 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                                  yr.sch.secF + llexpec + lfert + kg + ToT +
                                  ki + POLCONV + 
                                  south.asia + east.asia.pacific  +
                                  latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                                data = IQM_pro_data, 
                                model = "within",
                                effect="individual"))
summary(GLS.individual5 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                                  yr.sch.secF + llexpec + lfert + kg + ToT +
                                  ki + POLCONVJ + 
                                  south.asia + east.asia.pacific + 
                                  latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                                data = IQM_pro_data, 
                                model = "within",
                                effect="individual"))
summary(GLS.individual1 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                                   yr.sch.secF + llexpec + lfert + kg + ToT +
                                   ki + 
                                   south.asia + east.asia.pacific +
                                   latin.america.caribbean + middle.east.north.africa + 
                                   sub.saharan.africa + llaw.order, 
                                 data = IQM_pro_data, 
                                 model = "within",
                                 effect="individual"))
summary(GLS.individual6 <- pggls(gdpgrowth ~ linigdp + yr.sch.secM + 
                                  yr.sch.secF + llexpec + lfert + kg + ToT +
                                  ki + l.icrgQoG + 
                                  south.asia + east.asia.pacific  +
                                  latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
                                data = IQM_pro_data, 
                                model = "within",
                                effect="individual"))


## @knitr gmm
# summary(GMM1 <- pgmm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                         yr.sch.secF + llexpec + lfert + kg + ToT +
#                         ki + 
#                         south.asia + east.asia.pacific +
#                         latin.america.caribbean + middle.east.north.africa + 
#                         sub.saharan.africa + llaw.order, 
#                       data = IQM_pro_data, 
#                       model = "within",
#                       effect="twoways",
#                      model = "onestep"))
# summary(GMM2 <- pgmm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                         yr.sch.secF + llexpec + lfert + kg + ToT +
#                         ki + 
#                         south.asia + east.asia.pacific  +
#                         latin.america.caribbean + middle.east.north.africa + 
#                         sub.saharan.africa +  democ + democ2, 
#                       data = IQM_pro_data, 
#                       model = "within",
#                       effect="time"))
# summary(GMM3 <- pgmm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                         yr.sch.secF + llexpec + lfert + kg + ToT +
#                         ki + POLCONIII + 
#                         south.asia + east.asia.pacific  +
#                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                       data = IQM_pro_data, 
#                       model = "within",
#                       effect="time"))
# summary(GMM4 <- pgmm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                         yr.sch.secF + llexpec + lfert + kg + ToT +
#                         ki + POLCONV + 
#                         south.asia + east.asia.pacific  +
#                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                       data = IQM_pro_data, 
#                       model = "within",
#                       effect="time"))
# summary(GMM5 <- pgmm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                         yr.sch.secF + llexpec + lfert + kg + ToT +
#                         ki + POLCONVJ + 
#                         south.asia + east.asia.pacific + 
#                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                       data = IQM_pro_data, 
#                       model = "within",
#                       effect="time"))
# summary(GMM6 <- pgmm(gdpgrowth ~ linigdp + yr.sch.secM + 
#                         yr.sch.secF + llexpec + lfert + kg + ToT +
#                         ki + l.icrgQoG + 
#                         south.asia + east.asia.pacific  +
#                         latin.america.caribbean + middle.east.north.africa + sub.saharan.africa, 
#                       data = IQM_pro_data, 
#                       model = "within",
#                       effect="time"))



## @knitr balanceddata
# Henter balanceret datasaett
setwd("/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis")
IQM_pro_data_balanced <- read.dta("Data/IQM_pro_data_balanced.dta")
ls(IQM_pro_data_balanced)
summary(fixed_twoways1 <- plm(gdpgrowth ~ linigdp + yr_sch_secM + 
                             yr_sch_secF + llexpec + lfert + kg + ToT +
                             ki + 
                             south_asia + east_asia_pacific +
                             latin_america_caribbean + middle_east_north_africa + 
                             sub_saharan_africa + llaw_order, 
                           data = IQM_pro_data_balanced, 
                           model = "within",
                           effect="twoways"))




library(estout)
## @knitr analysis2
# eststo(fixed.time_effects1)
# eststo(fixed.time_effects2)
# eststo(fixed.time_effects3)
# eststo(fixed.time_effects4)
# eststo(fixed.time_effects5)
# eststo(fixed.time_effects6)
# esttab(label = "fe3", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from fixed.time effects estimation (individual effects)", caption.top=FALSE, table.pos="p", texfontsize="\\small")
# estclear()
# # fixed.time effects, twoways
# eststo(fixed.time_effects1a)
# eststo(fixed.time_effects2a)
# eststo(fixed.time_effects3a)
# eststo(fixed.time_effects4a)
# eststo(fixed.time_effects5a)
# eststo(fixed.time_effects6a)
# esttab(label = "fe4", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from fixed.time effects estimation (individual and time effects)", caption.top=FALSE, table.pos="p", texfontsize="\\small")
# estclear()
# # FE, Time effects
# eststo(fixed.time_effects1b)
# eststo(fixed.time_effects2b)
# eststo(fixed.time_effects3b)
# eststo(fixed.time_effects4b)
# eststo(fixed.time_effects5b)
# eststo(fixed.time_effects6b)
# esttab(label = "fe5", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from fixed.time effects estimation (time effects)", caption.top=FALSE, table.pos="p", texfontsize="\\small")
# estclear()

# fixed.time effects
eststo(fixed.time2)
eststo(fixed.time3)
eststo(fixed.time4)
eststo(fixed.time5)
eststo(fixed.time1)
eststo(fixed.time6)
esttab(label = "fe9", 
       colnumber=TRUE, 
       var.rename=NULL, 
       table="table", 
       caption = "Estimation results from OLS fixed effects model with time effects", 
       caption.top=FALSE, 
       table.pos="p", 
       texfontsize="\\small")
estclear()

# pooled.time
eststo(pooled.time2)
eststo(pooled.time3)
eststo(pooled.time4)
eststo(pooled.time5)
eststo(pooled.time1)
eststo(pooled.time6)
esttab(label = "fe6", 
       colnumber=TRUE, 
       var.rename=NULL, 
       table="table", 
       caption = "Estimation results from OLS pooling model with time effects", 
       caption.top=FALSE, 
       table.pos="p", 
       texfontsize="\\small",
       round.dec=3)
estclear()

# GLS
# eststo(GLS1)
# eststo(GLS2)
# eststo(GLS3)
# eststo(GLS4)
# eststo(GLS5)
# eststo(GLS6)
# esttab(label = "GLS", 
#        colnumber=TRUE, 
#        var.rename=NULL, 
#        table="sidewaystable", 
#        caption = "Estimation results from GLS regression", 
#        caption.top=FALSE, 
#        table.pos="p", 
#        texfontsize="\\small")
# estclear()
# library(texreg)
# texreg(GLS1)


#### Mis-specification #### 
## @knitr mis
plmtest(pooled.time1, effect = "twoways", type = "ghm")

## @knitr mis2
plmtest(pooled.time1, effect = "individual", type="bp")
plmtest(pooled.time2, effect = "individual", type="bp")
plmtest(pooled.time3, effect = "individual", type="bp")
plmtest(pooled.time4, effect = "individual", type="bp")
plmtest(pooled.time5, effect = "individual", type="bp")
plmtest(pooled.time6, effect = "individual", type="bp")

## @knitr mis3
pwtest(pooled.time1)
pwtest(pooled.time2)
pwtest(pooled.time3)
pwtest(pooled.time4)
pwtest(pooled.time5)
pwtest(pooled.time6)


## @knitr mis4
library(lmtest)
pbgtest(pooled.time1)
pbgtest(pooled.time2)
pbgtest(pooled.time3)
pbgtest(pooled.time4)
pbgtest(pooled.time5)
pbgtest(pooled.time6)

## @knitr mis5
pooltest(pooled.time1,fixed.time1)
pooltest(pooled.time2,fixed.time2)
pooltest(pooled.time3,fixed.time3)
pooltest(pooled.time4,fixed.time4)
pooltest(pooled.time5,fixed.time5)
pooltest(pooled.time6,fixed.time6)

## @knitr mis6
plmtest(pooled.time1, effect = "twoways", type = "ghm")
plmtest(pooled.time2, effect = "twoways", type = "ghm")
plmtest(pooled.time3, effect = "twoways", type = "ghm")
plmtest(pooled.time4, effect = "twoways", type = "ghm")
plmtest(pooled.time5, effect = "twoways", type = "ghm")
plmtest(pooled.time6, effect = "twoways", type = "ghm")

## @knitr mis7
plmtest(pooled.time1, effect = "time")
plmtest(pooled.time2, effect = "time")
plmtest(pooled.time3, effect = "time")
plmtest(pooled.time4, effect = "time")
plmtest(pooled.time5, effect = "time")
plmtest(pooled.time6, effect = "time")

## @knitr mis8
plmtest(fixed.time1, effect = "time")
plmtest(fixed.time2, effect = "time")
plmtest(fixed.time3, effect = "time")
plmtest(fixed.time4, effect = "time")
plmtest(fixed.time5, effect = "time")
plmtest(fixed.time6, effect = "time")

## @knitr mis9
plmtest(fixed.time1, effect = "individual", type="bp")
plmtest(fixed.time2, effect = "individual", type="bp")
plmtest(fixed.time3, effect = "individual", type="bp")
plmtest(fixed.time4, effect = "individual", type="bp")
plmtest(fixed.time5, effect = "individual", type="bp")
plmtest(fixed.time6, effect = "individual", type="bp")

## @knitr appendix1
#### Appendix tables ####
print.xtable(xtable(gdpgrowth9, label='gdpgrowth9',
             caption='Mean growth rate for all countries for all years available',
             caption.placement='top'),
      tabular.environment='longtable',
      floating = FALSE) # Output as LaTeX.


## @knitr warnings
warnings()
