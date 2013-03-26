## @knitr POLCON
#### POLCON: POLCONIII, POLCONV, POLCONJ, law.order ####
# POLCON <- read.delim("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Henisz/POLCON_2010_tabsep.txt")
# POLCON <- POLCON[c("CTRYNM", "Year", "POLCONIII", "POLCONV", "POLCONVJ", "Law...Order..from.ICRG.")] # Dropping irrelevant variables
# ls(POLCON) # Checking which variables I have
# write.csv(POLCON, file = "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Henisz/POLCON.csv", row.names = FALSE) # Writing data set, because the original is very large
POLCON <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/Henisz/POLCON.csv")
ls(POLCON)
POLCON$llaw.order <- log(POLCON$Law...Order..from.ICRG.)
# What are the number of observations per country?
summary(POLCON$CTRYNM)
library(foreign)

# Trying to get descriptive statistics by country
# dstats <- function(x)(c(min=min(x), max=max(x))
# POLCON_vars1 <- POLCON[c("Year")]
# by(POLCON_vars1, POLCON$CTRYNM, dstats)
# warnings()

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
  return(c(n=n, mean=m, median=med, min=min, max=max, stdev=s))
}
POLCON_vars <- POLCON[c("POLCONIII", "POLCONV", "POLCONVJ")]
POLCON_desc <- sapply(POLCON_vars, mystats, na.omit=TRUE)
is.matrix(POLCON_desc)
POLCON_desc <- t(POLCON_desc) # Transposing the matrix
POLCON_desc
#install.packages("doBy")                   
library(doBy)
POLCON_desc2 <- summaryBy(POLCONIII~CTRYNM, data=POLCON, FUN=mystats)
                      
## @knitr POLCON_tab
library(xtable)
print(xtable(POLCON_desc, label='POLCON_tab',caption='Descriptive statistics of the variables on political constraints (Henisz 2000)', sanitize.text.function = function(x){x}, table.placement = h)) # Output as LaTeX.
# print(xtable(POLCON_desc2, label='POLCON_tab2',caption='Descriptive statistics of the variables on political constraints (Henisz 2000) on a country level', sanitize.text.function = function(x){x}, table.placement = p), tabular.environment='longtable', floating = FALSE) # Output as LaTeX.
                      
# # Test to see what happens when the option na.omit is set to FALSE
# POLCON_desc2 <- sapply(POLCON_vars, mystats, na.omit=FALSE)
# POLCON_desc2 <- t(POLCON_desc2) # Transposing the matrix
# print(xtable(POLCON_desc2, label='POLCON_tab2',caption='Descriptive statistics of the variables on political constraints (Henisz 2000)', sanitize.text.function = function(x){x}, table.placement = h), digits = 2) # Output as LaTeX.

## @knitr PWT
#### Penn World Table: ki, kg, rgdl ####

## Reading PWT data set
pwt71 <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/pwt71_11302012version/pwt71_wo_country_names_wo_g_vars.csv")
# Dropping irrelevant variables
pwt71 <- pwt71[c("isocode", "year", "rgdpl", "kg", "ki")]
ls(pwt71)

## Create variable with initial gdp value
# Create new variable that is missing if rgdpl is missing and otherwise year to use for function that looks up earliest year that is not missing
pwt71$yearNA <- ifelse(is.na(pwt71$rgdpl), pwt71$yearNA <- NA, pwt71$year)
# Show data
head(pwt71, n=10)
# Here the pwt71 data is split into country groups along with variable year and rgdpl
countries <- split(pwt71[,2:6], pwt71$isocode)
# Show the first country in the list countries
countries[1]
# Create function that finds gdp in earliest year that is not missing
vlookup7 <- function(df){
  df[df[5] == min(df[5], na.rm=TRUE), 2]
}
# Use lapply
inigdp <- lapply(countries, vlookup7)
inigdp
# Remove NAs
inigdp <- lapply(inigdp, function(x) x[!is.na(x)])
inigdp
# Re-assembling the data
pwt71$inigdp <- unsplit(inigdp, pwt71$isocode)
head(pwt71, n=10)
# Deleting the variable yearNA
pwt71$yearNA <- NULL
head(pwt71, n=10)
# Merging Penn World Table and POLCON
IQM_pro_data <- merge(POLCON, pwt71, by.x=c("CTRYNM", "Year"), by.y=c("isocode", "year")) 
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

# Descriptive statistics for kg
kg <- pwt71[c("kg")]
is.numeric(kg)
kg_desc <- sapply(kg, mystats, na.omit=TRUE)
kg_desc <- t(kg_desc) # Transposing the matrix
kg_desc
## @knitr kg
library(xtable)
print(xtable(kg_desc, label='kg_tab',caption='Descriptive statistics of Government Consumption', sanitize.text.function = function(x){x}, table.placement = h)) # Output as LaTeX.

## @knitr educ
#### Barro-Lee: yr.sch.secF, yr.sch.secM ####
educMF <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/BL(2010)_MF1599_v1.2.csv") # Data for total population
educMF <- educMF[c("WBcode", "year", "yr_sch_sec")] # Dropping irrelevant variables
educF <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/BL(2010)_F1599_v1.2.csv") # Data for females
educF <- educF[c("WBcode", "year", "yr_sch_sec")] # Dropping irrelevant variables
# install.packages("reshape")
library(reshape) # Package needed for rename() function
educF <- rename(educF, c(yr_sch_sec="yr.sch.secF")) # Renaming the variable of interest to distinguish between female and total population in merged data set
educ <- merge(educMF, educF, by=c("WBcode", "year")) # Merging education data sets
educ$yr.sch.secM <- (2*educ$yr_sch_sec - educ$yr.sch.secF) # Calculating average for males
educ$yr_sch_sec <- NULL # Dropping variable for total population (only male and female left)
summary(IQM_pro_data)
IQM_pro_data <- merge(IQM_pro_data, educ, by.x=c("CTRYNM", "Year"), by.y=c("WBcode", "year"), all=TRUE) # Merging the two above with Barro-Lee data set. NB: Contains only data for every fifth year, so the data set is drastically reduced.
summary(IQM_pro_data)

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

## @knitr fert
#### Fertility rates: fert ####
fert <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/SP.DYN.TFRT.IN_Indicator_MetaData_en_EXCEL.csv", check.names=FALSE)
fert$"Country Name" <- NULL # Dropping irrelevant variables
fert <- melt(fert, id=c("Country Code")) # Reshaping data
fert <- rename(fert, c(value="fert", variable="year", "Country Code"="country.code")) # Renaming variables
fert$lfert <- log(fert$fert) # Here the variable is log-transformed
IQM_pro_data <- merge(IQM_pro_data, fert, by.x=c("CTRYNM", "Year"), by.y=c("country.code", "year")) # Merging data with main data set
ls(IQM_pro_data)

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

# ## @knitr test-plot
# hist(IQM_pro_data$bmp)

## @knitr bmp_tab
library(xtable)
print(xtable(bmp_desc, label='bmp_tab',caption='Descriptive statistics of the variables on black market premium', sanitize.text.function = function(x){x}, table.placement = h)) # Output as LaTeX.
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
IQM_pro_data <- merge(IQM_pro_data, icrgQoG, by.x=c("CTRYNM", "Year"), by.y=c("ccodewb", "year")) # Merging data with main data set
is.numeric(IQM_pro_data$icrgQoG) # Checking data is numeric
ls(IQM_pro_data)

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
ls(polIV)
# print(xtable(describe(polIV$democ, skew = FALSE)))
# hist(polIV$democ, breaks = 10, col = "red", xlab="Democracy Score", main="Histogram of democ")
ls(IQM_pro_data)
IQM_pro_data <- merge(IQM_pro_data, polIV, by.x=c("CTRYNM", "Year"), by.y=c("scode", "year")) # Merging data with main data set
ls(IQM_pro_data)

## @knitr saving
# Saving data set
write.csv(IQM_pro_data, file = "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/IQM_pro_data.csv", row.names = FALSE) # This is done so that the data set is read in the Lyx file

## @knitr my-label
#### Data description ####
ls()
# Creating data set only with numeric variables for descriptive statistics
ls(IQM_pro_data) # Show variables
IQM_pro_data_vars <- IQM_pro_data
ls(IQM_pro_data_vars) # Show variables
IQM_pro_data_vars$CTRYNM <- NULL # Deleting country name
IQM_pro_data_vars$Year <- NULL # Deleting year
ls(IQM_pro_data_vars)

# ### Statistics with psych package
# # install.packages("psych")
# library(psych) 
# options(scipen=999) # Change scientific notation
# IQM_desc <- describe(IQM_pro_data_vars, ranges = TRUE, na.rm = FALSE, skew = FALSE) # Saves data description into object 
# IQM_desc <- as.data.frame(IQM_desc) # Converting table to matrix 
# # print(IQM_desc, digits = 2)
# IQM_desc$var <- NULL # Deleting irrelevant column
# IQM_desc$X <- NULL # Deleting irrelevant column
# # print(IQM_desc, digits = 2)
# rownames(IQM_desc) <- c("POLCONIII", "POLCONV", "POLCONVJ", "Real Per Capita GDP Growth", "Government Consumption (% GDP)", "Total Investment (% GDP)", "Log(Life Expectancy)", "Log(Fertility Rate)", "Black Market Premium", "Terms of trade", "Log(ICRG Risk Measure)", "Log(Democracy Index (PolityIV))") # Giving new names to variables to table. NB: Gives error, but is correct in LaTeX output...
# # print(IQM_desc, digits = 2)
# library(xtable) 
# # print(xtable(IQM_desc, label='tabsmall',caption='Descriptive statistics of the variables used', digits=2, sanitize.text.function = function(x){x}, table.placement = h), floating.environment='sidewaystable', digits = 2) # Output as LaTeX.

### Trying only to make statistics that I need
mystats <- function(x, na.omit=FALSE){
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
IQM_my_desc <- sapply(IQM_pro_data_vars, mystats, na.omit=TRUE)
is.matrix(IQM_my_desc)
IQM_my_desc <- t(IQM_my_desc) # Transposing the matrix
IQM_my_desc
# rownames(IQM_my_desc) <- c("POLCONIII", "POLCONV", "POLCONVJ","Law and Order from ICRG", "Real Per Capita GDP Growth", "Government Consumption (% GDP)", "Total Investment (% GDP)", "Alternative measure of GDP growth", "Log(Life Expectancy)", "Log(Fertility Rate)", "Black Market Premium", "Terms of trade", "Log(ICRG Risk Measure)", "Democracy Index (PolityIV)") # Giving new names to variables to table.
## @knitr all_var
print(xtable(IQM_my_desc, label='tabsmall',caption='Descriptive statistics of the variables used', digits=2, sanitize.text.function = function(x){x}, table.placement = h), floating.environment='sidewaystable', digits = 2) # Output as LaTeX.

# Trying to do descriptive statistics on country level
# by(IQM_pro_data_vars, IQM_pro_data$CTRYNM, mystats)


## @knitr new-label
#### Converting to Stata data ####
# install.packages("foreign")
library(foreign) # Package needed for the write.dta() function
ls(IQM_pro_data)
write.dta(IQM_pro_data, "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/IQM_pro_data.dta", version=10)
write.csv(IQM_pro_data, "~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/thesis_data.csv")

## @knitr analysis
#### Analysis ####
# install.packages("plm")
library(plm) # Package for panel data model, see Croissant and Millo (2008)
# IQM_pro_data <- pdata.frame(IQM_pro_data, index = c("CTRYNM", "Year"), drop.index = TRUE, row.names = TRUE)

# Pooled regression
summary(pooled1 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + POLCONV, data = IQM_pro_data, model = "pooling"))
summary(pooled2 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + POLCONIII + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "pooling"))
summary(pooled3 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + POLCONVJ + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "pooling"))
summary(pooled4 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + democ + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "pooling"))
summary(pooled5 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + llaw.order, data = IQM_pro_data, model = "pooling"))
summary(pooled6 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + l.icrgQoG, data = IQM_pro_data, model = "pooling"))

# Fixed effects, individual effects
summary(fixed_effects1 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + POLCONV, data = IQM_pro_data, model = "within"))
summary(fixed_effects2 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + POLCONIII + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "within"))
summary(fixed_effects3 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + POLCONVJ + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "within"))
summary(fixed_effects4 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + democ + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "within"))
summary(fixed_effects5 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + llaw.order, data = IQM_pro_data, model = "within"))
summary(fixed_effects6 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + l.icrgQoG, data = IQM_pro_data, model = "within"))

# Fixed effects, twoways
summary(fixed_effects1a <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + POLCONV, data = IQM_pro_data, model = "within", effect = "twoways"))
summary(fixed_effects2a <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + POLCONIII + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "within", effect = "twoways"))
summary(fixed_effects3a <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + POLCONVJ + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "within", effect = "twoways"))
summary(fixed_effects4a <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + democ + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "within", effect = "twoways"))
summary(fixed_effects5a <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + llaw.order, data = IQM_pro_data, model = "within", effect = "twoways"))
summary(fixed_effects6a <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + l.icrgQoG, data = IQM_pro_data, model = "within", effect = "twoways"))

# Fixed effects, time effects
summary(fixed_effects1b <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + POLCONV, data = IQM_pro_data, model = "within", effect = "time"))
summary(fixed_effects2b <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + POLCONIII + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "within", effect = "time"))
summary(fixed_effects3b <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + POLCONVJ + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "within", effect = "time"))
summary(fixed_effects4b <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + democ + lbmp + lfert + kg + ki + llexpec + ToT, data = IQM_pro_data, model = "within", effect = "time"))
summary(fixed_effects5b <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + llaw.order, data = IQM_pro_data, model = "within", effect = "time"))
summary(fixed_effects6b <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + l.icrgQoG, data = IQM_pro_data, model = "within", effect = "time"))

# Random effects
# summary(random_effects1 <- plm(lgdp.growth ~ yr.sch.secF + yr.sch.secM + lbmp + lfert + kg + ki + llexpec + ToT + POLCONV, data = IQM_pro_data, model = "random"))


library(estout)
## @knitr analysis2
eststo(fixed_effects1)
eststo(fixed_effects2)
eststo(fixed_effects3)
eststo(fixed_effects4)
eststo(fixed_effects5)
eststo(fixed_effects6)
esttab(label = "fe3", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from fixed effects estimation (individual effects)", caption.top=FALSE, table.pos="p", texfontsize="\\small")
estclear()
# Fixed effects, twoways
eststo(fixed_effects1a)
eststo(fixed_effects2a)
eststo(fixed_effects3a)
eststo(fixed_effects4a)
eststo(fixed_effects5a)
eststo(fixed_effects6a)
esttab(label = "fe4", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from fixed effects estimation (individual and time effects)", caption.top=FALSE, table.pos="p", texfontsize="\\small")
estclear()
# FE, Time effects
eststo(fixed_effects1b)
eststo(fixed_effects2b)
eststo(fixed_effects3b)
eststo(fixed_effects4b)
eststo(fixed_effects5b)
eststo(fixed_effects6b)
esttab(label = "fe5", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from fixed effects estimation (time effects)", caption.top=FALSE, table.pos="p", texfontsize="\\small")
estclear()

# Pooled
eststo(pooled1)
eststo(pooled2)
eststo(pooled3)
eststo(pooled4)
eststo(pooled5)
eststo(pooled6)
esttab(label = "fe6", colnumber=TRUE, var.rename=NULL, table="sidewaystable", caption = "Estimation results from pooled regression", caption.top=FALSE, table.pos="p", texfontsize="\\small")
estclear()


#### Mis-specification #### 
## @knitr mis
plmtest(pooled1, effect = "twoways", type = "ghm")

## @knitr mis2
plmtest(pooled1, effect = "individual", type="bp")

## @knitr mis3
pwtest(pooled1)

## @knitr mis4
library(lmtest)
pbgtest(fixed_effects1)


## @knitr warnings
warnings()