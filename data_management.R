# Data Management
# In this section we read all the files necessary, 
# check number of complete observations

# POLCON ####
POLCON <- read.dta("Data/Henisz/polcon2012.dta")
ls(POLCON)
# Dropping irrelevant variables
POLCON <- 
  POLCON[c("ctrynm", "year", "polconiii", "polconv", "polconvj", 
           "laworderfromicrg")] 
# Renaming to confirm to previous usage
POLCON <- rename(POLCON, c(ctrynm="CTRYNM", year="Year", polconiii="POLCONIII",
                           polconv="POLCONV", polconvj="POLCONVJ",
                           laworderfromicrg="Law...Order..from.ICRG."))
POLCON$llaw.order <- log(POLCON$Law...Order..from.ICRG.) # make variable
# Checking which variables I have
ls(POLCON)
head(POLCON)
# Checking number of complete observations
sum(complete.cases(POLCON$POLCONIII))
sum(complete.cases(POLCON$POLCONV))
sum(complete.cases(POLCON$POLCONVJ))
sum(complete.cases(POLCON$Law...Order..from.ICRG.))

# QoG ####
icrg_qog <- read.csv("Data/QoG/1358067_qog_tsd_csv_v6apr11.csv", sep=";")
ls(icrg_qog)
head(icrg_qog[icrg_qog$ccodewb == "BEL"])
# Dropping irrelevant variables
icrg_qog <- icrg_qog[
  c("ccodewb", "year", "icrg_qog")]
ls(icrg_qog)
head(icrg_qog)
icrg_qog <- na.omit(icrg_qog)
icrg_qog$l.icrgQoG <- log(icrg_qog$icrg_qog) # Log transformation
head(icrg_qog)
# Merging data with main data set
IQM_pro_data <- merge(POLCON, icrg_qog, 
                      by.x=c("CTRYNM", "Year"), by.y=c("ccodewb", "year"),
                      all = TRUE)
ls(IQM_pro_data)
head(IQM_pro_data)
# Checking number of complete observations
sum(complete.cases(IQM_pro_data$POLCONIII)) # Should have 15737
sum(complete.cases(IQM_pro_data$POLCONV))
sum(complete.cases(IQM_pro_data$POLCONVJ))
sum(complete.cases(IQM_pro_data$Law...Order..from.ICRG.))
sum(complete.cases(IQM_pro_data$icrg_qog))

# POLITY ####
polIV <- read.csv("Data/Polity IV/p4v2011.csv")
ls(polIV)
polIV <- polIV[c("scode", "year", "democ")] # Dropping irrelevant variables
ls(polIV)
# Deleting -66, -77 and -88 observations
polIV <- subset(polIV, democ > -65, select=1:3)
polIV$democ2 <- polIV$democ^2
IQM_pro_data <- merge(IQM_pro_data, polIV, 
                      by.x=c("CTRYNM", "Year"), 
                      by.y=c("scode", "year"),
                      all = TRUE) # Merging data with main data set
ls(IQM_pro_data)
# Checking number of complete observations
sum(complete.cases(IQM_pro_data$POLCONIII)) # Should have 15737
sum(complete.cases(IQM_pro_data$POLCONV))
sum(complete.cases(IQM_pro_data$POLCONVJ))
sum(complete.cases(IQM_pro_data$Law...Order..from.ICRG.))
sum(complete.cases(IQM_pro_data$icrg_qog))
sum(complete.cases(IQM_pro_data$democ)) # Should have 16248

# PWT ####
pwt71 <- read.dta("Data/pwt71.dta")
str(pwt71)
ls(pwt71)
pwt71 <- pwt71[c("isocode", "year", "rgdpl", 
                 "kg", "ki", "rgdpl2", "gdpgrowth")]
pwt71 <- na.omit(pwt71)
# Create variable with initial gdp value
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
IQM_pro_data <- merge(IQM_pro_data, pwt71, 
                      by.x=c("CTRYNM", "Year"), 
                      by.y=c("isocode", "year"),
                      all = TRUE)
ls(IQM_pro_data)
# Checking number of complete observations
sum(complete.cases(IQM_pro_data$POLCONIII)) # Should have 15737
sum(complete.cases(IQM_pro_data$POLCONV))
sum(complete.cases(IQM_pro_data$POLCONVJ))
sum(complete.cases(IQM_pro_data$Law...Order..from.ICRG.))
sum(complete.cases(IQM_pro_data$icrg_qog))
sum(complete.cases(IQM_pro_data$democ)) # Should have 16248
sum(complete.cases(IQM_pro_data$kg)) # Should have 8940
sum(complete.cases(IQM_pro_data$ki)) # Should have 8940
sum(complete.cases(IQM_pro_data$rgdpl)) # Should have 8940
sum(complete.cases(IQM_pro_data$gdpgrowth)) # Should have 8750

# Barro-Lee (educ) ####
educMF <- read.dta("Data/BL2013_MF1599_v1.3.dta_full.dta") 
ls(educMF)
# Dropping irrelevant variables
educMF <- educMF[c("WBcode", "year", "yr_sch_sec_full")] 
# Data for females
educF <- read.dta("Data/BL2013_F1599_v1.3.dta_full.dta") 
# Dropping irrelevant variables
educF <- educF[c("WBcode", "year", "yr_sch_sec_full")] 
head(educF)
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
# Merging the two above with Barro-Lee data set. 
IQM_pro_data <- merge(IQM_pro_data, educ, 
                      by.x=c("CTRYNM", "Year"), 
                      by.y=c("WBcode", "year"), 
                      all=TRUE)
ls(IQM_pro_data)
# Checking number of complete observations
sum(complete.cases(IQM_pro_data$POLCONIII)) # Should have 15737
sum(complete.cases(IQM_pro_data$POLCONV))
sum(complete.cases(IQM_pro_data$POLCONVJ))
sum(complete.cases(IQM_pro_data$Law...Order..from.ICRG.))
sum(complete.cases(IQM_pro_data$icrg_qog))
sum(complete.cases(IQM_pro_data$democ)) # Should have 16248
sum(complete.cases(IQM_pro_data$kg)) # Should have 8940
sum(complete.cases(IQM_pro_data$ki)) # Should have 8940
sum(complete.cases(IQM_pro_data$rgdpl)) # Should have 8940
sum(complete.cases(IQM_pro_data$gdpgrowth)) # Should have 8750
sum(complete.cases(IQM_pro_data$yr.sch.secM)) # Should have 8906
sum(complete.cases(IQM_pro_data$yr.sch.secF)) # Should have 8906

# Life expectancy ####
lexpec <- read.csv("Data/SP.DYN.LE00.IN_Indicator_MetaData_en_EXCEL.csv", check.names=FALSE)
lexpec$"Country Name" <- NULL # Dropping irrelevant variables
lexpec <- melt(lexpec, id=c("Country Code")) # Reshaping data
lexpec <- rename(lexpec, c(value="lexpec", variable="year", "Country Code"="country.code")) # Renaming variables
lexpec$llexpec <- log(lexpec$lexpec) # Here the variable is log-transformed
ls(IQM_pro_data)
IQM_pro_data <- merge(IQM_pro_data, lexpec, 
                      by.x=c("CTRYNM", "Year"), 
                      by.y=c("country.code", "year"),
                      all = TRUE) # Merging data with main data set
ls(IQM_pro_data)
# Checking number of complete observations
sum(complete.cases(IQM_pro_data$POLCONIII)) # Should have 15737
sum(complete.cases(IQM_pro_data$POLCONV))
sum(complete.cases(IQM_pro_data$POLCONVJ))
sum(complete.cases(IQM_pro_data$Law...Order..from.ICRG.))
sum(complete.cases(IQM_pro_data$icrg_qog))
sum(complete.cases(IQM_pro_data$democ)) # Should have 16248
sum(complete.cases(IQM_pro_data$kg)) # Should have 8940
sum(complete.cases(IQM_pro_data$ki)) # Should have 8940
sum(complete.cases(IQM_pro_data$rgdpl)) # Should have 8940
sum(complete.cases(IQM_pro_data$gdpgrowth)) # Should have 8750
sum(complete.cases(IQM_pro_data$yr.sch.secM)) # Should have 8906
sum(complete.cases(IQM_pro_data$yr.sch.secF)) # Should have 8906
sum(complete.cases(IQM_pro_data$llexpec)) # Should have 10949

# Fertility rates: fert ####
fert <- read.csv("Data/SP.DYN.TFRT.IN_Indicator_MetaData_en_EXCEL.csv", check.names=FALSE)
fert$"Country Name" <- NULL # Dropping irrelevant variables
fert <- melt(fert, id=c("Country Code")) # Reshaping data
fert <- rename(fert, c(value="fert", variable="year", "Country Code"="country.code")) # Renaming variables
fert$lfert <- log(fert$fert) # Here the variable is log-transformed
IQM_pro_data <- merge(IQM_pro_data, fert,
                      by.x=c("CTRYNM", "Year"), 
                      by.y=c("country.code", "year"),
                      all = TRUE) # Merging data with main data set
ls(IQM_pro_data)

# Black market premium: bmp ####
bmp <- read.csv("Data/Black_market_premium.csv", check.names=FALSE, 
                na.strings = "..", sep = ";", 
                colClasses=c(rep("character", 2),
                             rep("numeric",40)))
bmp$"Country Name" <- NULL # Dropping irrelevant variables
bmp <- melt(bmp, id=c("Country Code")) # Reshaping data
bmp <- rename(bmp, c(value="bmp", variable="year", "Country Code"="country.code")) # Renaming variables
IQM_pro_data <- merge(IQM_pro_data, bmp, 
                      by.x=c("CTRYNM", "Year"), 
                      by.y=c("country.code", "year"),
                      all = TRUE) # Merging data with main data set

# Terms of trade: ToT ####
ToT <- read.csv("Data/ToT.csv", check.names=FALSE, sep = ";", na.strings = "..")
ToT$"Country Name" <- NULL # Dropping irrelevant variables
ToT <- melt(ToT, id=c("Country Code")) # Reshaping data
ToT <- rename(ToT, c(value="ToT", variable="year", "Country Code"="country.code")) # Renaming variables
IQM_pro_data <- merge(IQM_pro_data, ToT, 
                      by.x=c("CTRYNM", "Year"), 
                      by.y=c("country.code", "year"),
                      all = TRUE) # Merging data with main data set

# World Bank: Dummy variables ####
head(
  WB <- 
    read.csv("Data/World Bank/countries.csv", header=T)
)
WB$Economy <- NULL
WB$Income.group <- NULL
ls(WB)
# Merging data with main data set
head(
  IQM_pro_data <- 
    merge(IQM_pro_data, WB, 
          by.x=c("CTRYNM"), 
          by.y=c("Code"),
          all = TRUE)
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
IQM_pro_data$Region <- NULL
head(IQM_pro_data)
ls(IQM_pro_data)
# Checking number of complete observations
sum(complete.cases(IQM_pro_data$POLCONIII)) # Should have 15737
sum(complete.cases(IQM_pro_data$POLCONV))
sum(complete.cases(IQM_pro_data$POLCONVJ))
sum(complete.cases(IQM_pro_data$Law...Order..from.ICRG.))
sum(complete.cases(IQM_pro_data$icrg_qog))
sum(complete.cases(IQM_pro_data$democ)) # Should have 16248
sum(complete.cases(IQM_pro_data$kg)) # Should have 8940
sum(complete.cases(IQM_pro_data$ki)) # Should have 8940
sum(complete.cases(IQM_pro_data$rgdpl)) # Should have 8940
sum(complete.cases(IQM_pro_data$gdpgrowth)) # Should have 8750
sum(complete.cases(IQM_pro_data$yr.sch.secM)) # Should have 8906
sum(complete.cases(IQM_pro_data$yr.sch.secF)) # Should have 8906
sum(complete.cases(IQM_pro_data$llexpec)) # Should have 10949

# Saving data set ####
ls(IQM_pro_data)
head(IQM_pro_data)
write.csv(IQM_pro_data, file = "Data/IQM_pro_data1.4.csv", row.names = FALSE)
write.dta(IQM_pro_data, file = "Data/IQM_pro_data1.4.dta")
