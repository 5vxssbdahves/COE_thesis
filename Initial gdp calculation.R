## This piece of code tries to create a new variable, which takes the minimum of rgdpl for each country in the earliest year that is not missing
# Reading PWT data set
pwt71 <- read.csv("~/Dropbox/Studieophold/College_of_Europe/Master_Thesis/Data/pwt71_11302012version/pwt71_wo_country_names_wo_g_vars.csv")
# Keeping only relevant variables
pwt71 <- pwt71[c("isocode", "year", "rgdpl", "kg", "ki")]
# Show data
head(pwt71, n=10)

# Create new variable that is missing if rgdpl is missing and otherwise year to use for function that looks up earliest year that is not missing
pwt71$yearNA <- ifelse(is.na(pwt71$rgdpl), pwt71$yearNA <- NA, pwt71$year)
# Show data
head(pwt71, n=10)

#### Forsoeg 5 ####
# vlookup3 <- function(df, row){
#   df[df[6] == min(df[6], na.rm=TRUE), row][3]
#   }
# by(pwt71, pwt71$isocode, vlookup3)

#### Forsoeg 6 ####
# # Create data without missing values. NB Not used further
# pwt71.na.omit <- na.omit(pwt71)
# # show data
# head(pwt71.na.omit, n=10)

#### Forsoeg 7 ####
# Here the pwt71 data is split into country groups along with variable year and rgdpl
countries <- split(pwt71[,2:6], pwt71$isocode)
# Show first country
countries[1]
# ## Follow procedure on p. 178 in Dalgaard (2008) # Virker ikke
# # Laver funktion
# # minstats <- function(x, na.omit=TRUE)(c(min=min(x, na.rm=TRUE))) # Denne funktion virker ikke
# vlookup1 <- function(df, row){
#   df[df[5] == min(df[5], na.rm=TRUE), row][2]
# }
# # Anvender funktion til at lave nu variabel
# min.gdp <- lapply(countries, vlookup1(countries,2))
# # See data
# countries[1]
# # Put data together again
# pwt71$min.gdp <- unsplit(min.gdp, pwt71$isocode)
# # Confirm we have created new variable with min gdp. Doesn't work completely. The function takes the lowest value in either column, so for the second country the lowest value is the year 1950.
# head(pwt71, n=150)
# 
# # Tjekker resultat
# countries[1]
# 
# # Find rgdpl value of earliest year of AFG
# pwt71[pwt71$year==min(pwt71$year) & pwt71$isocode=="AFG",3]
# 
# # Trying to find rgdl value of earliest of AFG that is not NA
# pwt71[pwt71$year==min(pwt71$year) & pwt71$isocode=="AFG" & pwt71$rgdpl>0 ,3]
# pwt71[pwt71$year==min(pwt71$year) & pwt71$isocode=="AFG" & isTRUE(is.na(pwt71$rgdpl)),3]
# 
# isTRUE(is.na(pwt71[pwt71$year==min(pwt71$year) & pwt71$isocode=="AFG" & pwt71$rgdpl>0 ,3]))


# Calculating variable with initial gdp for every country. NB Not successful
# minstats <- function(x, na.omit=TRUE)(c(min=min(x, na.rm=TRUE))) # Making function that takes minimum
# 
# pwt71$ini.gdp <- by(pwt71$rgdpl, pwt71$isocode, minstats, simplify = FALSE) # Making variable
# head(pwt71, n=3)
# 
# ini.gdp <- by(pwt71$rgdpl, pwt71$isocode, minstats, simplify = FALSE) # Making variable
# head(ini.gdp, n=3)
# 
# countries3 <- 
# 
# countries2 <- lapply(countries, minstats)
# countries2[1:3]
# pwt71$min.rgdpl <- unsplit(countries2, pwt71$isocode)
# head(pwt71, n=200)
# is.numeric(pwt71$min.rgdpl)
# 
# # pwt71$ini.gdp2 <- ave(pwt71$rgdpl, pwt71$isocode, minstats) # Doesn't work
# 
# AFG <- pwt71[2:3][pwt71$isocode == "AFG"]

# Change