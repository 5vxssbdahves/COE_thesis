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

#### Forsoeg 7: split, lapply  ####
# This attempt tries to split the data into country groups and then apply a function and then put back the data again. I have succeeded to use this procedure to make a new variable with the lowest number ... can't remember. I have to try to apply the function vlookup to a list object. The attempt is largely modelled on the procedure on p. 178 in Dalgaard (2008)
# Here the pwt71 data is split into country groups along with variable year and rgdpl
countries <- split(pwt71[,2:6], pwt71$isocode)
# Show the first country in the list countries
countries[1]
# Show first observation of column year of first country in the list countries
countries[[1]]$year[1]
# Find value of gdp in 1972
countries[[1]][countries[[1]]$year == 1972,2] # Based on this expression: y[y$year==1972,2]
# Find value of gdp in earliest year that is not missing for the first country
countries[[1]][countries[[1]]$year == min(countries[[1]]$yearNA, na.rm=TRUE),2]  # Based on this expression: y[y$yearNA == min(y$yearNA, na.rm=TRUE),2]
# Try to make column in first country with the above expression
countries[[1]]$inigdp.test1 <-  countries[[1]][countries[[1]]$year == min(countries[[1]]$yearNA, na.rm=TRUE),2]                        
# See result
countries[[1]]
# This actually works. I now have a column with the gdp value in the first year that is not missing for AFG. How can I apply this to the rest of the countries in the list? Maybe by deleting the '1' in countries[[1]]? No, that was not right. Maybe only deleting the '1'? Not working either. Maybe with 'x'? Har også proevet med 'i', men det virker heller ikke. Not that either. Maybe the solution is indeed to attempt to construct the function below. But here I encounter a similar problem. I need to figure out what I replace [1] with... Meanwhile, I continue with trying to figure out how to create the function below.
# Show first and second country
countries[1:2]
# Try to make column in the first two countries with the above expression
countries[1:2]$inigdp.test2 <-  countries[1:2][countries[1:2]$year == min(countries[1:2]$yearNA, na.rm=TRUE),2] # Fail

# Make function vlookup for finding value in specified year. So far not successful...
vlookup5 <- function(val, list, col){list[list[1] == val, col][1]} # Based on this expression: vlookup <- function(val, df, row){df[df[1] == val, row][1]}
vlookup5(1972,countries,2)  # Based on this expression: vlookup(1974,y,2)


# Laver funktion, der finder den mindste vaerdi i soejlen yearNA, der ikke mangler, og returnerer vaerdien fra soejlen rgdpl.
vlookup4 <- function(list, row){
   list[list[5] == min(list[5], na.rm=TRUE), row][2]
 }
# Anvender funktion til at lave nu variabel
min.gdp <- lapply(countries, vlookup4(countries,2))
# Test for at anvende lapply
test <- lapply(countries, function(x) x / x[1])
# Test to put data together
pwt71$test <- unsplit(test, pwt71$isocode)
# See data
head(pwt71, n=150)

# Put data together again
pwt71$min.gdp <- unsplit(min.gdp, pwt71$isocode)
# Confirm we have created new variable with min gdp. Doesn't work completely. The function takes the lowest value in either column, so for the second country the lowest value is the year 1950.
head(pwt71, n=150)
