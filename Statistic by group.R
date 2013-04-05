## Create summary statistics by country
# Here the data is split into country groups along with variable year and rgdpl
CTRYNM <- split(IQM_pro_data, IQM_pro_data$CTRYNM)
# Show the first country in the list countries
CTRYNM
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