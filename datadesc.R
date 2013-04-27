# Data Desscription
ls(IQM_pro_data)
datadesc2.0 <- ddply(IQM_pro_data,~CTRYNM,summarise,
                     mean=mean(POLCONV,na.rm = TRUE), 
                     min.yr=min(Year), 
                     max.yr=max(Year))
datadesc2.0


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