# Descriptive statistics for alternative gdp measure
POLCON_desc2 <- summaryBy(POLCONIII~CTRYNM, data=gdp.growth, FUN=mean, na.rm = TRUE)
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