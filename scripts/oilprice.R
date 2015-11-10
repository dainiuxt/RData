###############################################################################
##
## Working with Quandl
##
###############################################################################
library(Quandl)
library(quantmod)
Quandl.auth("9FgxcyZLUuPWmU3Ak6LA")
assignInNamespace('Quandl.host', 'http://www.quandl.com/api', 'Quandl')
options(RCurlOptions = list(proxy = "gl.pl", proxyport = 80))

Quandl.search(query = "petroleum", silent = FALSE)

# Load the Facebook data with the help of Quandl
Facebook <- Quandl("GOOG/NASDAQ_FB", type = "xts", start_date="2014-01-01")
# Plot the chart with the help of candleChart()
candleChart(Facebook)
##Brent crude price up to date
rbrte = Quandl("DOE/RBRTE", start_date="2000-01-01", type = "xts")
futureb1 = Quandl("OFDP/FUTURE_B1", type = "xts")

candleChart(rbrte)
plot(rbrte)
candleChart(futureb1)
plot(futureb1)
