# Load required packages
library(quantmod)
library(zoo)

### STEP 1: Define SMI tickers ###
# SMI tickers to fetch data
smi_tickers <- c("NOVN.SW", "ROG.SW", "NESN.SW", "UBSG.SW", "ABB.SW", "ZURN.SW",
                 "ALC.SW", "ADEN.SW", "GIVN.SW", "LHN.SW", "SIKA.SW",
                 "LOGN.SW", "SREN.SW", "SGSN.SW")

### STEP 2: Download Data ###
# Initialize a list to store downloaded data
smi_data <- list()

# Loop through each ticker to download data
for (ticker in smi_tickers) {
  cat("Downloading data for:", ticker, "\n")  # Display progress
  tryCatch({
    # Download adjusted close prices
    stock_data <- getSymbols.yahoo(ticker, from = "2010-01-01", periodicity = "daily", auto.assign = FALSE)[,6]
    
    # Remove missing values from the series
    stock_data <- na.omit(stock_data)
    
    # Save the cleaned data to the list
    smi_data[[ticker]] <- stock_data
  }, error = function(e) {
    # Handle errors and notify
    cat("Error downloading data for:", ticker, "\n")
  })
}

# Combine all downloaded data into a single data frame
smi_data_combined <- do.call(cbind, smi_data)

# Rename columns with tickers
colnames(smi_data_combined) <- names(smi_data)

# Check the first few rows of the combined data
cat("First few rows of combined data:\n")
head(smi_data_combined)

###############################################################################

### STEP 3: Clean Missing Values ###
# Handle missing values using interpolation
cat("Interpolating missing values...\n")
smi_data_cleaned <- smi_data_combined  # Create a copy for cleaning
for (col in colnames(smi_data_cleaned)) {
  smi_data_cleaned[, col] <- na.approx(smi_data_cleaned[, col], na.rm = FALSE)  # Interpolation
}

# Check the first few rows after cleaning
cat("First few rows after cleaning:\n")
head(smi_data_cleaned)

###############################################################################

### STEP 4: Extract and Plot Data for NOVN.SW ###
# Extract data for the specific ticker "NOVN.SW"
novn_data <- smi_data_cleaned[, "NOVN.SW"]

# Plot the time series
cat("Plotting the time series for NOVN.SW...\n")
plot(index(novn_data), novn_data, type = "l", col = "blue", lwd = 2,
     main = "NOVN.SW Adjusted Close Price (Time Series)",
     xlab = "Date", ylab = "Adjusted Close Price (CHF)")
grid()





###############################################################################
###############################################################################
###############################################################################



### Do It Yourself! - MPT ###
# This script downloads data for SMI tickers, cleans it, and plots the time series for a specific ticker.


# Loading required package
#install.packages("quantmod")
library(quantmod)

# Downloading Prices via Yahoo Finance API
data <- NULL
tickers_index <- c("^GSPC", "AGG") # Tickers from Yahoo Finance for S&P 500 (^GSPC) and US Aggregate Bond Index (AGG)

for (Ticker in tickers_index){
  data <- cbind(data,
        getSymbols.yahoo(Ticker, from="2010-01-01", periodicity = "weekly", auto.assign=FALSE)[,6])
}
colnames(data) <- c("Stocks", "Bonds")

# Caluculating continuous returns
log_Returns<-diff(log(data))[-1,]

# Defining risk free rate
risk_free_rate<-0


## a) Calculating the Sharpe-ratios for bonds and stocks
# I) Expected returns
er_bonds <- mean(log_Returns$Bonds)
er_stocks <- mean(log_Returns$Stocks)

# II) Standard deviations
sd_bonds <- sd(log_Returns$Bonds)
sd_stocks <- sd(log_Returns$Stocks)

# III) Sharpe-rations
sr_bonds <- (er_bonds-risk_free_rate)/sd_bonds
sr_stocks <- (er_stocks-risk_free_rate)/sd_stocks


## b) Plotting efficient frontier
# Creating 1000 portfolio weights and calculating the correlation between stocks and bonds returns
x_weights <- seq(from = 0, to = 1, length.out = 1000)
cor_bs <- cor(log_Returns$Bonds, log_Returns$Stocks)

# Creating a data.frame that contains the weights for the two asset and empty columns for the portfolio return, standard deviation and Sharpe-rations
pf <- data.frame(w_bonds = x_weights, w_stocks = 1 - x_weights, er_p=NA, sd_p=NA, sr_p=NA)

# Calculating the expected returns and standard deviations for the 1000 portfolios
for(i in 1:nrow(pf)){
  pf$er_p[i] <- pf$w_bonds[i] * er_bonds + pf$w_stocks[i] * er_stocks # Formula for calculating portfolio returns
  pf$sd_p[i] <- sqrt(pf$w_bonds[i]^2 * sd_bonds^2 + 
                  pf$w_stocks[i]^2 * sd_stocks^2 + 
                    2 * pf$w_bonds[i] * pf$w_stocks[i] * sd_bonds * sd_stocks* cor_bs) # Formula for calculating portfolio standard deviation
}

# Plotting the efficient frontier
plot(x=pf$sd_p, y=pf$er_p, xlab="SD Portfolio", ylab="Expected Return Portfolio") + 
  grid()


## c) Deriving the weightings of the market portfolio, i.e. the one that maximizes the Sharpe-ratio
# Calculating the Sharpe-ratio per portfolio
pf$sr_p <- (pf$er_p-risk_free_rate)/pf$sd_p

# Identifying the index of the market portfolio, i.e. the row number of the Sharpe-ratio-maximizing portfolio
indx_mp <- which.max(pf$sr_p)

# Identifying the weightings of the market portfolio
weightings <- cbind(pf$w_stocks[indx_mp], pf$w_bonds[indx_mp])
colnames(weightings) <- c("Stocks", "Bonds")
pie(weightings, labels = paste(round(weightings*100), "% ", colnames(weightings),sep = ""), main = "Asset allocation of market portfolio")


## d) Extracting the Sharpe-ratio of the market portfolio
sr_mp <- pf$sr_p[indx_mp]
cbind(sr_stocks, sr_bonds, sr_mp)
plot(x=pf$sd_p, y=pf$er_p, xlab="SD Portfolio", ylab="Expected Return Portfolio", main="Efficient frontier") +
  grid()
abline(a=risk_free_rate, b=sr_mp, lty=2, col="red")



###############################################################
### Alternative proceeding using the package fPortfolio
## Package loading, data import
# install.packages("fPortfolio")
# install.packages("quantmod")
library(fPortfolio)
library(quantmod)

# Downloading Prices via Yahoo Finance API
data <- NULL
tickers_index <- c("^GSPC", "AGG") # Tickers from Yahoo Finance for S&P 500 (^GSPC) and US Aggregate Bond Index (AGG)

for (Ticker in tickers_index){
  data <- cbind(data,
        getSymbols.yahoo(Ticker, from="2010-01-01", periodicity = "weekly", auto.assign=FALSE)[,6])
}
colnames(data) <- c("Stocks", "Bonds")

# Calculating log-returns and definition of risk-free rate
log_Returns<-diff(log(data))[-1,]
risk_free_rate<-0

# Efficient Frontier
Spec<-portfolioSpec() # Creating variable for the subsequent portfolio optimization specification
setRiskFreeRate(Spec)<-risk_free_rate # Defining risk-free rate
setTargetRisk(Spec)<-"Sigma" # Defining target risk to be optimized, here Sigma = Standard deviation
setNFrontierPoints(Spec)<-1000  # Defining number of efficient portfolios to be constructed

# Definition of constraints
constraint1<-"maxW[]<=1" # Constraint that the maximum weighting of a single asset is equal or smaller than 100%
constraint2<-"LongOnly" # Constraint that investors can't take short positions
constraint3<-"maxsumW[]=1" # Constraint that the sum of all weightings needs to equal 100% 

# Calculating and plotting efficient frontier and tangency line
pf<-portfolioFrontier(as.timeSeries(log_Returns), spec=Spec, constraints=c(constraint1, constraint2, constraint3))
frontierPlot(pf, return="mean", risk="Sigma")
grid()
tangencyLines(pf,col="blue")

# Alternatively: Shortcut
tailoredFrontierPlot(pf, return="mean", risk="Sigma", sharpeRatio = FALSE, twoAssets = TRUE)

# Deriving tangency portfolio (TP) and corresponding weights of individual assets included
tp<-tangencyPortfolio(as.timeSeries(log_Returns), spec=Spec, constraints=c(constraint1, constraint2, constraint3))
weightsPie(tp)

# Sharpe-Ratio of tangency portfolio
SR_Tangency<-(getPortfolio(tp)$targetReturn[1]-risk_free_rate)/getPortfolio(tp)$targetRisk[1]
names(SR_Tangency)<-"Sharpe-Ratio TP"
SR_Tangency


###############################################################
### Alternative proceeding: In-sample optimization, out-of-sample performance measurement
## Package loading, data import
# install.packages("fPortfolio")
# install.packages("quantmod")
# install.packages("tidyverse")
# install.packages("lubridate")
library(fPortfolio)
library(quantmod)
library(tidyverse)
library(lubridate)

# Downloading Prices via Yahoo Finance API
data <- NULL
tickers_index <- c("^GSPC", "AGG") # Tickers from Yahoo Finance for S&P 500 (^GSPC) and US Aggregate Bond Index (AGG)

for (Ticker in tickers_index){
  data <- cbind(data,
        getSymbols.yahoo(Ticker, from="2010-01-01", periodicity = "weekly", auto.assign=FALSE)[,6])
}
colnames(data) <- c("Stocks", "Bonds")

# Calculating log-returns and definition of risk-free rate
log_Returns<-diff(log(data))[-1,]
risk_free_rate<-0

# Optimization set
data_train <- log_Returns %>% subset(year(index(.)) < 2022) # Training set (before 2022)
data_test <- log_Returns %>% subset(year(index(.)) >= 2022) # Test set (after 2022)

# Efficient Frontier
Spec<-portfolioSpec() # Creating variable for the following portfolio optimization specification
setRiskFreeRate(Spec)<-risk_free_rate # Defining risk-free rate
setTargetRisk(Spec)<-"Sigma" # Defining target risk to be optimized
setNFrontierPoints(Spec)<-1000  # Defining number of efficient portfolios to be constructed

# Definition of constraints
constraint1<-"maxW[]<=1" # Constraint that the maximum weighting of a single asset is equal or smaller than 100%
constraint2<-"LongOnly" # Constraint that investors can't take short positions
constraint3<-"maxsumW[]=1" # Constraint that the sum of all weightings needs to equal 100% 

# Estimation of tangency portfolio
tp<-tangencyPortfolio(as.timeSeries(data_train), spec=Spec, constraints=c(constraint1, constraint2, constraint3))
weights<-getWeights(tp)

# Calculation and plotting of out-of-sample performance (rebalancing every week)
data_test$ret_portfolio <- data_test%*%weights
plot(cumsum(data_test$ret_portfolio), main="Portfolio performance")
plot(cumsum(data_test$Stocks), main="Stocks performance")
plot(cumsum(data_test$Bonds), main="Bonds performance")

# Alternatively:
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
data_test <- log_Returns %>% subset(year(index(.)) >= 2022)
pf_weekly_rebalanced <- Return.portfolio(data_test, weights = weights, rebalance_on = "weeks") # Weekly rebalancing
plot(cumsum(pf_weekly_rebalanced), main="Portfolio performance, weekly rebalancing")

pf_not_rebalanced <- Return.portfolio(data_test, weights = weights, rebalance_on = NA) # No rebalancing
plot(cumsum(pf_not_rebalanced), main="Portfolio performance, no rebalancing")
