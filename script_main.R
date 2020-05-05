### Read the data
library(readxl)
fund.data.van <- read_excel("FundData.xls")
fund.data.vir <- read_excel("FundData.xls", sheet = 2)
fund.data.vir$mret <- as.numeric(fund.data.vir$mret)
fund.data.fid <- read_excel("FundData.xls", sheet = 3)
fund.data.fid$mret <- as.numeric(fund.data.fid$mret)

## Plot historical returns
### Vanguard 500 Index Fund Admiral
# attaching packages
library(dygraphs)
library(lubridate)
require(ggplot2)
require(dplyr)
require(plotly)
library(ggplot2)
library(dplyr)
library(plotly)
library(xts)
#converitng data to convenient format
fund.data.van$datetime <- ymd(fund.data.van$caldt)

# selecting data to plot
plot.data.van <- data.frame(fund.data.van$datetime, fund.data.van$mret)
plot.data.van <- xts(x = plot.data.van$fund.data.van.mret, order.by = plot.data.van$fund.data.van.datetime)

# interactive plot
p.van <- dygraph(plot.data.van)

### Virtus KAR Small-Cap Growth Fund
#converitng data to convenient format
fund.data.vir$datetime <- ymd(fund.data.vir$caldt)

# selecting data to plot
plot.data.vir <- data.frame(fund.data.vir$datetime, fund.data.vir$mret)
plot.data.vir <- xts(x = plot.data.vir$fund.data.vir.mret, order.by = plot.data.vir$fund.data.vir.datetime)

# interactive plot
p.vir <- dygraph(plot.data.vir)

### Fidelity Advisor Growth Opps M 
#converitng data to convenient format
fund.data.fid$datetime <- ymd(fund.data.fid$caldt)

# selecting data to plot
plot.data.fid <- data.frame(fund.data.fid$datetime, fund.data.fid$mret)
plot.data.fid <- xts(x = plot.data.fid$fund.data.fid.mret, order.by = plot.data.fid$fund.data.fid.datetime)

# interactive plot
p.fid <- dygraph(plot.data.fid)

## Descriptive statistics
### Vanguard 500 Index Fund Admiral
#install.packages('moments')
library(moments)
#mean
fund.data.van.mean <- mean(fund.data.van$mret)
#standard deviation
fund.data.van.sd <- sd(fund.data.van$mret)
#skewness
fund.data.van.skew<- skewness(fund.data.van$mret)
#kurtosis
fund.data.van.kur <- kurtosis(fund.data.van$mret)
#sharpe ratio
fund.data.van.SR <- (fund.data.van.mean / fund.data.van.sd) * sqrt(12)

### Virtus KAR Small-Cap Growth Fund
#mean

fund.data.vir.mean <- mean(fund.data.vir$mret, na.rm=TRUE)
#standard deviation
fund.data.vir.sd <- sd(fund.data.vir$mret, na.rm=TRUE)
#skewness
fund.data.vir.skew <- skewness(fund.data.vir$mret, na.rm=TRUE)
#kurtosis
fund.data.vir.kur <- kurtosis(fund.data.vir$mret, na.rm=TRUE)
#sharpe ratio
fund.data.vir.SR <- (fund.data.vir.mean / fund.data.vir.sd) * sqrt(12)

### Fidelity Advisor Growth Opps M 
#mean
fund.data.fid.mean <- mean(fund.data.fid$mret, na.rm=TRUE)
#standard deviation
fund.data.fid.sd <- sd(fund.data.fid$mret, na.rm=TRUE)
#skewness
fund.data.fid.skew <- skewness(fund.data.fid$mret, na.rm=TRUE)
#kurtosis
fund.data.fid.kur <- kurtosis(fund.data.fid$mret, na.rm=TRUE)
#sharpe ratiod
fund.data.fid.SR <- (fund.data.fid.mean / fund.data.fid.sd) * sqrt(12)

### Convenient Report
df <- data.frame(c(fund.data.van.mean, fund.data.van.sd, fund.data.van.skew, fund.data.van.kur, fund.data.van.SR), c(fund.data.vir.mean, fund.data.vir.sd, fund.data.vir.skew, fund.data.vir.kur, fund.data.vir.SR), c(fund.data.fid.mean, fund.data.fid.sd, fund.data.fid.skew, fund.data.fid.kur, fund.data.fid.SR))
colnames(df) <- c("VFIAX", "PSGAX", "FAGOX")
rownames(df) <- c("Mean", "SD", "Skewness", "Kurtosis", "Sharpe ratio")
df

hist(fund.data.van$mret, main="VFIAX")
hist(fund.data.vir$mret, main = "PSGAX")
hist(fund.data.fid$mret, main= "FAGOX")

## Fama and French Data
library(readr)
FF3 <- read_csv("F-F_Research_Data_Factors 3.CSV")
FF3 <- FF3 %>% filter(FF3$X1 >= "1990")
FF3 <- FF3 %>% filter(FF3$X1 <= "2020")
FF3 <- FF3[1:360, ]
FF3 <- FF3 %>% rename(Mkt = `Mkt-RF`)

## Regressions 
### Vanguard 500 Index Fund Admiral
# multiply on 100 to make percentage values
y.van <- (fund.data.van$mret*100) - FF3$RF
fund.data.van.model <- lm(y.van ~ Mkt + SMB + HML, data = FF3)
summary(fund.data.van.model)

### Virtus KAR Small-Cap Growth Fund
# multiply on 100 to make percentage values
FF3 <- FF3 %>% filter(FF3$X1 >= "199706")

y.vir <- (fund.data.vir$mret * 100) - FF3$RF
fund.data.vir.model <- lm(y.vir ~ Mkt + SMB + HML, data = FF3)
summary(fund.data.vir.model)

### Fidelity Advisor Growth Opps M 
# multiply on 100 to make percentage values
FF3 <- FF3 %>% filter(FF3$X1 >= "200605")
y.fid <- (fund.data.fid$mret * 100) - FF3$RF
fund.data.fid.model <- lm(y.fid ~ Mkt + SMB + HML, data = FF3)
summary(fund.data.fid.model)

