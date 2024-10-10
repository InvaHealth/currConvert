## currency converter
## translates any currency value into a specified USD-equivalent (must set year)
## also includes code to correct for purchase power parity

install.packages('yahoofinancer')
library(yahoofinancer)

# create dummy database
sim_count <- 100

simcosts <- tibble(
  cost_date = sample(
    seq(as.Date('2000/06/30'), 
        as.Date('2024/07/01'), 
        by = "year"), 
    replace = TRUE, sim_count) %>% 
    sort(),
  local_currencies = sample(
    c("CAD", "JPY", "GBP"), 
    replace = TRUE, sim_count),
  list_cost = abs(rnorm(sim_count, 100000, 5000))
)

## set USD equivalent year
USDyr <- 2023

ex.rate <- USD23.conv <- rep(NA,dim(simcosts)[1])
for (i in 1:dim(simcosts)[1]) {
  curncy <- as.character(simcosts$local_currencies[i])
  cost.date <- as.Date(simcosts$cost_date[i])
  
  if (cost.date <= as.Date(paste(USDyr,'-07-31',sep=""))) {
    rates <- (currency_converter(from = curncy, 
                                 to = 'USD',
                                 start = cost.date,
                                 end = as.Date(paste(USDyr,'-07-31',sep="")),
                                 interval = '3mo'))[,c(1,7)]

    ex.rate[i] <- rates[dim(rates)[1],2]
    USD23.conv[i] <- simcosts$list_cost[i] * ex.rate[i]
  } # end if
  
  if (cost.date > as.Date(paste(USDyr,'-07-31',sep=""))) {
    rates <- (currency_converter(from = curncy, 
                                to = 'USD',
                                start = '2023-03-01',
                                end = '2023-06-01',
                                interval = '1mo'))[,c(1,7)]
    
    ex.rate[i] <- rates[dim(rates)[1],2]
    USD23.conv[i] <- simcosts$list_cost[i] * ex.rate[i]
  }
  print(i)
}

simcosts.usd23 <- data.frame('fromyr'=year(simcosts$cost_date),
           'fromCurr'=simcosts$local_currencies, 
           'toCurr'=rep('USD23', dim(simcosts)[1]),
           'costOrig'=simcosts$list_cost,
           'exchRate'=ex.rate,
           'costConv'=USD23.conv)
head(simcosts.usd23)



## Correct for purchase power parity (PPP)
## data from: https://data.worldbank.org/indicator/PA.NUS.PPP?end=2017&start=1990
## 1990-2023

PPPdat <- read.table('PPP1990_2023.csv', header=T, sep=',')
head(PPPdat)

## add country codes to simulated dataset from above
head(simcosts.usd23)
simcosts.usd23$ISO3 <- ifelse(simcosts.usd23$fromCurr == 'CAD', 'CAN', NA)
simcosts.usd23$ISO3 <- ifelse(simcosts.usd23$fromCurr == 'JPY', 'JPN', simcosts.usd23$ISO3)
simcosts.usd23$ISO3 <- ifelse(simcosts.usd23$fromCurr == 'GBP', 'GBR', simcosts.usd23$ISO3)
head(simcosts.usd23)

PPPval <- rep(NA,dim(simcosts.usd23)[1])
for (c in 1:dim(simcosts.usd23)[1]) {
  pppyr <- ifelse(simcosts.usd23$fromyr[c] == 2024, 2023, simcosts.usd23$fromyr[c])
  pppcntry <- simcosts.usd23$ISO3[c]
  
  PPProw <- which(PPPdat$ISO3 == pppcntry)
    PPProw <- ifelse(length(PPProw)==0, which(PPPdat$ISO3=="USA"), PPProw)
  PPPcol <- which(colnames(PPPdat) == paste('X',pppyr,sep=""))
    PPPcol <- ifelse(length(PPPcol)==0, dim(PPPdat)[2], PPPcol)
  PPPval[c] <- PPPdat[PPProw, PPPcol]
}
PPPval[is.na(PPPval) == T] <- 1
simcosts.usd23$PPP <- PPPval
head(simcosts.usd23)

simcosts.usd23$costConvPPP <- simcosts.usd23$costConv*(simcosts.usd23$PPP/(1/simcosts.usd23$exchRate))
head(simcosts.usd23)
