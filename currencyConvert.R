## currency converter
## translates any currency value into a specified USD-equivalent (must set year)

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

simcosts.usd23 <- data.frame('fromCurr'=simcosts$local_currencies, 
           'toCurr'=rep('USD23', dim(simcosts)[1]),
           'costOrig'=simcosts$list_cost,
           'costConv'=USD23.conv)
head(simcosts.usd23)
