## currency converter
## translates any currency value into a specified USD-equivalent (must set year)
## also includes code to correct for purchase power parity (international dollars)

## load libraries
library(yahoofinancer)
library(tidyverse)
library(quantmod)
library(fredr)
library(dplyr)
library(countrycode)

## import raw costs
ihrawcosts <- read.csv("IHcostsRaw.csv", header=T)
head(ihrawcosts)

## import purchasing power parity data from World Bank
PPPdat <- read.table('PPP1990_2023.csv', header=T, sep=',')
head(PPPdat)

## find year-specific PPP and add to ihrawcosts_ISO
ppp_yr <- ifelse(ihrawcosts$applicable_year > 2023, 2023, ihrawcosts$applicable_year)
ppp_cntry <- ifelse(ihrawcosts$ISO3 == "", NA, ihrawcosts$ISO3)
ppp <- rep(NA,dim(ihrawcosts)[1])
for (h in 1:dim(ihrawcosts)[1]) {
  cntry_row <- which(PPPdat$ISO3 == ppp_cntry[h])
  year_col <- which(colnames(PPPdat) == paste('X',ppp_yr[h],sep=""))
  if (length(cntry_row) == 0 || length(year_col) == 0) {
    ppp[h] <- NA
  } else if (is.na(ppp_cntry[h])) {
    ppp[h] <- NA
  } else {
    ppp[h] <- PPPdat[cntry_row, year_col]
  }
}
ppp

ihrawcosts_PPP <- data.frame(ihrawcosts, 'PPP'=ppp)
head(ihrawcosts_PPP)

## identify the currency expected for the applicable country
iso_currency <- na.omit(as.data.frame(codelist %>%
  select(iso3c, iso4217c)))
colnames(iso_currency) <- c("ISO3","currency.code")
head(iso_currency)

ihrawcosts_PPP_IC <- merge(ihrawcosts_PPP, iso_currency, by="ISO3")
head(ihrawcosts_PPP_IC)

## set USD equivalent year
USDyr <- 2023

## work flow
## 2005 EUR cost
##    ↓
## 2005 EUR/USD
##    ↓
## inflate USD to 2023 USD

## use consumer price index to convert past USD to 2023 USD equivalents
## using package fredr (Federal Reserve Bank of St. Louis)
setDefaults(getSymbols.FRED, api.key = "606737389899652f38745672963c02fb")
fredr_set_key("606737389899652f38745672963c02fb")

cpi <- fredr(
    series_id = "CPIAUCSL",
    frequency = "m",
    observation_start = as.Date("1850-01-01"),
    observation_end = as.Date(paste(USDyr,'-12-31',sep=""))
)

get_cpi_closest <- function(target_date, cpi_data = cpi) {
  target_date <- as.Date(target_date)
  cpi_data |>
    slice_min(abs(date - target_date), n = 1)
}

## legacy currency exchange rates
FRF2USD <- fredr(
  series_id = "EXFRUS", # French francs per US$
  frequency = "a",
  observation_start = as.Date("1900-01-01"),
  observation_end = as.Date(paste(USDyr,'-12-31',sep=""))
)
FRF2USD$date

DEM2USD <- fredr(
  series_id = "EXGEUS", # Deutsche marks per US$
  frequency = "a",
  observation_start = as.Date("1900-01-01"),
  observation_end = as.Date(paste(USDyr,'-12-31',sep=""))
)
DEM2USD$date

# storage vectors
USD_orig <- # USD equivalent of original currency in applicable year
ex_rate <- # original currency -> USD exchange rate in applicable year
USD_ppp <- # original currency converted to purchase power-parity international dollars in applicable year
ppp_rate <- # original currency -> USD_ppp converate rate in applicable year
cpi_ratio <- # ratio of consumer price index [CPI] in 2023 / CPI in applicable year
USD23 <- # USD_orig -> USD in 2023 equivalent
USDppp23 <- # USD_ppp -> USD_ppp in 2023 equivalent
  rep(NA,dim(ihrawcosts_PPP_IC)[1])
  
for (i in 1:dim(ihrawcosts_PPP_IC)[1]) {
  curncy <- as.character(ihrawcosts_PPP_IC$local_currency[i]) # this row's currency
  ISO3 <- ihrawcosts_PPP_IC$ISO3[i]
  cost_yr <- ihrawcosts_PPP_IC$applicable_year[i] # applicable year
  if (cost_yr != 2023) {
    cost_date <- as.Date(paste(cost_yr,'-07-31',sep=""))  
  }
  if (cost_yr == 2023) {
    cost_date <- as.Date(paste(cost_yr,'-08-31',sep=""))  
  }
  
  # Caribbean guilder introduced March 2025
  if(ihrawcosts_PPP_IC$currency.code[i] == "XCG" &&
     cost_date <= as.Date("2025-03-31")) {
     ihrawcosts_PPP_IC$currency.code[i] <- "ANG"
  }
  
  # cost already given in international dollars (USD_ppp) in applicable year
  if (curncy == "INT") { 
    USD_orig[i] <- USD_ppp[i] <- ihrawcosts_PPP_IC$cost_orig[i]
    ex_rate[i] <- ppp_rate[i] <- 1 
    cpi_orig <- get_cpi_closest(cost_yr)$value
    cpi_USDyr <- get_cpi_closest(as.Date(paste(USDyr,'-08-01',sep="")))$value
    cpi_ratio[i] <- cpi_USDyr / cpi_orig
    USD23[i] <- USDppp23[i] <- ihrawcosts_PPP_IC$cost_orig[i] * cpi_ratio[i]
  }
  
  # cost already given in USD (USD_orig) in applicable year, but not in USA
  if (curncy == "USD" && ISO3 != "USA") {
    if (ihrawcosts_PPP_IC$currency.code[i] != "USD") {
      USD_orig[i] <- ihrawcosts_PPP_IC$cost_orig[i]
      if (cost_yr < 2023 && ihrawcosts_PPP_IC$currency.code[i] != "ANG") {
        rates <- (currency_converter(from = ihrawcosts_PPP_IC$currency.code[i], 
                                     to = 'USD',
                                     start = cost_date,
                                     end = as.Date(paste(USDyr,'-07-31',sep="")),
                                     interval = '3mo'))[,c(1,7)]
        ex_rate[i] <- rates[1,2] # this is the exchange rate for that country's currency, even though amount provided in USD
        curr_cntry <- USD_orig[i] / ex_rate[i]
        ppp_rate[i] <- ihrawcosts_PPP_IC$PPP[i]
        USD_ppp[i] <- USD_orig[i] / ex_rate[i] / ppp_rate[i]
        cpi_orig <- get_cpi_closest(cost_date)$value
        cpi_USDyr <- get_cpi_closest(as.Date(paste(USDyr,'-08-01',sep="")))$value
        cpi_ratio[i] <- cpi_USDyr / cpi_orig
        USD23[i] <-  USD_orig[i] * cpi_ratio[i]
        USDppp23[i] <- USD_ppp[i] * cpi_ratio[i]
      }
      if (cost_yr >= 2023 || ihrawcosts_PPP_IC$currency.code[i] == "ANG") {
        USD_orig[i] <- USD_ppp[i] <- ihrawcosts_PPP_IC$cost_orig[i]
        ex_rate[i] <- ppp_rate[i] <- 1
        cpi_orig <- get_cpi_closest(cost_date)$value
        cpi_USDyr <- get_cpi_closest(as.Date(paste(USDyr,'-08-01',sep="")))$value
        cpi_ratio[i] <- cpi_USDyr / cpi_orig
        USD23[i] <- USDppp23[i] <- USD_orig[i] * cpi_ratio[i]
      }
    }
    if (ihrawcosts_PPP_IC$currency.code[i] == "USD") {
      USD_orig[i] <- USD_ppp[i] <- ihrawcosts_PPP_IC$cost_orig[i]
      ex_rate[i] <- ppp_rate[i] <- 1
      cpi_orig <- get_cpi_closest(cost_date)$value
      cpi_USDyr <- get_cpi_closest(as.Date(paste(USDyr,'-08-01',sep="")))$value
      cpi_ratio[i] <- cpi_USDyr / cpi_orig
      USD23[i] <- USDppp23[i] <- USD_orig[i] * cpi_ratio[i]
    }
   }
  
  # cost given in USD (USD_orig) in applicable year, and in USA
  if (curncy == "USD" && ISO3 == "USA") {
    USD_orig[i] <- USD_ppp[i] <- ihrawcosts_PPP_IC$cost_orig[i]
    ex_rate[i] <- ppp_rate[i] <- 1
    cpi_orig <- get_cpi_closest(cost_date)$value
    cpi_USDyr <- get_cpi_closest(as.Date(paste(USDyr,'-08-01',sep="")))$value
    cpi_ratio[i] <- cpi_USDyr / cpi_orig
    USD23[i] <- USDppp23[i] <- USD_orig[i] * cpi_ratio[i]
  }
  
  if (curncy == "FRF") {
    ex_rate[i] <- 1/(FRF2USD[which(year(FRF2USD$date) == cost_yr),]$value)
    USD_orig[i] <- ihrawcosts_PPP_IC$cost_orig[i] * ex_rate[i]
    ppp_rate[i] <- ihrawcosts_PPP_IC$PPP[i]
    USD_ppp[i] <- USD_orig[i] / ex_rate[i] / ppp_rate[i]
    cpi_orig <- get_cpi_closest(cost_date)$value
    cpi_USDyr <- get_cpi_closest(as.Date(paste(USDyr,'-08-01',sep="")))$value
    cpi_ratio[i] <- cpi_USDyr / cpi_orig
    USD23[i] <- USD_orig[i] * cpi_ratio[i]
    USDppp23[i] <- USD_ppp[i] * cpi_ratio[i]
  }
  
  if (curncy == "DEM") {
    ex_rate[i] <- 1/(DEM2USD[which(year(DEM2USD$date) == cost_yr),]$value)
    USD_orig[i] <- ihrawcosts_PPP_IC$cost_orig[i] * ex_rate[i]
    ppp_rate[i] <- ihrawcosts_PPP_IC$PPP[i]
    USD_ppp[i] <- USD_orig[i] / ex_rate[i] / ppp_rate[i]
    cpi_orig <- get_cpi_closest(cost_date)$value
    cpi_USDyr <- get_cpi_closest(as.Date(paste(USDyr,'-08-01',sep="")))$value
    cpi_ratio[i] <- cpi_USDyr / cpi_orig
    USD23[i] <- USD_orig[i] * cpi_ratio[i]
    USDppp23[i] <- USD_ppp[i] * cpi_ratio[i]
  }
  
  if (cost_date <= as.Date(paste(USDyr,'-07-31',sep="")) && curncy != "USD" && curncy != "FRF" && curncy != "DEM" && curncy != "INT") {
    rates <- (currency_converter(from = curncy, 
                                 to = 'USD',
                                 start = cost_date,
                                 end = as.Date(paste(USDyr,'-07-31',sep="")),
                                 interval = '3mo'))[,c(1,7)]
    ex_rate[i] <- rates[1,2]
    USD_orig[i] <- ihrawcosts_PPP_IC$cost_orig[i] * ex_rate[i]
    ppp_rate[i] <- ihrawcosts_PPP_IC$PPP[i]
    USD_ppp[i] <- USD_orig[i] / ex_rate[i] / ppp_rate[i]
    cpi_orig <- get_cpi_closest(cost_date)$value
    cpi_USDyr <- get_cpi_closest(as.Date(paste(USDyr,'-08-01',sep="")))$value
    cpi_ratio[i] <- cpi_USDyr / cpi_orig
    USD23[i] <- USD_orig[i] * cpi_ratio[i]
    USDppp23[i] <- USD_ppp[i] * cpi_ratio[i]
  } # end if
  
  if (cost_date >= as.Date(paste(USDyr,'-07-31',sep="")) && curncy != "USD" && curncy != "FRF" && curncy != "DEM" && curncy != "INT") {
    rates <- (currency_converter(from = curncy, 
                                 to = 'USD',
                                 start = '2023-03-01',
                                 end = '2024-03-01',
                                 interval = '1mo'))[,c(1,7)]
    
    ex_rate[i] <- rates[1,2]
    USD_orig[i] <- ihrawcosts_PPP_IC$cost_orig[i] * ex_rate[i]
    ppp_rate[i] <- ihrawcosts_PPP_IC$PPP[i]
    USD_ppp[i] <- USD_orig[i]
    cpi_ratio[i] <- 1
    USD23[i] <- USD_orig[i]
    USDppp23[i] <- USD_ppp[i]
  }
  print(i)
}

ihrawcosts.usd23 <- data.frame('InvaHealth_ID'=ihrawcosts_PPP_IC$InvaHealth_ID,
                               'fromyr'=ihrawcosts_PPP_IC$applicable_year,
                               'fromCurr'=ihrawcosts_PPP_IC$local_currency, 
                               'toCurr'=rep('USD23', dim(ihrawcosts_PPP_IC)[1]),
                               'costOrig'=ihrawcosts_PPP_IC$cost_orig,
                               'origCurr2USD_ex_rate'=ex_rate,
                               'USDOrig'=USD_orig,
                               'PPP'=ppp_rate,
                               'USDppp'=USD_ppp,
                               'cpiRatio'=cpi_ratio,
                               'USD23'=USD23,
                               'USD23ppp'=USDppp23)
head(ihrawcosts.usd23)

## export
write.table(ihrawcosts.usd23, file="ihrawcosts_usd23.csv", sep=",", col.names=T, row.names=F)

## merge with full InvaHealth_ID list (to maintain same order as online database)
full_IH_ID_list <- read.csv("full_InvHealth_ID_list.csv", header=T)
tail(full_IH_ID_list)
ihrawcosts_usd23_out <- read.csv("ihrawcosts_usd23.csv", header=T)
tail(ihrawcosts_usd23_out)
ihrawcosts.usd23_full <- merge(full_IH_ID_list, ihrawcosts_usd23_out, by='InvaHealth_ID', all.x=T)
head(ihrawcosts.usd23_full)
tail(ihrawcosts.usd23_full)
write.table(ihrawcosts.usd23_full, file="ihrawcosts_usd23_full2.csv", sep=",", col.names=T, row.names=F)
