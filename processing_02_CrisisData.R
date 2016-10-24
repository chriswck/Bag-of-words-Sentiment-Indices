#Processing Crises Data
require(lubridate)
require(tidyr)

#**********Daily Equity Crisis**********
source("processing_WSJFrontPage.R")

equity_crisis <- read.csv("Daily_Equity_Crisis_copied20151014.csv")
#http://stackoverflow.com/questions/8961063/combining-multiple-identically-named-columns-in-r
selected_equity <- equity_crisis[,c(1,5,7)]
selected_equity$Date <- ymd(as.character(equity_crisis$Date))

#**********Writing ntp and ntt files (with dates and countries tidied)**********
#selecting those dates in crisis dummy file that are in country_check and ratio/word_count
temp <- selected_equity[selected_equity$Date %in% mdy(agg_ntp$Date),]
new_equity <- spread(temp, country, crisis, drop=FALSE)

#selecting those countries present in crisis data
selected_countries <- colnames(agg_ntp) %in% colnames(new_equity)
us_ntp <- agg_ntp[, selected_countries]
us_ntt <- agg_ntt[, selected_countries]

write.csv(us_ntp, "WSJ Business & Finance RESULTS/us_bnf_ntp.csv", row.names = FALSE)
write.csv(us_ntt, "WSJ Business & Finance RESULTS/us_bnf_ntt.csv", row.names = FALSE)

#**********Daily Currency Crisis**********
currency_crisis <- read.csv("Daily_Currency_Crisis_copied20151014.csv")
currency_crisis$crisis_2.5sd <- ifelse(with(currency_crisis, exch_mean-exch_sd*2.5) > currency_crisis$exch, 1, 0)
currency_crisis$crisis_3.0sd <- ifelse(with(currency_crisis, exch_mean-exch_sd*3.0) > currency_crisis$exch, 1, 0)
selected_currency <- currency_crisis[,c(5,1,9)]#select which crisis threshold
colnames(selected_currency) <- c("Date", "country", "crisis")
selected_currency$Date <- ymd(as.character(currency_crisis$Date))

#**********Monthly Currency Crisis (with Daily Frequency Data)**********
require(xlsx)
currency_crisis <- read.xlsx2("Monthly_Currency_crisis_DailyFreqData_copied20151014.xls", sheetIndex = 1)
selected_currency <- currency_crisis[,c(1,2,4)]
colnames(selected_currency) <- c("Date", "country", "crisis")
selected_currency$Date <- ymd(as.character(paste0(selected_currency$Date, ".01")))






#####PLAYGROUND

qplot(exch, data=subset(currency_crisis, country=="CANADA"), geom="histogram",
      binwidth=diff(range(subset(currency_crisis, country=="CANADA")$exch, na.rm=TRUE))/500)


