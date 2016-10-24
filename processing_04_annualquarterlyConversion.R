require(lubridate); require(tidyr)

#Input Parameters
setwd("C:/Users/EF-CB01/Google Drive/CityU/Yan, Isabel/Research/Crises Prediction Project/News Articles Sentiment Indicators/Python_Chris_20151014")
loading_path <- "WSJAsia Front Page News"
desired_freq <- "quarter" #Select from "annual" or "quarter"

#**********Loading Data**********
neg_summary <- data.frame()
for (i in seq(1995,2014)) {
  temp <- read.csv(paste0(loading_path, "/n", i, ".csv"))
  temp_t <- setNames(data.frame(t(temp[,-1])), temp[,1])
  neg_summary <- rbind(neg_summary, temp_t)
}

pos_summary <- data.frame()
for (i in seq(1995,2014)) {
  temp <- read.csv(paste0(loading_path, "/p", i, ".csv"))
  temp_t <- setNames(data.frame(t(temp[,-1])), temp[,1])
  pos_summary <- rbind(pos_summary, temp_t)
}

country_check <- data.frame()
for (i in seq(1995,2014)) {
  temp <- read.csv(paste0(loading_path, "/CountryCheck", i, ".csv"), 1)
  temp_t <- setNames(data.frame(t(temp[,-1])), temp[,1])
  country_check <- rbind(country_check, temp_t)
}

#**********Prepping**********

new_summary <- data.frame(row.names(neg_summary), neg_summary$`Total Negative Words`, pos_summary$`Total Positive Words`, neg_summary$`Total Words/Article`)
colnames(new_summary) <- c("Date", "NegWords", "PosWords", "TotWords")

new_country_check <- country_check[,1:(ncol(country_check)-2)]
new_country_check <- data.frame(row.names(new_country_check), new_country_check)
colnames(new_country_check)[1] <- "Date"

#**********Fixing Dates (WSJ Front Page)**********
DateFixing <- function(new_country_check) {
  
  #Remove leading "x"
  new_country_check$Date <- gsub("^X", "", new_country_check$Date)
  
  #Fixing marker appended to same dates (originally for distinguishing between articles of same day)
  odd_dates <- grep("^.*\\..*\\..*\\.[0-9]{1,2}$", new_country_check$Date)
  new_country_check$Date[odd_dates] <- gsub("\\.[0-9]{1,2}$", "", new_country_check$Date[odd_dates])
  
  #Fixing year values that only have 2 characters
  odd_dates2 <- grep("^[0-9]{1,2}\\.[a-zA-Z]{3,10}\\.[0-9]{2}$", new_country_check$Date)
  if (length(odd_dates2)>0) {
    if (loading_path == "WSJUS Front Page News") {
      #####Follow fix only applies SPECIFICALLY depending on WSJ version#####(Currently set to US)
      new_country_check$Date[odd_dates2] <- gsub("[04]{2}$", "2004", new_country_check$Date[odd_dates2])
    } else if (loading_path == "WSJAsia Front Page News") {
      new_country_check$Date[odd_dates2] <- gsub("[96]{2}$", "1996", new_country_check$Date[odd_dates2])
    }
  }
  
  #Final checkpoint for any remaining invalid date formats
  cache <- grep("^[0-9]{1,2}\\.[a-zA-Z]{3,10}\\.[0-9]{4}$", new_country_check$Date, invert = TRUE)
  if (length(cache)>0) {
    if (loading_path == "WSJEu Front Page News") {
      new_country_check$Date[cache] <- gsub("1$", "", new_country_check$Date[cache])
      new_country_check$Date <- dmy(new_country_check$Date)
    } else {
      starting <- head(sort(cache))[1]
      ending <- head(sort(cache, decreasing = TRUE))[1]
      new_country_check$Date <- c(dmy(new_country_check$Date[1:(starting-1)]), ymd(new_country_check$Date[starting:ending]))
    }
  } else {
    new_country_check$Date <- dmy(new_country_check$Date)
  }
  
  #Sort new_country_check by Date
  new_country_check <- new_country_check[order(new_country_check$Date),]
  
  return(new_country_check)
}

new_country_check <- DateFixing(new_country_check)
new_summary <- DateFixing(new_summary)
?ddply
#Combine different names of the same country
long_df <- gather(new_country_check, Country, Count, -Date)
long_df$Country <- as.character(long_df$Country)
long_df$Country[grep("BOSNIA", long_df$Country)] <- "BOSNIA"
long_df$Country[grep("CZECH", long_df$Country)] <- "CZECH"
long_df$Country[grep("LAO", long_df$Country)] <- "LAOS"
long_df$Country[grep("THE.NETHERLANDS", long_df$Country)] <- "NETHERLANDS"
long_df$Country[grep("RUSSIAN", long_df$Country)] <- "RUSSIA"
long_df$Country[grep("ARAB.EMIRATES", long_df$Country)] <- "UAE"
long_df$Country[grep("^S.*AFRICA", long_df$Country)] <- "SOUTH.AFRICA"
long_df$Country[grep("^S.*KOREA", long_df$Country)] <- "SOUTH.KOREA"
long_df$Country[grep("^N.*KOREA", long_df$Country)] <- "NORTH.KOREA"
long_df$Country[grep("^U\\.S\\.|^UNITED.STATES$", long_df$Country)] <- "US"
long_df$Country[grep("^U\\.K\\.|^UNITED.KINGDOM$", long_df$Country)] <- "UK"
new_country_check_long <- long_df
rm(long_df)

new_summary <- DateFixing(new_summary)

#**********Aggregation by desired_freq**********
AggTools <- function(foo_df, desired_freq) {
  #Input: Requires foo_df to have a POSIX column named "Date"
  if (desired_freq == "annual") {
    foo_df$Year <- year(foo_df$Date)
  } else if (desired_freq == "quarter") {
    foo_df$Quarter <- paste0(year(foo_df$Date), "0", quarter(foo_df$Date))
  }
  return(foo_df)
}

if (desired_freq == "annual") {
  new_summary <- AggTools(new_summary, desired_freq)
  new_country_check_long <- AggTools(new_country_check_long, desired_freq)
  agg_words_df <- aggregate(cbind(NegWords, PosWords, TotWords) ~ Year, data = new_summary, FUN = sum, na.action = na.omit)
  agg_country_df <- aggregate(Count ~ Country + Year, data = new_country_check_long[-1], FUN = sum, na.action = na.omit)
  agg_country_df <- spread(agg_country_df, Country, Count)
  denom <- rowSums(agg_country_df[2:ncol(agg_country_df)])
  rdy_country_df <- data.frame(agg_country_df[1], agg_country_df[2:ncol(agg_country_df)]/denom)
} else if (desired_freq == "quarter") {
  new_summary <- AggTools(new_summary, desired_freq)
  new_country_check_long <- AggTools(new_country_check_long, desired_freq)
  agg_words_df <- aggregate(cbind(NegWords, PosWords, TotWords) ~ Quarter, data = new_summary, FUN = sum, na.action = na.omit)
  agg_country_df <- aggregate(Count ~ Country + Quarter, data = new_country_check_long[-1], FUN = sum, na.action = na.omit)
  agg_country_df <- spread(agg_country_df, Country, Count)
  denom <- rowSums(agg_country_df[2:ncol(agg_country_df)])
  rdy_country_df <- data.frame(agg_country_df[1], agg_country_df[2:ncol(agg_country_df)]/denom)
}

agg_words_df$NtP <- with(agg_words_df, NegWords/PosWords)
agg_words_df$NtT <- with(agg_words_df, NegWords/TotWords)

#The following multiplies the word count ratios by the country count ratios
neg_to_pos <- rdy_country_df
neg_to_pos[,2:ncol(neg_to_pos)] <- sapply(neg_to_pos[,2:ncol(neg_to_pos)], function(x) x*agg_words_df$NtP)
neg_to_tot <- rdy_country_df
neg_to_tot[,2:ncol(neg_to_tot)] <- sapply(neg_to_tot[,2:ncol(neg_to_tot)], function(x) x*agg_words_df$NtT)

#Writes output as .csv
write.csv(neg_to_pos, paste0(loading_path, "_ntp_", desired_freq, ".csv"), row.names=FALSE)
write.csv(neg_to_tot, paste0(loading_path, "_ntt_", desired_freq, ".csv"), row.names=FALSE)
