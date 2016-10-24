#Creating sentiment indicator
#@Input: Word count and country count
#@Output: sentiment indicator dataframe

require(lubridate)

#**********Loading Data**********

loading_path <- "WSJAsia Front Page News/"

neg_summary <- data.frame()
for (i in seq(1995,2014)) {
  temp <- read.csv(paste0(loading_path, "n", i, ".csv"))
  temp_t <- setNames(data.frame(t(temp[,-1])), temp[,1])
  neg_summary <- rbind(neg_summary, temp_t)
}

pos_summary <- data.frame()
for (i in seq(1995,2014)) {
  temp <- read.csv(paste0(loading_path, "p", i, ".csv"))
  temp_t <- setNames(data.frame(t(temp[,-1])), temp[,1])
  pos_summary <- rbind(pos_summary, temp_t)
}

country_check <- data.frame()
for (i in seq(1995,2014)) {
  temp <- read.csv(paste0(loading_path, "CountryCheck", i, ".csv"), 1)
  temp_t <- setNames(data.frame(t(temp[,-1])), temp[,1])
  country_check <- rbind(country_check, temp_t)
}

#**********Combining word counts with country counts & tidying dataframe**********

new_summary <- data.frame(row.names(neg_summary), neg_summary$`Total Negative Words`/pos_summary$`Total Positive Words`, neg_summary$Ratio)
colnames(new_summary) <- c("Date", "Negative-to-Positive", "Negative-to-Total")

country_check <- country_check[,1:(ncol(country_check)-2)]
country_check_scaled <- country_check/rowSums(country_check)
new_country_check <- data.frame(row.names(country_check_scaled), country_check_scaled)
colnames(new_country_check)[1] <- "Date"

#**********Fixing Dates (WSJ Front Page)**********
new_country_check$Date <- gsub("^X", "", new_country_check$Date)
#Fixing marker appended to same dates (originally for distinguishing between articles of same day)
odd_dates <- grep("^.*\\..*\\..*\\.[0-9]{1,2}$", new_country_check$Date)
new_country_check$Date[odd_dates] <- gsub("\\.[0-9]{1,2}$", "", new_country_check$Date[odd_dates])
#Fixing year values that only have 2 characters
odd_dates2 <- grep("^[0-9]{1,2}\\.[a-zA-Z]{3,10}\\.[0-9]{2}$", new_country_check$Date)
if (length(odd_dates2)>0 & loading_path == "WSJUS Front Page News/") {
  new_country_check$Date[odd_dates2] <- gsub("[0-9]{2}$", "2004", new_country_check$Date[odd_dates2])
} else if (length(odd_dates2)>0 & loading_path == "WSJAsia Front Page News/") {
  new_country_check$Date[odd_dates2] <- gsub("[0-9]{2}$", "1996", new_country_check$Date[odd_dates2])
}
#Final checkpoint for any remaining invalid date formats
odd_dates3 <- grep("^[0-9]{1,2}\\.[a-zA-Z]{3,10}\\.[0-9]{4}$", new_country_check$Date, invert = TRUE)
#new_country_check$Date[cache]
if (length(odd_dates3)>0 & loading_path != "WSJEu Front Page News/") {
  cached = new_country_check$Date[odd_dates3]; cached2 = strsplit(cached, split="\\.")
  cached3 = lapply(cached2, function(x) paste(x[3], x[2], x[1], sep = ".")); cached4 = do.call(c, cached3)
  new_country_check$Date[odd_dates3] = cached4
  new_country_check$Date <- dmy(new_country_check$Date)
} else if (length(odd_dates3)>0 & loading_path == "WSJEu Front Page News/") {
  cached = new_country_check$Date[odd_dates3]
  new_country_check$Date[odd_dates3] = sapply(new_country_check$Date[odd_dates3], function(x) substr(x, 1, nchar(x)-1))
  new_country_check$Date <- dmy(new_country_check$Date)
}
new_country_check <- new_country_check[order(new_country_check$Date),]

#**********Fixing Dates (WSJ Business & Finance)**********
odd_dates <- grep("^.*\\..*\\..*\\.[0-9]{1,2}$", new_country_check$Date)
new_country_check$Date[odd_dates] <- gsub("\\.[0-9]{1,2}$", "", new_country_check$Date[odd_dates])

#Combining country names (e.g. U.S. and United States), and merging names with those in crisis data
colnames(new_country_check)[grep("BOSNIA", colnames(new_country_check))] <- "BOSNIA"
colnames(new_country_check)[grep("CZECH", colnames(new_country_check))] <- "CZECH"
colnames(new_country_check)[grep("LAO", colnames(new_country_check))] <- "LAOS"
colnames(new_country_check)[grep("THE.NETHERLANDS", colnames(new_country_check))] <- "NETHERLANDS"
colnames(new_country_check)[grep("RUSSIAN", colnames(new_country_check))] <- "RUSSIA"
colnames(new_country_check)[grep("ARAB.EMIRATES", colnames(new_country_check))] <- "UAE"
colnames(new_country_check)[grep("^S.* AFRICA", colnames(new_country_check))] <- "SOUTH.AFRICA"
colnames(new_country_check)[grep("^S.* KOREA", colnames(new_country_check))] <- "SOUTH.KOREA"
colnames(new_country_check)[grep("^N.* KOREA", colnames(new_country_check))] <- "NORTH.KOREA"
colnames(new_country_check)[grep("^U\\.S\\.|^UNITED.STATES$", colnames(new_country_check))] <- "US"
colnames(new_country_check)[grep("^U\\.K\\.|^UNITED.KINGDOM$", colnames(new_country_check))] <- "UK"

#**********Creating sentiment dataframes & aggregating same day articles**********

neg_to_pos <- new_country_check
neg_to_pos[,2:ncol(neg_to_pos)] <- sapply(neg_to_pos[,2:ncol(neg_to_pos)], function(x) x*new_summary$`Negative-to-Positive`)
neg_to_tot <- new_country_check
neg_to_tot[,2:ncol(neg_to_tot)] <- sapply(neg_to_tot[,2:ncol(neg_to_tot)], function(x) x*new_summary$`Negative-to-Total`)

agg_ntp <- aggregate(. ~ Date, data = neg_to_pos, sum, na.rm=TRUE, na.action = na.pass)
agg_ntt <- aggregate(. ~ Date, data = neg_to_tot, sum, na.rm=TRUE, na.action = na.pass)
