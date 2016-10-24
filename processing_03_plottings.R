#Plotting graphs

#https://learnr.wordpress.com/2009/05/18/ggplot2-three-variable-time-series-panel-chart/
#http://www.sixhat.net/how-to-plot-multpile-data-series-with-ggplot.html
require(lubridate); require(tidyr); require(ggplot2); require(grid); require(gridExtra)
require(reshape2); require(gtable); require(scales); require(stringr)

#**********Loading files, (finding common dates)******** --------------
loading_bf <- "WSJ Business & Finance RESULTS/"
loading_fp <- "WSJ Front Page News RESULTS/"

us_bf_ntt <- read.csv(paste0(loading_bf, "us", "_bnf", "_ntt.csv"))
us_bf_ntp <- read.csv(paste0(loading_bf, "us", "_bnf", "_ntp.csv"))
eu_bf_ntt <- read.csv(paste0(loading_bf, "eu", "_bnf", "_ntt.csv"))
eu_bf_ntp <- read.csv(paste0(loading_bf, "eu", "_bnf", "_ntp.csv"))
asia_bf_ntt <- read.csv(paste0(loading_bf, "asia", "_bnf", "_ntt.csv"))
asia_bf_ntp <- read.csv(paste0(loading_bf, "asia", "_bnf", "_ntp.csv"))

us_bf_ntt$Date <- mdy(us_bf_ntt$Date)
us_bf_ntp$Date <- mdy(us_bf_ntp$Date)
eu_bf_ntt$Date <- mdy(eu_bf_ntt$Date)
eu_bf_ntp$Date <- mdy(eu_bf_ntp$Date)
asia_bf_ntt$Date <- mdy(asia_bf_ntt$Date)
asia_bf_ntp$Date <- mdy(asia_bf_ntp$Date)

us_fp_ntt <- read.csv(paste0(loading_fp, "us", "_ntt.csv"))
us_fp_ntp <- read.csv(paste0(loading_fp, "us", "_ntp.csv"))
eu_fp_ntt <- read.csv(paste0(loading_fp, "eu", "_ntt.csv"))
eu_fp_ntp <- read.csv(paste0(loading_fp, "eu", "_ntp.csv"))
asia_fp_ntt <- read.csv(paste0(loading_fp, "asia", "_ntt.csv"))
asia_fp_ntp <- read.csv(paste0(loading_fp, "asia", "_ntp.csv"))

asia_fp_ntt$Date <- gsub(" 08:00:00$", "", asia_fp_ntt$Date)
asia_fp_ntp$Date <- gsub(" 08:00:00$", "", asia_fp_ntp$Date)

us_fp_ntt$Date <- ymd(us_fp_ntt$Date)
us_fp_ntp$Date <- ymd(us_fp_ntp$Date)
eu_fp_ntt$Date <- ymd(eu_fp_ntt$Date)
eu_fp_ntp$Date <- ymd(eu_fp_ntp$Date)
asia_fp_ntt$Date <- ymd(asia_fp_ntt$Date)
asia_fp_ntp$Date <- ymd(asia_fp_ntp$Date)

#fp_dates <- intersect(us_fp_ntp$Date, intersect(eu_fp_ntp$Date, asia_fp_ntp$Date))
#bf_dates <- intersect(us_bf_ntp$Date, intersect(eu_bf_ntp$Date, asia_bf_ntp$Date))
#my_dates <- intersect(as.character(ymd(fp_dates)), as.character(mdy(bf_dates)))
#range(ymd(my_dates))

#us_fp_ntt_rdy <- us_fp_ntt[us_fp_ntt$Date %in% my_dates,]
#us_fp_ntp_rdy <- us_fp_ntp[us_fp_ntp$Date %in% my_dates,]
#eu_fp_ntt_rdy <- eu_fp_ntt[eu_fp_ntt$Date %in% my_dates,]
#eu_fp_ntp_rdy <- eu_fp_ntp[eu_fp_ntp$Date %in% my_dates,]
#asia_fp_ntt_rdy <- asia_fp_ntt[asia_fp_ntt$Date %in% my_dates,]
#asia_fp_ntp_rdy <- asia_fp_ntp[asia_fp_ntp$Date %in% my_dates,]

#us_bf_ntt_rdy <- us_bf_ntt[mdy(us_bf_ntt$Date) %in% ymd(my_dates),]
#us_bf_ntp_rdy <- us_bf_ntp[mdy(us_bf_ntp$Date) %in% ymd(my_dates),]
#eu_bf_ntt_rdy <- eu_bf_ntt[mdy(eu_bf_ntt$Date) %in% ymd(my_dates),]
#eu_bf_ntp_rdy <- eu_bf_ntp[mdy(eu_bf_ntp$Date) %in% ymd(my_dates),]
#asia_bf_ntt_rdy <- asia_bf_ntt[mdy(asia_bf_ntt$Date) %in% ymd(my_dates),]
#asia_bf_ntp_rdy <- asia_bf_ntp[mdy(asia_bf_ntp$Date) %in% ymd(my_dates),]


#**********Country names, Crisis Data********** ----------------

countries <- colnames(us_fp_ntp)[-1]

#Equity Crisis
equity_crisis <- read.csv("Daily_Equity_Crisis_copied20151014.csv")
#http://stackoverflow.com/questions/8961063/combining-multiple-identically-named-columns-in-r
my_equity <- equity_crisis[,c(1,5,7)]
my_equity$Date <- ymd(as.character(equity_crisis$Date))
my_equity <- spread(my_equity, country, crisis, drop = FALSE)

#Currency Crisis (EMP)
currency_crisis <- read.csv("EMP 3.0sd Currency Crisis.csv")
my_currency <- currency_crisis
my_currency$Date <- ymd(as.character(my_currency$Date))

#Banking Crisis
banking <- read.csv("Banking Crisis to Shuo.csv")
banking$country <- gsub("_", ".", banking$country)
banking$Date <- paste(substr(banking$monthid,1,4),substr(banking$monthid,5,6), "01", sep = ".")
banking$Date <- ymd(banking$Date)
banking$Date.count <- banking$Date
banking$Date.count[is.na(banking$banking_crisis)] <- NA
banking$Date.count[banking$banking_crisis==0] <- NA
#qplot(x = Date.count, y = country, data=banking, geom="point")
my_banking <- banking[, c(1,4,5)]
my_banking <- spread(my_banking, country, banking_crisis, drop = FALSE)



#**********Creating plots (with separate sentiments: US, Eu & Asia)********** -----------

#http://stackoverflow.com/questions/20901715/ggplot2-facet-grid-group-variables

for (i in countries) {
  prep_df <- data.frame(new_equity_rdy$Date, new_equity_rdy[,i], new_currency_rdy[,i], new_banking_rdy[,i], us_fp_ntp_rdy[,i], eu_fp_ntp_rdy[,i], asia_fp_ntp_rdy[,i], us_fp_ntt_rdy[,i], eu_fp_ntt_rdy[,i], asia_fp_ntt_rdy[,i])
  colnames(prep_df) <- c("Date", "Equity Crisis", "Currency Crisis","US FtPg NtP", "EU FtPg NtP", "Asia FtPg NtP", "US FtPg NtT", "EU FtPg NtT", "Asia FtPg NtT")
  prep_df$Date <- as.Date(prep_df$Date)
  long_prep_df <- gather(prep_df, Key, Values, -Date)
  my_df <- long_prep_df
  my_df$Key2 <- as.character(my_df$Key)
  my_df$Key2[my_df$Key2 %in% c("US FtPg NtP", "EU FtPg NtP", "Asia FtPg NtP")] <- "Neg-to-Pos"
  my_df$Key2[my_df$Key2 %in% c("US FtPg NtT", "EU FtPg NtT", "Asia FtPg NtT")] <- "Neg-to-Tot"
  my_df$Key2[my_df$Key2 %in% c("Equity Crisis", "Currency Crisis", "Banking Crisis")] <- "Crisis"
  my_df[my_df == Inf] <- 0

  #Now begins the plotting, adjust location for saving and plot sizes in the following line
  #png(filename = paste0("Plots/20151020_editCombinedSentiments/", i, ".png"), width = 600, height = 600)
  plot_top <- ggplot() + scale_x_date(breaks = date_breaks("1 year")) + #scale_y_discrete(breaks=c(-0.1,0.0,1.0,1.1), labels=c(NULL,0.0,1.0,NULL)) +
    geom_segment(data=subset(my_df, Key2=="Crisis"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) + theme_bw() +
    scale_colour_manual(values = c("gray20", "orange1")) + labs(list(title = i, x = "", y = "", colour="Crisis Dummy")) +
    theme(plot.title = element_text(size = 16, face = "bold", vjust = 1), axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(0.25,0.1,-0.5,0.1), "lines"))
  plot_mid <- ggplot() +
    geom_segment(data=subset(my_df, Key2=="Neg-to-Tot"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) +
    labs(list(x = "", y = "", color="Negative-to-Total")) + scale_x_date(breaks = date_breaks("1 year")) + theme_bw() +
    theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"))
  plot_bot <- ggplot() +
    geom_segment(data=subset(my_df, Key2=="Neg-to-Pos"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) + theme_bw() +
    labs(list(y = "", colour="Negative-to-Positive")) + theme(plot.margin=unit(c(-0.5,0.1,0.25,0.1), "lines"), axis.text.x=element_text(angle = 45, hjust=1)) +
    scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y"))
  
  #The following lines prior to grid.arrange align the plots (despite differences in ylabel lengths)
  gTop <- ggplotGrob(plot_top)
  gMid <- ggplotGrob(plot_mid)
  gBot <- ggplotGrob(plot_bot)
  maxWidth = grid::unit.pmax(gTop$widths[2:5], gMid$widths[2:5], gBot$widths[2:5])
  gTop$widths[2:5] <- as.list(maxWidth)
  gMid$widths[2:5] <- as.list(maxWidth)
  gBot$widths[2:5] <- as.list(maxWidth)
  
  grid.arrange(gTop, gMid, gBot, nrow=3, ncol=1, heights=c(0.35,0.30,0.35))
  
  dev.off()
}

#**********Creating Plots (Combined Sentiment of all 3 regions)********** ------------

for (i in countries) {
  prep_df <- data.frame(new_equity_rdy$Date, new_equity_rdy[,i], new_currency_rdy[,i], all_ntp_rdy[,i], all_ntt_rdy[,i])
  prep_df <- data.frame(new_equity_rdy$Date, new_equity_rdy[,i], new_currency_rdy[,i], all_ntp_rdy[,i], all_ntt_rdy[,i])
  colnames(prep_df) <- c("Date", "Equity Crisis", "Currency Crisis", "All Neg-to-Pos", "All Neg-to-Tot")
  prep_df$Date <- as.Date(prep_df$Date)
  long_prep_df <- gather(prep_df, Key, Values, -Date)
  my_df <- long_prep_df
  my_df$Key2 <- as.character(my_df$Key)
  my_df$Key2[my_df$Key2 %in% c("All Neg-to-Pos")] <- "Neg-to-Pos"
  my_df$Key2[my_df$Key2 %in% c("All Neg-to-Tot")] <- "Neg-to-Tot"
  my_df$Key2[my_df$Key2 %in% c("Equity Crisis", "Currency Crisis")] <- "Crisis"
  my_df[my_df == Inf] <- 0
  
  #Now begins the plotting, adjust location for saving and plot sizes in the following line
  png(filename = paste0("Plots/20151020_editCombinedSentiments/", i, ".png"), width = 600, height = 600)
  plot_top <- ggplot() + scale_x_date(breaks = date_breaks("1 year")) + #scale_y_discrete(breaks=c(-0.1,0.0,1.0,1.1), labels=c(NULL,0.0,1.0,NULL)) +
    geom_segment(data=subset(my_df, Key2=="Crisis"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) + theme_bw() +
    scale_colour_manual(values = c("gray20", "orange1")) + labs(list(title = i, x = "", y = "", colour="Crisis Dummy")) +
    theme(plot.title = element_text(size = 16, face = "bold", vjust = 1), axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(0.25,0.1,-0.5,0.1), "lines"))
  plot_mid <- ggplot() +
    geom_segment(data=subset(my_df, Key2=="Neg-to-Tot"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) +
    labs(list(x = "", y = "", color="Negative-to-Total")) + scale_x_date(breaks = date_breaks("1 year")) + theme_bw() +
    theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines")) +
    scale_colour_manual(values="firebrick2")
  plot_bot <- ggplot() +
    geom_segment(data=subset(my_df, Key2=="Neg-to-Pos"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) + theme_bw() +
    labs(list(y = "", colour="Negative-to-Positive")) + theme(plot.margin=unit(c(-0.5,0.1,0.25,0.1), "lines"), axis.text.x=element_text(angle = 45, hjust=1)) +
    scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) + scale_colour_manual(values="deepskyblue2")
  
  #The following lines prior to grid.arrange align the plots (despite differences in ylabel lengths)
  gTop <- ggplotGrob(plot_top)
  gMid <- ggplotGrob(plot_mid)
  gBot <- ggplotGrob(plot_bot)
  maxWidth = grid::unit.pmax(gTop$widths[2:5], gMid$widths[2:5], gBot$widths[2:5])
  gTop$widths[2:5] <- as.list(maxWidth)
  gMid$widths[2:5] <- as.list(maxWidth)
  gBot$widths[2:5] <- as.list(maxWidth)
  
  grid.arrange(gTop, gMid, gBot, nrow=3, ncol=1, heights=c(0.35,0.30,0.35))
  
  dev.off()
}

#**********Creating Plots (6 total sentiments: Front Page and Business & Finance)********** ----------

my_seqdates <- seq.POSIXt(ymd("1995-01-01"), ymd("2014-12-31"), by = "day")
my_monthid <- paste(year(my_seqdates), str_pad(month(my_seqdates), 2, side = "left", pad="0"))

for (i in countries) {
  my_df <- data.frame(my_seqdates, as.data.frame(matrix(data = NA, ncol = 15)))
  colnames(my_df) <- c("Date", "Equity", "Currency", "Banking",
                       "US FP NtP", "Eu FP NtP", "Asia FP NtP",
                       "US BF NtP", "Eu BF NtP", "Asia BF NtP",
                       "US FP NtT", "Eu FP NtT", "Asia FP NtT",
                       "US BF NtT", "Eu BF NtT", "Asia BF NtT")
  my_df$Equity[my_df$Date %in% my_equity$Date] <- my_equity[my_equity$Date %in% my_df$Date, i]
  my_df$Currency[my_df$Date %in% my_currency$Date] <- my_currency[my_currency$Date %in% my_df$Date, i]
  my_df$Banking[my_df$Date %in% my_banking$Date] <- my_banking[my_banking$Date %in% my_df$Date, i]
  my_df$`US FP NtP`[my_df$Date %in% us_fp_ntp$Date] <- us_fp_ntp[us_fp_ntp$Date %in% my_df$Date, i]
  my_df$`Eu FP NtP`[my_df$Date %in% eu_fp_ntp$Date] <- eu_fp_ntp[eu_fp_ntp$Date %in% my_df$Date, i]
  my_df$`Asia FP NtP`[my_df$Date %in% asia_fp_ntp$Date] <- asia_fp_ntp[asia_fp_ntp$Date %in% my_df$Date, i]
  my_df$`US BF NtP`[my_df$Date %in% us_bf_ntp$Date] <- us_bf_ntp[us_bf_ntp$Date %in% my_df$Date, i]
  my_df$`Eu BF NtP`[my_df$Date %in% eu_bf_ntp$Date] <- eu_bf_ntp[eu_bf_ntp$Date %in% my_df$Date, i]
  my_df$`Asia BF NtP`[my_df$Date %in% asia_bf_ntp$Date] <- asia_bf_ntp[asia_bf_ntp$Date %in% my_df$Date, i]
  my_df$`US FP NtT`[my_df$Date %in% us_fp_ntt$Date] <- us_fp_ntt[us_fp_ntt$Date %in% my_df$Date, i]
  my_df$`Eu FP NtT`[my_df$Date %in% eu_fp_ntt$Date] <- eu_fp_ntt[eu_fp_ntt$Date %in% my_df$Date, i]
  my_df$`Asia FP NtT`[my_df$Date %in% asia_fp_ntt$Date] <- asia_fp_ntt[asia_fp_ntt$Date %in% my_df$Date, i]
  my_df$`US BF NtT`[my_df$Date %in% us_bf_ntt$Date] <- us_bf_ntt[us_bf_ntt$Date %in% my_df$Date, i]
  my_df$`Eu BF NtT`[my_df$Date %in% eu_bf_ntt$Date] <- eu_bf_ntt[eu_bf_ntt$Date %in% my_df$Date, i]
  my_df$`Asia BF NtT`[my_df$Date %in% asia_bf_ntt$Date] <- asia_bf_ntt[asia_bf_ntt$Date %in% my_df$Date, i]
  my_df[my_df == Inf] <- 0
  
  my_longdf <- gather(my_df, Key, Values, -Date)
  my_longdf$Key2 <- as.character(my_longdf$Key)
  my_longdf$Key2[my_longdf$Key2 %in% c("US FP NtP", "Eu FP NtP", "Asia FP NtP",
                                         "US BF NtP", "Eu BF NtP", "Asia BF NtP")] <- "Neg-to-Pos"
  my_longdf$Key2[my_longdf$Key2 %in% c("US FP NtT", "Eu FP NtT", "Asia FP NtT",
                                         "US BF NtT", "Eu BF NtT", "Asia BF NtT")] <- "Neg-to-Tot"
  my_longdf$Key2[my_longdf$Key2 %in% c("Equity", "Currency", "Banking")] <- "Crisis"
  my_longdf$Date <- as.Date(my_longdf$Date)
  
  #Now begins the plotting, adjust location for saving and plot sizes in the following line
  png(filename = paste0("Plots/20151027_3 Crises 6 Sentiments/", i, ".png"), width = 700, height = 700)
  plot_top <- ggplot() + scale_x_date(breaks = date_breaks("1 year")) + #scale_y_discrete(breaks=c(-0.1,0.0,1.0,1.1), labels=c(NULL,0.0,1.0,NULL)) +
    geom_segment(data=subset(my_longdf, Key2=="Crisis"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) + theme_bw() +
    scale_colour_manual(values = c("turquoise3", "orange1", "green3")) + labs(list(title = i, x = "", y = "", colour="Crisis Dummy")) +
    theme(plot.title = element_text(size = 16, face = "bold", vjust = 1), axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(0.25,0.1,-0.5,0.1), "lines")) +
    guides(colour = guide_legend(overide.aes = list(size=3)))
  plot_mid <- ggplot() + guides(colour = guide_legend(overide.aes = list(size=3))) +
    geom_segment(data=subset(my_longdf, Key2=="Neg-to-Tot"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) +
    labs(list(x = "", y = "", color="Negative-to-Total")) + scale_x_date(breaks = date_breaks("1 year")) + theme_bw() +
    theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"))
  plot_bot <- ggplot(data=subset(my_longdf, Key2=="Neg-to-Pos"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) +
    geom_segment() + theme_bw() + guides(colour = guide_legend(overide.aes = list(size=3, linetype = 0))) +
    labs(list(y = "", colour="Negative-to-Positive")) + theme(plot.margin=unit(c(-0.5,0.1,0.25,0.1), "lines"), axis.text.x=element_text(angle = 45, hjust=1)) +
    scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y"))
  
  #The following lines prior to grid.arrange align the plots (despite differences in ylabel lengths)
  gTop <- ggplotGrob(plot_top)
  gMid <- ggplotGrob(plot_mid)
  gBot <- ggplotGrob(plot_bot)
  maxWidth = grid::unit.pmax(gTop$widths[2:5], gMid$widths[2:5], gBot$widths[2:5])
  gTop$widths[2:5] <- as.list(maxWidth)
  gMid$widths[2:5] <- as.list(maxWidth)
  gBot$widths[2:5] <- as.list(maxWidth)
  
  grid.arrange(gTop, gMid, gBot, nrow=3, ncol=1, heights=c(0.35,0.30,0.35))
  
  dev.off()
}





#####
#**********PLAYGROUND**********

#creating sequence for tick labels (did not work because labels fewer than tick marks?!)
my_seq <- as.character(seq(1995,2015,2))
my_desired <- as.character()
my_count <- 0
for (i in seq(1,21)) {
  if (i%%2 == 1) {my_count = my_count + 1; my_desired <- append(my_desired, my_seq[my_count])}
  else {my_desired <- append(my_desired, "")}
}


##########Method One
plotted_df <- ggplot() +
  geom_segment(data=subset(my_df, Key2=="Crisis"), mapping=aes(Date, Values, yend=0, xend=Date, colour=c("black", "red"))) +
  geom_segment(data=subset(my_df, Key2=="Neg-to-Tot"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) +
  geom_segment(data=subset(my_df, Key2=="Neg-to-Pos"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) +
  ggtitle(i) + facet_grid(Key2 ~ ., scales="free") +
  theme(plot.title = element_text(size = 16, face = "bold", vjust = 1))

ggsave(paste0(i, ".png"), plotted_df, path = "Plots/20151015/", scale = 8, width = 0.8, height = 0.8)
#########Method Two
png(paste0("Plots/20151014/", i, "_testing.png"), width = 600, height = 600)

plot_top <- ggplot() +
            geom_segment(data=subset(my_df, Key2=="Crisis"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) + 
            scale_colour_manual(values = c("gray20", "orange1")) + labs(list(title = i, x = "", y = "", colour="Crisis")) +
            theme(plot.title = element_text(size = 16, face = "bold", vjust = 1), axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(0.25,0.1,-0.5,0.1), "lines"))
plot_mid <- ggplot() +
            geom_segment(data=subset(my_df, Key2=="Neg-to-Tot"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) +
            labs(list(x = "", y = "", color="Negative-to-Total")) +
            theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"))
plot_bot <- ggplot() +
            geom_segment(data=subset(my_df, Key2=="Neg-to-Pos"), mapping=aes(Date, Values, yend=0, xend=Date, colour=Key)) +
            labs(list(y = "", colour="Negative-to-Positive")) + theme(plot.margin=unit(c(-0.5,0.1,0.25,0.1), "lines"))

gTop <- ggplotGrob(plot_top)
gMid <- ggplotGrob(plot_mid)
gBot <- ggplotGrob(plot_bot)
maxWidth = grid::unit.pmax(gTop$widths[2:5], gMid$widths[2:5], gBot$widths[2:5])
gTop$widths[2:5] <- as.list(maxWidth)
gMid$widths[2:5] <- as.list(maxWidth)
gBot$widths[2:5] <- as.list(maxWidth)

grid.arrange(gTop, gMid, gBot, nrow=3, ncol=1, heights=c(0.35,0.30,0.35))

dev.off()
