library(quantmod)
library(xts)

# install.packages("quantmod")

setwd( 'C:/Users/User/Documents/R/work/boost/' )

rm(list=ls())

# get market data
# Nasdaq100_Symbols <- c( "AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM" )
# Nasdaq100_Symbols <- c( "QQQ", "SPY", "IWM", "TLT", "XLE", "VXX", "XBI" )
Nasdaq100_Symbols <- c( "SPY", "IWM" )
getSymbols(Nasdaq100_Symbols, src = "yahoo" )
nasdaq100 <- data.frame(do.call(cbind, mget(Nasdaq100_Symbols), envir = parent.frame()))

if (T) {
   getSymbols(Nasdaq100_Symbols, src = "google" )

   nasdaq100_goo <- data.frame(do.call(cbind, mget(Nasdaq100_Symbols), envir = parent.frame()))

   new_dt <- row.names(nasdaq100_goo)[!(row.names(nasdaq100_goo) %in% row.names(nasdaq100))]
   new_goo_row <- nasdaq100_goo[new_dt,]
   new_goo_row['IWM.Adjusted'] = new_goo_row['IWM.Close']
   new_goo_row['SPY.Adjusted'] = new_goo_row['SPY.Close']

   nasdaq100 <- rbind( nasdaq100, new_goo_row )
}

# getSymbols(Nasdaq100_Symbols, src = "csv" )

# merge them all together
# nasdaq100 <- data.frame(as.xts(merge( SPY, IWM )))
# nasdaq100 <- data.frame(do.call(cbind, mget(Nasdaq100_Symbols), envir = parent.frame()))

# set outcome variable
outcomeSymbol <- "IWM.High"

# shift outcome value to be on same line as predictors
nasdaq100 <- xts(nasdaq100,order.by=as.Date(rownames(nasdaq100)))

nasdaq100 <- as.data.frame(merge(nasdaq100, lm1=lag(nasdaq100[,outcomeSymbol],-1)))
nasdaq100$outcome <- ifelse(nasdaq100[,paste0(outcomeSymbol,'.1')] > nasdaq100[,outcomeSymbol], 1, 0)

# remove shifted down volume field
nasdaq100 <- nasdaq100[,!names(nasdaq100) %in% c(paste0(outcomeSymbol,'.1'))]

# cast date to true date and order in decreasing order
nasdaq100$date <- as.Date(row.names(nasdaq100))
nasdaq100 <- nasdaq100[order(as.Date(nasdaq100$date, "%m/%d/%Y"), decreasing = TRUE),]

# calculate all day differences and populate them on same row
GetDiffDays <- function(objDF,days=c(10), offLimitsSymbols=c('outcome'), roundByScaler=3) {
  # needs to be sorted by date in decreasing order
  ind <- sapply(objDF, is.numeric)
  for (sym in names(objDF)[ind]) {
    if (!sym %in% offLimitsSymbols) {
      print(paste('*********', sym))
      objDF[,sym] <- round(scale(objDF[,sym]),roundByScaler)

      print(paste('theColName', sym))
      for (day in days) {
        objDF[paste0(sym,'_',day)] <- c(diff(objDF[,sym],lag = day),rep(x=0,day)) * -1
      }
    }
  }
  return (objDF)
}

# call the function with the following differences
nasdaq100 <- GetDiffDays(nasdaq100, days=c(1,2,3,4,5,10,20), offLimitsSymbols=c('outcome'), roundByScaler=2)

latestRow <- rownames(nasdaq100[1,])

# drop most recent entry as we don't have an outcome
# nasdaq100 <- nasdaq100[2:nrow(nasdaq100),]

# use POSIXlt to add day of the week, day of the month, day of the year
nasdaq100$wday <- as.POSIXlt(nasdaq100$date)$wday
nasdaq100$yday <- as.POSIXlt(nasdaq100$date)$mday
nasdaq100$mon  <- as.POSIXlt(nasdaq100$date)$mon

# remove date field and shuffle data frame
nasdaq100 <- subset(nasdaq100, select=-c(date))
nasdaq100 <- nasdaq100[sample(nrow(nasdaq100)),]

# xgboost Modeling
library(xgboost)
predictorNames <- names(nasdaq100)[names(nasdaq100) != 'outcome']

set.seed(1234)
# split <- sample(nrow(nasdaq100), floor(0.7*nrow(nasdaq100)))
# train <-nasdaq100[split,]
# test <- nasdaq100[-split,]

train <- subset( nasdaq100, format(as.Date( row.names(nasdaq100) ),"%Y") < 2017)
# test  <- subset( nasdaq100, format(as.Date( row.names(nasdaq100) ),"%Y") >= 2017)
# test_dates <- sort(row.names(test))

test_dates <- format(seq(as.Date("2017/10/26"), by = "day", length.out = 10), "%Y-%m-%d")

# max(row.names(train))

for (latestRow in test_dates) {
  # print(paste("Running for:", latestRow ))

  # latestRow = "2017-11-01"
  # train <- subset( nasdaq100, row.names(nasdaq100) != latestRow )
  test  <- nasdaq100[latestRow ,]

  bst <- xgboost(data = as.matrix(train[,predictorNames]),
               label = train$outcome,
               verbose = 0,
               eta = 0.1,
               gamma = 50,
               missing = NaN,
               nround = 150,
               colsample_bytree = 0.1,
               subsample = 1,
               nthread = 4,
               objective="binary:logistic")

  predictions <- predict(bst, as.matrix(test[,predictorNames]), missing = NaN, outputmargin=FALSE)
  print( paste( "Latest date: ", latestRow, " prob of new high:", predictions ) )
}

if (F) {
  library(pROC)
  auc <- roc(test$outcome, predictions)
  print(paste('AUC score:', auc$auc))
}
