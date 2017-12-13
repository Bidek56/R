
library(RQuantLib)
library(RODBC)
library(XML)

options(width = 200)
load(".RData")

format( Sys.time(), "%a %b %d %H:%M:%S %Y" )
TimeStamp <- format( Sys.time(), "%Y-%m-%d %H:%M:%S" )
tradeDt <- format( Sys.time(), "%Y-%m-%d" )
settleDt <- adjust( calendar="TARGET", dates=as.Date( tradeDt )+2, bdc=0 ) # settle is t+2

if ( F ) {

	library(RBloomberg)

	## by default establish connection via Desktop COM API
	conn <- blpConnect()
	## get Bloom data
	eda <- blpGetData(conn, c("ED1 Comdty","ED2 Comdty","ED3 Comdty","ED4 Comdty","ED5 Comdty","ED6 Comdty","ED7 Comdty","ED8 Comdty"
					, "US0001W Index", "US0002W Index", "US0001M Index", "US0002M Index", "US0003M Index", "US0006M Index", "US0009M Index", "US0012M Index"
					, "USSWAP2 Curncy", "USSWAP3 Curncy", "USSWAP4 Curncy", "USSWAP5 Curncy", "USSWAP6 Curncy", "USSWAP7 Curncy", "USSWAP8 Curncy"
					, "USSWAP9 Curncy", "USSWAP10 Curncy","USSWAP11 Curncy","USSWAP12 Curncy"
					, "USSWAP13 Curncy", "USSWAP14 Curncy", "USSWAP15 Curncy", "USSWAP16 Curncy", "USSWAP17 Curncy", "USSWAP18 Curncy", "USSWAP19 Curncy", "USSWAP20 Curncy"
					, "USSWAP21 Curncy", "USSWAP25 Curncy", "USSWAP30 Curncy"
					)
              , "PX_LAST" )

	eda

	blpDisconnect(conn)
}

ls()

eda <- eda1

rownames(eda) <- gsub( " COMDTY| INDEX| CURNCY", "", rownames(eda))

edaDf <- as.data.frame( cbind( TimeStamp, t(eda) ) )
edaDf

if ( F ) {
	channel <- odbcConnect( dsn = "sql-ts;Database=POS" )
	sqlSave( channel, as.data.frame(edaDf), tablename = "tblRates", append = T, rownames = F, verbose = T );
	if (exists("channel")) odbcClose(channel)
}

params <- list( tradeDate=as.Date( tradeDt ), settleDate=settleDt, dt=.0, interpWhat="zero", interpHow="spline" )
times <- seq(0,29.98,.0833333333333)

rownames(eda) <- gsub( "ED(\\d+)", "fut\\1", rownames(eda))
rownames(eda) <- gsub( "US0{2,3}(\\d+.)", "d\\1", rownames(eda))
rownames(eda) <- tolower(gsub( "USSWAP(\\d+)", "s\\1y", rownames(eda)))

tags <- c( "d1w", "d1m", "d3m", "fut1", "fut2", "fut3", "fut4", "fut5", "fut6", "fut7", "fut8", "s3y", "s5y", "s10y", "s15y", "s20y", "s30y" )
eda <- as.data.frame( eda[tags,] )
rownames(eda) <- tags
colnames(eda) <- c("PX_LAST")
eda

tsQuotes <- as.list(tapply(eda$PX_LAST, rownames(eda),c))
tsQuotes <- lapply(tsQuotes, FUN=function(L) { ifelse( L < 25, L / 100, L ) } )

tsQuotes



spCurves <- DiscountCurve(params, tsQuotes, times)
# plot( spCurves,setpar=FALSE)

spCurves$forwards <- spCurves$forwards * 100

fwd <- cbind( TimeStamp, round(spCurves$times*12)+1, round( spCurves$forwards, 6 ) );
colnames( fwd ) <- c( "ReportDt", "Period", "LIB1" );

# summary( fwd )
head( fwd )

if ( F ) {
	channel <- odbcConnect( dsn = "sql-ts;Database=POS" )
	sqlSave( channel, as.data.frame(fwd), tablename = "tblForwards", append = T, rownames = F, verbose = T );
	if (exists("channel")) odbcClose(channel)
}

q()

##############################################################################################################

## creates different curves based on different deposit, fra, futures and swap data
# d1w = 0.0016075, # d1m = 0.001858, # d3m = 0.002455, # d6m = 0.00395, # d9m = 0.00562, d1y = 0.0072775,
tsQuotes <- list( d1w = 0.0016075, d1m = 0.0018555, d3m = 0.0024575,
		 fut1 = 99.565, fut2 = 99.485, fut3 = 99.44, fut4 = 99.375, fut5 = 99.25, fut6 = 99.09, fut7 = 98.93, fut8 = 98.735,
		 # s2y  = 0.00647,
		 s3y  = 0.00979,
		 s5y  = 0.017645,
		 s10y = 0.0301,
		 s15y = 0.035295,
		 s20y = 0.03729,
		 s30y = 0.038815 )

# Loglinear interpolation of discount factors
logCurves <- DiscountCurve(params, tsQuotes, times)
summary( logCurves )
plot( logCurves,setpar=FALSE )

logCurves$forwards <- logCurves$forwards * 100

# Linear interpolation of discount factors
params$interpHow="linear"
linearCurves <- DiscountCurve(params, tsQuotes, times)
plot( linearCurves, setpar=FALSE )

linearCurves$forwards <- linearCurves$forwards * 100

# Spline interpolation of discount factors
params$interpHow="spline"
spCurves <- DiscountCurve(params, tsQuotes, times)
plot( spCurves,setpar=FALSE)

savepar <- par(mfrow=c(3,3), mar=c(4,4,2,0.5))
par(savepar)


spCurves$forwards <- spCurves$forwards * 100

fwd <- cbind( spCurves$times, logCurves$forwards, linearCurves$forwards, spCurves$forwards );
colnames( fwd ) <- c( "Time", "Log Linear", "Linear", "Spline" );

write.csv( fwd, file = "fwd.csv", row.names = T );

q()
