
library(RQuantLib)
library(RODBC)

options(width = 200)
# load(".RData")

format( Sys.time(), "%a %b %d %H:%M:%S %Y" )
reportDt <- format( Sys.time(), "%Y-%m-%d %H:%M:%S" )
tradeDt <- format( Sys.time(), "%Y-%m-%d" )
settleDt <- adjust( calendar="TARGET", dates=as.Date( tradeDt )+2, bdc=0 ) # settle is t+2

if ( F ) {

	library(RBloomberg)

	## by default establish connection via Desktop COM API
	conn <- blpConnect()
	## get Bloom data
	eda <- blpGetData(conn, c("ED1 Comdty","ED2 Comdty","ED3 Comdty","ED4 Comdty","ED5 Comdty","ED6 Comdty","ED7 Comdty","ED8 Comdty", "US0001W Index", "US0001M Index", "US0003M Index", "USSWAP3 Curncy", "USSWAP5 Curncy", "USSWAP10 Curncy", "USSWAP15 Curncy", "USSWAP20 Curncy", "USSWAP30 Curncy"), "PX_LAST")

	eda

	rownames(eda) <- gsub( "ED(\\d+) COMDTY", "fut\\1", rownames(eda))
	rownames(eda) <- gsub( "US000(\\d+.) INDEX", "d\\1", rownames(eda))
	rownames(eda) <- tolower(gsub( "USSWAP(\\d+) CURNCY", "s\\1y", rownames(eda)))
	tsQuotes <- as.list(tapply(eda$PX_LAST, rownames(eda),c))

	blpDisconnect(conn)
}

if ( T ) {

	try(rm( eda ),silent=TRUE)

	channel <- odbcConnect("sql-ts")

	sqlCmd <- gsub( "\n", "", "select * from POS..tblRates where [TimeStamp] = ( select MAX( [TimeStamp] ) from POS..tblRates )" )

    # print(sqlCmd)
	term <- sqlQuery( channel, sqlCmd, errors = TRUE )
	print(odbcGetErrMsg(channel))

	# print(term);

	if (exists("channel")) odbcClose(channel)

	# tags <- c( "ED1","ED2","ED3","ED4","ED5","ED6","ED7","ED8", "US0001W", "US0001M", "US0003M", "USSWAP3", "USSWAP5", "USSWAP10", "USSWAP15", "USSWAP20", "USSWAP30" )
	# tags <- c( "ED1","ED2","ED3","ED4","ED5","ED6","ED7","ED8", "US0001W", "US0001M", "US0003M", "USSWAP2", "USSWAP3", "USSWAP5", "USSWAP10", "USSWAP15", "USSWAP20", "USSWAP30" )
	tags <- c( "US0001M", "US0003M", "USSWAP3", "USSWAP5", "USSWAP10", "USSWAP15", "USSWAP20", "USSWAP30" )
	eda <- term[, tags]
	colnames(eda) <- gsub( "ED(\\d+)", "fut\\1", colnames(eda))
	colnames(eda) <- gsub( "US0012M", "d1y", colnames(eda))
	colnames(eda) <- gsub( "US000(\\d+.)", "d\\1", colnames(eda))
	colnames(eda) <- tolower(gsub( "USSWAP(\\d+)", "s\\1y", colnames(eda)))
	# eda <- as.data.frame(eda)
	eda
	tsQuotes <- lapply(eda, FUN=function(L) { ifelse( L < 25, L/100, L ) } )
}

# ls()

params <- list( tradeDate=as.Date( tradeDt ), settleDate=settleDt, dt=.0, interpWhat="zero", interpHow="spline" )
times <- seq(0,29.98,.0833333333333)

# tsQuotes$d1m <- 0.0075425
# tsQuotes


spCurves <- DiscountCurve(params, tsQuotes, times)
# plot( spCurves,setpar=FALSE)

time <- spCurves$times
lib1 <- spCurves$forwards
disc <- spCurves$discounts
lib3 <- numeric(length = length(lib1))
lib6 <- numeric(length = length(lib1))
lib01 <- numeric(length = length(lib1))
lib12 <- numeric(length = length(lib1))

head ( disc, 10 )
write.csv( disc, file = "disc.csv", row.names = T );

startDt <- settleDt
endDt <- settleDt
for(i in 1:length(lib1)) {

	end1Dt <- advance( calendar="TARGET", dates=settleDt, i, timeUnit=2, bdc=0 )
	end3Dt <- advance( calendar="TARGET", dates=settleDt, i+2, timeUnit=2, bdc=0 )
	end6Dt <- advance( calendar="TARGET", dates=settleDt, i+5, timeUnit=2, bdc=0 )
	end12Dt <- advance( calendar="TARGET", dates=settleDt, i+11, timeUnit=2, bdc=0 )
	diff1 <- end1Dt - startDt
	diff3 <- end3Dt - startDt
	diff6 <- end6Dt - startDt
	diff12 <- end12Dt - startDt
	startDt <- end1Dt
	print( sprintf( "EndDt:%s diff1:%d diff3: %d diff6:%d diff12:%d\n", endDt, diff1, diff3, diff6, diff12 ) )
	# print( endDt )
	# print( diff1 )
	# print( diff3 )
	# print( diff6 )
	# print( diff12 )

	lib01[i] = 100 * 12 * ( 12 * diff1 ) * ( disc[i]/disc[i+1] - 1 ) / 365.25
	lib3[i]  = 100 *  4 * (  4 * diff3 ) * ( disc[i]/disc[i+3] - 1 ) / 365.25
	lib6[i]  = 100 *  2 * (  2 * diff6 ) * ( disc[i]/disc[i+6] - 1 ) / 365.25
	lib12[i] = 100 *  diff12 * ( disc[i]/disc[i+12]- 1 ) / 365.25

}

fwd <- cbind( reportDt, round(spCurves$times*12)+1, round( lib01, 6), round( spCurves$forwards*100, 6 ), round( lib3, 6 ), round( lib6, 6 ), round( lib12, 6 ) );
colnames( fwd ) <- c( "ReportDt", "Period", "LIB01", "LIB1", "LIB3", "LIB6", "LIB1Y" );

write.csv( fwd, file = "fwd.csv", row.names = T );

# summary( fwd )
head( fwd, 50 )

if ( F ) {
	channel <- odbcConnect( dsn = "sql-ts;Database=POS" )
	sqlSave( channel, as.data.frame(fwd), tablename = "tblForwards", append = T, rownames = F, verbose = T );
	if (exists("channel")) odbcClose(channel)
}

q()
