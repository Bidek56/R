options( width = 190, warn=1, scipen=3 )
fileDate <- format( Sys.time(), "%Y%m" )

# rm(list = ls())
ls()
if (F) {
	# con <- file( "C:\\Users\\user\\Documents\\SQL Server Management Studio\\Projects\\terminationModel.sql", "r", blocking = FALSE )
	# sqlText <- scan( file=con, what="", sep="\n" )
	# close(con);
	# sqlCmd = "";
	# for ( i in 0:length(sqlText) )
	# 	sqlCmd <- paste(sqlCmd, sqlText[i], sep=" ");
	# rm(i)

	require(RODBC)

	channel <- odbcConnect("sql-ts")

	# sql <- gsub( "\n", "", "select * FROM [hpa].[dbo].[vwMdrTerminations]" )
	# sqlCmd <- gsub( "\n", "", "exec lp..[spTerminationModel]" )
	sqlCmd <- gsub( "\n", "", "exec lp..spDefaultModel" )

    # print(sqlCmd)
	term <- sqlQuery( channel, sqlCmd, errors = TRUE )
	print(odbcGetErrMsg(channel))

	if (exists("channel")) odbcClose(channel)

	print(summary(term));
	# write.csv( term, file = paste ("loans.", fileDate, ".csv", sep="" ) )
	# write.csv( term, file = paste ("loans.", fileDate, ".csv", sep="" ) )
	write.csv( term, file = "matrix.csv", row.names = FALSE )
	# read.csv("foo.csv", row.names = 1)
  stop()
	# q()
}

term <- read.csv( "matrix.csv" ) # , row.names = 0 )

# paste("Loan Count: ", formatC( length(term$ReportDt) , mode="integer", big.mark="," ))
# summary( subset( term, aqStatus == 'CURRENT' ) )
# summary( subset( term, StartDelinq == '0-29') )
summary( term )

## termination status
# efix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), "PIF" = "PIF", "RP" = "RP", "TPS" = "TPS", "SS" = "SS" , "REO" = "REO",  toupper(x) ) }
ffix <- function(x) { switch ( EXPR = tolower(sub(' +$', '', x)), "cur" = 1, "pif" = 2, "dlq" = 3, "liq" = 4, toupper(x) ) }

table ( term$endF <- sapply( term$CurrStatus, function(x) ffix ( toString(x) ) ) )
table ( term$endX <- term$CurrStatus )

# Product Type
ptfix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), "MBS" = 0, "ALTA" = 1, "ABS" = 2, toupper(x) ) }
table( term$ptypeX <- sapply( term$PType, function(x) ptfix ( x ) ) )

# Product Type
ltfix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), "FIX" = 0, "ARM" = 1, "NEG" = 2, toupper(x) ) }
table( term$ltypeX <- sapply( term$Product, function(x) ltfix ( x ) ) )

# Purpose
pfix <- function(x) { switch ( EXPR = tolower(sub(' +$', '', x)), "purchase" = 0, "cash-out"=1, "rate refi"=2, "refi" = 2, "second" = 2, "other" = 3, x ) }
table( term$purpX <- sapply( term$Purpose, function(x) pfix ( x ) ) )

# Occupancy
ofix <- function(x) { switch ( EXPR = tolower(sub(' +$', '', x)), "owner" = 0, "investor" = 1, "second" = 2, "other" = 1, 0 ) }
table( term$occupX <- sapply( term$Occupancy, function(x) ofix ( x ) ) )

rfix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), 
								"SFR" = 0, "PUD" = 0, "TOWNHOUSE" = 0,								
								"CONDO" = 1, "COOP" = 1,
                "'2-4" = 2, "5+" = 2,
								"MANUFACTURED" = 3, "OTHER" = 4, 0 ) }

table( term$propX <- sapply( term$PropType, function(x) rfix ( x ) ) )

afix <- function(x) { switch ( EXPR = tolower(sub(' +$', '', x)), "<=417k" = 0, "417k&<=1m" = 1, ">1m&<=1.5m" = 2, ">1.5m&<=2m" = 3, ">2m" = 4, 0 ) }
table( term$balX <- sapply( term$OrigAmtRange, function(x) afix ( x ) ) )


lfix <- function(x) { switch ( EXPR = tolower(sub(' +$', '', x)), "<=60" = 0, "60.01-65" = 1, "65.01-70" = 2, "70.01-75" = 3, "75.01-80" = 4, ">80" = 5, 6 ) }
table( term$ltvX <- sapply( term$LtvRange, function(x) lfix ( x ) ) )


fcfix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), '>=760' = 0, '740-759' = 1, '720-739' = 2
                               , '700-719' = 3, '660-699' = 4, '620-659' = 5, '580-619' = 6, '300-579' = 7, 'Missing' = 8, 8 ) }
                             
table( term$ficoX <- sapply( term$Fico, function(x) fcfix ( x ) ) )

# table(term$AgeBucket)
# brks <- c(min(term$Age, na.rm=F),24, 48, 60, 120, max(term$Age, na.rm=F)+1)
# table( term$ageX <- cut(term$Age, breaks = brks, labels = F, right = F) )
# table( term$agex <- sapply(term$agex, function(x) x- 1 ))

# brks <- c(min(term$CurrCoupon, na.rm=T), 4, 6, 8, max(term$CurrCoupon, na.rm=T)+1)
# table( term$rateX <- cut( term$CurrCoupon, breaks = brks, labels = F, right = FALSE ) )
# table( term$ratex <- sapply(term$ratex, function(x) x- 1 ))


# brks <- c(0,7,13,25,max(term$MonthsInInv, na.rm=T)+1)
# table( term$invX <- cut(term$MonthsInInv, breaks = brks, labels = F, right = FALSE ) )
# table( format(cut(term$MonthsInInv, breaks = brks, right = FALSE ) ))
# table( term$invX <- sapply(term$invX, function(x) x - 1 ))

brks <- c(1950,2000,2001,2002,2003,2004,2005,2006,2007,2008,2011,2020)
table( term$origyrX <- cut(term$OrigYr, breaks = brks, labels = F, right = FALSE ) )
table( format(cut(term$OrigYr, breaks = brks, right = FALSE ) ))
#table( term$origyrX <- sapply(term$origyrx, function(x) x- 1 )) 
str( term )

options(contrasts = c("contr.treatment", "contr.poly"))
library(nnet)
# library(MASS)

# basecase - initial run
# formula <- "ptypeX+propX+occupX+purpX+balX+ltvX+ficoX+origyrX"
formula <- "ptypeX+ltypeX+propX+occupX+purpX+balX+ltvX+ficoX"
model <- as.formula(paste("endF ~ ", formula))

# calc Chisq and F statistics
library (gnm)
mod.gnm <- gnm( formula=model, data=term, family=gaussian, na.action=na.omit, weights=CloseBal )
summary( mod.gnm )
anova(mod.gnm, test = "Chisq")
anova(mod.gnm, test = "F")
# summary( aov( formula=model, data=term, na.action=na.omit, weights=SchedBal ) )

model <- as.formula(paste("endX ~ ", formula))
model

mod.multinom  <- multinom( formula=model, data=term, weights=CloseBal)
coeff <- coefficients(mod.multinom)
coeff

stop();
# sum(residuals(mod.multinom,type="pearson")^2)

# mod.multinom1 <- multinom(endF ~ typeX + purpX + lienX + ltvX + ficoX, data=term)
# mod.multinom2 <- multinom( as.formula(paste("endX ~ ", gsub('\\+', '*', formula) ) ), data=term, weights=SchedBal)
# anova(mod.multinom, mod.multinom2, type=c("II", "III", 2, 3), test = "Chisq")
# anova(mod.multinom1, mod.multinom2, test = "Chisq")

# ends <- ordered(term$endX, levels=c('CUR', 'PDF', 'DLQ'))

p.port <- subset( term, as.Date(ReportDt) >= as.Date(Sys.time()) - 60 )

p.port <- term # subset( term, as.Date(ReportDt) >= as.Date(Sys.time()) - 60 )
old.colnames <- colnames(p.port)
# old.colnames[ old.colnames == "ageX" ] <- "ageA"
# old.colnames[ old.colnames == "startF" ] <- "startA"
# old.colnames[ old.colnames == "endF" ]   <- "startF"

names(p.port) <- old.colnames

p.fit <- predict(mod.multinom, newdata=p.port, type='probs')
# p.fit <- predict(mod.multinom, endX, newdata=p.port, type='probs')
# max( term$ReportDt, na.rm=TRUE )
length(p.port)
head(p.fit)

p.all <- data.frame(p.port, p.fit)
write.csv ( p.all, file = paste("fit.", fileDate, ".csv", sep=""), row.names = T )

# write.csv ( p.all, file = paste("fit.", fileDate, ".csv", sep=""), row.names = FALSE )
# subset( term, ReportDt == max(term$ReportDt, na.rm=TRUE) )

# stats <- unique(as.character(p.all$StartDelinq))
cols <- colnames(p.fit)
# for ( s in colnames(p.fit) ) { print s }
mm <- aggregate( p.all[ cols ], p.all[ 'StartDelinq' ], FUN=mean, na.rm = T )
mm


# library(relimp)
# relimp(mod.multinom, set1 = 2, set2 = 2:20, response.cat = "RP")

# data.frame(term, p.fit)[1:9,]
# p.all[1:10,]

q()

pdf( file = "plot.pdf",  pointsize = 4 )
plot(c(1,50), c(0,1), type='n', xlab="Count", ylab='Fitted Probability', main='Fitted Probability')
lines(1:50, p.fit[1:50, 'Perf'], lty=1, lwd=3, col='light blue')
lines(1:50, p.fit[1:50, 'PIF'], lty=2, lwd=3, col='green')
lines(1:50, p.fit[1:50, 'NP'], lty=3, lwd=3, col='orange')
lines(1:50, p.fit[1:50, 'Short'], lty=3, lwd=3, col='yellow')
lines(1:50, p.fit[1:50, 'Loss'], lty=3, lwd=3, col='red')
legend("bottomright", lty=1:5, lwd=2, legend=c('Perf', 'PIF', 'NP', 'Short', 'Loss' ), fill = c("light blue", "green", "orange", "yellow", "red" ))
dev.off()


q()

gnm.X <- gnm( formula=endX~typeX+purpX+Lien+ltvX+ficoX, data=term, family = gaussian, na.action = na.omit )
summary(gnm.X)

# ltbl <- ftable(term$Lien, term$startX, term$endX, dnn = c( "LTV", "Start", "Term" ) )
fit <- data.frame(predict.glm(gnm.X, newdata=term, type = "link", se.fit = F ), term)

old.colnames <- colnames(fit)
old.colnames[old.colnames == "predict.glm.gnm.X..newdata...term..type....link...se.fit...F."] <- "Score"
names(fit) <- old.colnames

# convert logit to prob
antilogit <- function(x) { exp(x)/(1+exp(x)) }

fit$prob <- sapply( fit$Score, function(x) antilogit ( x ) )

summary(fit)

# write.csv( fit, file = paste("fit.", fileDate, ".csv", sep=""), row.names = FALSE )

q()

ftbl <- ftable( term$Lien, term$Type, term$PPP, term$startX, term$endX, dnn = c( "Lien", "Type", "Start", "Term" ) )
ftbl
write.ftable ( ftbl, quote=F, file = "mdrterm.txt" )

ptbl <- prop.table( ftable( term$Lien, term$Type, term$PPP, term$startX, term$endX, dnn = c( "Lien", "Type", "Start", "Term" )), 1 )
write.ftable ( ptbl, quote=F, file = "mdrterm.txt", append=T, digits =  5 )

q()

sapply(unique(as.character(term$aqStatus)), function(s) summary( subset( term, aqStatus == s ) ) )


for ( s in unique(as.character(term$aqStatus)) ) {
	summary( subset( term, aqStatus == s ) )
    # glm( formula=aqStatus~currStatus, data=subset( term, aqStatus == s ), family = binomial(link = logit), na.action = na.omit )
	# print (s)
}

q()


chisq.test( term$aqStatus, term$currStatus )

glm.out <- glm( formula=aqStatus~currStatus, data=na.omit(term), family = binomial(link = logit), na.action = na.omit )
summary( glm.out )
