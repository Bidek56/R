options( width = 190, warn=1, scipen=3 )
# setwd("C:/Users/user/Documents/R/work/termination")

# load("C:\Users\user\Documents\R\work\termination/.Rdata", .GlobalEnv)
fileDate <- format( Sys.time(), "%Y%m" )

# rm(list = ls())
ls()
if (F) {
	# con <- file( "C:\\Users\\users\\Documents\\SQL Server Management Studio\\Projects\\terminationModel.sql", "r", blocking = FALSE )
	# sqlText <- scan( file=con, what="", sep="\n" )
	# close(con);
	# sqlCmd = "";
	# for ( i in 0:length(sqlText) )
	# 	sqlCmd <- paste(sqlCmd, sqlText[i], sep=" ");
	# rm(i)

	require(RODBC)

	channel <- odbcConnect("sql-ts")
	# channel <- odbcDriverConnect("Driver={SQL Native Client};Server=sql-ts;Database=LP;Trusted_Connection=yes;")
	# odbcGetInfo(channel) # under Windows XP
	# print(channel)

	# sql <- gsub( "\n", "", "select * FROM [hpa].[dbo].[vwMdrTerminations]" )
	sqlCmd <- gsub( "\n", "", "exec lp..[spTerminationModel]" )

    # print(sqlCmd)
	term <- sqlQuery( channel, sqlCmd, errors = TRUE )
	print(odbcGetErrMsg(channel))

	if (exists("channel")) odbcClose(channel)

	print(summary(term));
	# write.csv( term, file = paste ("loans.", fileDate, ".csv", sep="" ) )
	# write.csv( term, file = paste ("loans.", fileDate, ".csv", sep="" ) )
	write.csv( term, file = "loans.csv", row.names = FALSE )
	# read.csv("foo.csv", row.names = 1)
	q()
}

term <- read.csv( "loans.csv", row.names = 1 )

paste("Loan Count: ", formatC( length(term$ReportDt) , mode="integer", big.mark="," ))
# summary( subset( term, aqStatus == 'CURRENT' ) )
# summary( subset( term, StartDelinq == '0-29') )
summary( term )

table( term$StartDelinq )

sfix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), "C" = 1, "3" = 2, "6" = 3, "9" = 4, "F" = 5, "R" = 6, toupper(x) ) }

table ( term$startF <- sapply( term$StartDelinq, function(x) sfix ( toString(x) ) ) )
# table ( term$startX <- sapply( term$AcqStatus, function(x) efix ( toString(x) ) ) )
# table ( term$startX <- term$AcqStatus )
# table ( term$lienX <- term$Lien )

## termination status
# efix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), "PIF" = "PIF", "RP" = "RP", "TPS" = "TPS", "SS" = "SS" , "REO" = "REO",  toupper(x) ) }
ffix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), "PIF" = 1, "PERF" = 2, "SHORT" = 3, "LOSS" = 4, "NP" = 5, toupper(x) ) }

table ( term$endF <- sapply( term$EndStatus, function(x) ffix ( toString(x) ) ) )
table ( term$endX <- term$EndStatus )

# Product Type
tfix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), "ARM" = 0, "FIX" = 1, "NEG" = 3, toupper(x) ) }
table( term$typeX <- sapply( term$Product, function(x) tfix ( x ) ) )

# Purpose
pfix <- function(x) { switch ( EXPR = tolower(sub(' +$', '', x)), "purchase" = 0, "cash-out"=1, "rate refi"=2, "refi" = 2, "second" = 3, "other" = 4, x ) }
table( term$purpX <- sapply( term$Purpose, function(x) pfix ( x ) ) )

# Occupancy
ofix <- function(x) { switch ( EXPR = tolower(sub(' +$', '', x)), "owner" = 0, "investor" = 1, "second" = 2, "other" = 1, 0 ) }
table( term$occupX <- sapply( term$Occupancy, function(x) ofix ( x ) ) )

rfix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), 
								"SFR" = 0, "PUD" = 0, "TOWNHOUSE" = 0,
								"2-4" = 1, "5+" = 1,
								"CONDO" = 2, "COOP" = 2,
								"MANUFACTURED" = 3, "OTHER" = 4, 0 ) }

table( term$propX <- sapply( term$PropType, function(x) rfix ( x ) ) )

### Delinquent
dfix <- function(x) { switch ( EXPR = tolower(sub(' +$', '', x)), "perfect" = 1, "1x30" = 2, "dented" = 3, x ) }
table( term$delinqX <- sapply( term$DType, function(x) dfix ( x ) ) )

sfix <- function(x) { switch ( EXPR = toupper(sub(' +$', '', x)), 
	"AK" = 0, "AL" = 0, "AR" = 0, "AZ" = 1, "CA" = 1, "CO" = 1, "CT" = 2, "DC" = 2, "DE" = 2, "FL" = 3, "GA" = 3,
	"HI" = 1, "IA" = 0, "ID" = 0, "IL" = 4, "IN" = 4, "KS" = 0, "KY" = 0, "LA" = 0, "MA" = 2, "MD" = 2, "ME" = 0,
	"MI" = 4, "MN" = 0, "MO" = 0, "MS" = 0, "MT" = 0, "NC" = 0, "ND" = 0, "NE" = 0, "NH" = 0, "NJ" = 2, "NM" = 0,
	"NV" = 1, "NY" = 2, "OH" = 4, "OK" = 0, "OR" = 0, "PA" = 2, "PR" = 0, "RI" = 2, "SC" = 0, "SD" = 0, "TN" = 0,
	"TX" = 0, "UT" = 0, "VA" = 2, "VT" = 0, "WA" = 0, "WI" = 4, "WV" = 4, "WY" = 0, x ) }
table( term$stateX <- sapply( term$State, function(x) sfix ( x ) ) )

brks <- c(min(term$OrigAmt, na.rm=T), 50000, 100000, 200000, max(term$OrigAmt, na.rm=T) + 1)
table( term$balX <- cut( term$OrigAmt, breaks = brks, labels = F, right = F) )
# table( term$balL <- format(cut( term$BalOrig, breaks = brks, right = F), scientific=F, justify="right" ))

## Orig LTV buckets
brks <- c(floor(min(term$LtvOrig, na.rm=T)), 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 200, max(term$LtvOrig, na.rm=T)+1)
table( term$ltvX <- cut( term$LtvOrig, breaks = brks, labels = F, right = T) )
table( term$ltvL <- format(cut( term$LtvOrig, breaks = brks, right = T ), scientific=F, digits=4, justify="right" ))

## Curr LTV buckets
brks <- c(floor(min(term$LtvEff, na.rm=T)), 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 200, max(term$LtvEff, na.rm=T)+1)
table( term$cltvX <- cut( term$LtvEff, breaks = brks, labels = F, right = T) )
table( term$cltvL <- format(cut( term$LtvEff, breaks = brks, right = T ), scientific=F, digits=4, justify="right" ))

## Distressed LTV buckets
brks <- c(floor(min(term$LtvDistress, na.rm=T)), 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 200, max(term$LtvDistress, na.rm=T)+1)
table( term$dltvX <- cut( term$LtvDistress, breaks = brks, labels = F, right = T) )
table( term$dltvL <- format(cut( term$LtvDistress, breaks = brks, right = T ), scientific=F, digits=4, justify="right" ))

brks <- c(min(term$Fico, na.rm=T), 500, 550, 600, 650, 700, 750, max(term$Fico, na.rm=T)+1)
table( term$ficoX <- cut(term$Fico, breaks = brks, labels = F, right = F ) ) 
table( term$ficoL <- format(cut( term$Fico, breaks = brks, right = F ), scientific=F, digits=4, justify="right" ) )

brks <- c(min(term$Dti, na.rm=T), 25.0, 35.0, 45.0, 50.0, 55.0, 60.0, max(term$Dti, na.rm=T)+1)
table( term$dtiX <- cut(ifelse(term$Dti <=0, 50, term$Dti), breaks = brks, labels = F, right = FALSE ) )
table( format(cut(ifelse(term$Dti <=0, 50, term$Dti), breaks = brks, right = F ) ) )
term$dtiF <- format(cut(ifelse(term$Dti <=0, 50, term$Dti), breaks = brks, right = F ) )
# table( term$dtiX <- sapply(term$dtiX, function(x) max(term$dtiX, na.rm=TRUE) - x ))

# table(term$AgeBucket)
brks <- c(min(term$Age, na.rm=F),24, 48, 60, 120, max(term$Age, na.rm=F)+1)
table( term$ageX <- cut(term$Age, breaks = brks, labels = F, right = F) )
#table( term$agex <- sapply(term$agex, function(x) x- 1 ))


brks <- c(min(term$CurrCoupon, na.rm=T), 4, 6, 8, max(term$CurrCoupon, na.rm=T)+1)
table( term$rateX <- cut( term$CurrCoupon, breaks = brks, labels = F, right = FALSE ) )
#table( term$ratex <- sapply(term$ratex, function(x) x- 1 ))


# brks <- c(0,7,13,25,max(term$MonthsInInv, na.rm=T)+1)
# table( term$invX <- cut(term$MonthsInInv, breaks = brks, labels = F, right = FALSE ) )
# table( format(cut(term$MonthsInInv, breaks = brks, right = FALSE ) ))
# table( term$invX <- sapply(term$invX, function(x) x - 1 ))

brks <- c(1950,2000,2001,2002,2003,2004,2005,2006,2007,2008,2020)
table( term$origyrX <- cut(term$OrigYr, breaks = brks, labels = F, right = FALSE ) )
table( format(cut(term$OrigYr, breaks = brks, right = FALSE ) ))
#table( term$origyrX <- sapply(term$origyrx, function(x) x- 1 )) 
str( term )

# brks <- c(min(term$LTV, na.rm=T), 0.75, 0.80, 0.85, 0.90, 0.95, 1.0, 1.05, 1.10, 1.15, 1.20, 1.25, 1.35, max(term$LTV, na.rm=T))
# table( term$ltvX <- factor( cut( term$LTV, breaks = brks, right = FALSE ) ) )
# table( term$ltvX <- format( cut( term$LTV, breaks = brks, right = FALSE ) ) )
# table( term$ltvX <- cut( term$LTV, breaks = brks, labels = FALSE, right = FALSE) )
# table( term$typeX <- cut( as.factor(term$Type ) ) )
# table( term$startX <- apply( term, 1, function (x) sfix ( x["StartDelinq"], x["ltvX"] ) ) )
# table(sapply( term$StartDelinq, function(x) sfix ( toString(x) ) ) )
# term$startX <- sapply( term$StartDelinq, function(x) sfix ( toString(x) ) )

options(contrasts = c("contr.treatment", "contr.poly"))
library(nnet)
# library(MASS)

# basecase - initial run
formula <- "startF+propX+occupX+purpX+balX+rateX+ltvX+cltvX+dltvX+ficoX+dtiX+ageX+stateX+delinqX+origyrX"
model <- as.formula(paste("endF ~ ", formula))

# calc Chisq and F statistics
library (gnm)
mod.gnm <- gnm( formula=model, data=term, family=gaussian, na.action=na.omit, weights=SchedBal )
summary( mod.gnm )
anova(mod.gnm, test = "Chisq")
anova(mod.gnm, test = "F")
# summary( aov( formula=model, data=term, na.action=na.omit, weights=SchedBal ) )

model <- as.formula(paste("endX ~ ", formula))
model

mod.multinom  <- multinom( formula=model, data=term, weights=SchedBal)
coeff <- coefficients(mod.multinom)
coeff
# sum(residuals(mod.multinom,type="pearson")^2)

# mod.multinom1 <- multinom(endF ~ typeX + purpX + lienX + ltvX + ficoX, data=term)
# mod.multinom2 <- multinom( as.formula(paste("endX ~ ", gsub('\\+', '*', formula) ) ), data=term, weights=SchedBal)
# anova(mod.multinom, mod.multinom2, type=c("II", "III", 2, 3), test = "Chisq")
# anova(mod.multinom1, mod.multinom2, test = "Chisq")

# ends <- ordered(term$endX, levels=c('CUR', 'PDF', 'DLQ'))

p.port <- subset( term, as.Date(ReportDt) >= as.Date(Sys.time()) - 60 )
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


stats <- unique(as.character(p.all$StartDelinq))
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

