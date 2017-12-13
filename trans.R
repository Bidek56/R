options( width = 200, warn=1, scipen=3, max.print = 400, digits=10 )

# getOption("max.print")

setwd("C:/Users/user/Documents/R/work/fannie")

rm(list = ls(all = TRUE))

transFile <- "Freddie_trans_2011_sql.csv"  # input data file
transFile <- "trans1.csv"

# read file
transSeries <- read.csv( transFile, sep=",", quote=NULL, header=T, stringsAsFactors=T )# , nrows = 5000 )

transSeries <- data.matrix(transSeries)

head( transSeries, n=20 )
# print ( unique(transSeries[,"orig_yyyy"] ) ) 

# maxYear <- max(transSeries[, "year"] * 10 + transSeries[,"quarter"] )

transSubset <- transSeries[ transSeries[, "Status24"] == 1 
               & transSeries[, "ULTV"] == 1 & transSeries[, "Score"] == 800
               & transSeries[,"ModFlg"] == 1 
               , ]
transSubset
# print ( unique(transSeries[,"loan_purpose"] ) )

transMx <- xtabs( Roll~Pdelinq+Cdelinq, data=transSubset[, c( "Pdelinq", "Cdelinq", "Roll" ) ] )

buyoutMx <- transMx[, "97"]  # buyout/default matrix
payoffMx <- transMx[, "99"]  # payoff matrix

# rowSums(transMx)

statusCount <- 6;
initBals <- matrix( c(1,0,0,0,0,0), nrow=1, ncol=statusCount, byrow=T )  # create initial matrix

prdBals <- matrix( 0, nrow=361, ncol=statusCount, byrow=T )  # rolling balance matrix
prdBals[1,] <- initBals[1,]   # default 1st period balances

# prdBals[1,] %*% buyouts

mdr <- vector( mode="numeric", length=361 );
def <- vector( mode="numeric", length=361 );
smm <- vector( mode="numeric", length=361 );
pff <- vector( mode="numeric", length=361 );

# prdBals[1,]

for ( p in 1:36) {
  prdBals[p+1,] <- prdBals[p,] %*% transMx[,1:statusCount] # matrix multiplication to transition balances
  # print( prdBals[p+1,] )

  def[p] <- prdBals[p,] %*% buyoutMx      # default balance
  mdr[p] <- def[p] / sum( prdBals[p,] )   # calc MDR
  pff[p] <- prdBals[p,] %*% payoffMx      # payoff balance
  smm[p] <- pff[p] / sum( prdBals[p,] )   # calc SMM
  cumsum <- sum( def[1:p] ) + sum( pff[1:p] )
  
  cdr <- ( 1-(1-mdr[p])^12 ) * 100
  vpr <- ( 1-(1-smm[p])^12 ) * 100
  print ( sprintf( "Prd: %s check: %9.7f CDR: %5.2f VPR: %5.2f", p, sum( prdBals[p+1,] )+cumsum, cdr, vpr ))
}

# rowSums(prdBals)
