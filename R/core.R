# maRketSim - Core functions

# The heirarchy:
# core functions calculate values from values
# market objects calculates from core functions
# bond object and functions calculate from market object
# portfolio calculates from bond object and market object
# account calculates from bond object and market objects held within a portfolio

# 
# Time (in years):
# Each market object is a market in a single time period.  For convenience, we may want to create a market.history object which holds markets just like a portfolio holds bonds.
# Bonds inherit the market time when they are created and store that as their issue date.

# TODO
# Make accounts work - add history.account (just a data.frame of values over time) and plot.history.account
	# Current step: update.account needs to work
# Change "market" object class name to "market.bond"
# plot(history.market()) needs a legend
# Add zero-coupon bonds, stock markets, 
# Add daily pricing (between coupons)
# Add other kinds of risk premia to market objects (e.g. not just Treasuries)

# register package S3methods S3method(print,bond)




# --- Convenience functions --- #
# Uses taRifx instead


# --- Core functions --- #

# Find maturity from duration
# Right now this rounds to the nearest coupon date
findMat <- function(dur,i,f=.5) {
	#ms <- searchPattern(as.integer(dur)*5,as.integer(dur)*4/f,f)
	min.search <- ifelse(floor(dur)>0,floor(dur),.5) # ensure we can't be starting at a maturity of zero
	ms <- seq(min.search,min.search*10,.5)
	ds <- sapply(ms,findDur,i=i,f=f)
	return(ms[which.min(abs(ds-dur))])
}

# Find duration from maturity
findDur <- function(mat,i,market.rate=NA,f=.5,type="modified",...) {
	# Error check inputs
	if(type!="modified"&type!="Macaulay") stop("Must specify modified or Macaulay for record type.\n")
	# Set numbers which cancel out
	par=1000
	cpn=par*i*f
	# Calculate intermediate values
	if(is.na(market.rate)) market.rate=i
	pv <- findPV(i=i,market.rate=market.rate,mat=mat,f=f,par=par)
	# Calculate periods based on whether or not the bond is on its coupon date
	if(((mat/f)-as.integer(mat/f))!=0) { # If the bond isn't on its coupon date
		ms <- seq(as.integer(mat),as.integer(mat)+1,f)
		mat.whole <- ms[(mat-ms)>=0][length(ms[(mat-ms)>=0])]
		mat.frac <- mat-mat.whole
		w <- mat.frac/f # fraction of the period remaining until the next coupon payment
		n <- mat.whole/f # number of whole coupon payments remaining
		periods <- seq(1,n)-1+w
	} else { # If the bond falls on its coupon date
		n <- mat/f
		periods <- seq(1,n)
	}
	# Duration calculation
	dur.Macaulay <- f * (sum(periods * pv$pv.cf) / pv$pv ) # Macaulay duration, in years
	if(type=="Macaulay") {
		cat("Returning Macaulay duration\n")
		return(dur.Macaulay)
	} else {
		dur <- dur.Macaulay / (1+market.rate*f)
		return(dur)
	}
}

# Find PV of a bond and its cash flows - the fundamental function used by many others (including constructing a bond object)
findPV <- function(i,mat,market.rate,par=1000,f=.5,fractional.method="30/360") {
	if(((mat/f)-as.integer(mat/f))!=0) stop("No between-coupon maturities please.  This is currently producing discontinuous values in an odd way.")
	# Error check inputs
	if(fractional.method!="30/360") stop("actual/actual mid-period accounting not yet supported") # see Handbook of Fixed Income, 1991, p91
	# Calculate intermediate values
	cpn <- par*i*f
	# When settlement date falls between coupon periods
	if(fractional.method=="30/360" & ((mat/f)-as.integer(mat/f))!=0) {
		# Find fractional part of the maturity
		ms <- seq(as.integer(mat),as.integer(mat)+1,f)
		mat.whole <- ms[(mat-ms)>=0][length(ms[(mat-ms)>=0])]
		mat.frac <- mat-mat.whole
		w <- mat.frac/f # fraction of the period remaining until the next coupon payment
		n <- mat.whole/f # number of whole coupon payments remaining
		ws <- c(w,rep(1,n-1)) # Multiply only the first term by w
		periods <- w+seq(1,n)-1
		# Calculate present value of each cash flow
		pv.cpn <- ws*cpn/(1+market.rate*f)^periods
		pv.par <- par/(1+market.rate*f)^(n-1+w)
	} else { # When settlement date falls on a coupon period
		n <- mat/f # number of periods
		periods <- seq(1,n)
		pv.cpn <- cpn/(1+market.rate*f)^periods
		pv.par <-   par/(1+market.rate*f)^periods[n]
	}
	# - Calculate PV - #
	pv.cf <- pv.cpn
	pv.cf[n] <- pv.cpn[n] + pv.par
	pv <- sum(pv.cf)
	# Return results
	res <- list(pv=pv, pv.cpn = pv.cpn, pv.par = pv.par, pv.cf = pv.cf)
	res
}

# Find the future value of a cashflow
findFV <- function(P,i,t.elapsed,compound="continuous") {
	if(compound=="continuous") {		
		fv <- P * exp(i*t.elapsed)
	} else if (!is.na(as.numeric(compound))) {
		n <- 1/as.numeric(compound)
		fv <- P * (1+i/n)^(t.elapsed*n)
	} else { stop("Must specify compound as continuous or as a frequency e.g. 0.5 is semi-annual compounding.\n") }
	fv
}
