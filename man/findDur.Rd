\name{findDur}
\Rdversion{1.1}
\alias{findDur}
\alias{findMat}
\title{
Find duration/maturity given interest rate and maturity/duration
}
\description{
Calculates the desired value given basic inputs
}
\usage{
findDur(mat, i, market.rate = NA, f = 0.5, type = "modified", ...)
findMat(dur,i,f=.5)
}
\arguments{
	  \item{mat}{
	Maturity
	}
	\item{dur}{Duration}
	\item{i}{
	Interest rate
	}
	  \item{market.rate}{
	Market rate under which to evaluate mat/dur
	}
	\item{f}{
	Frequency of coupon payments
	}
	\item{type}{
	"modified" currently the only option
	}
	\item{\dots}{ Pass-alongs. }
}
\value{
Returns a numeric value with the maturity/duration as described
}
