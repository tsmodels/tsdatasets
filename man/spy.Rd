\name{spy}
\alias{spy}
\docType{data}
\title{
SPY ETF Adjusted Close
}
\description{
The adjusted closing price of the SPY ETF.
}
\usage{data("spy")}
\format{
The format is:
An xts object on 1993-01-29 / 2023-03-30 containing: 
Data:    double [7597, 1]
Columns: Close
Index:   Date [7597] (TZ: "UTC")
}
\source{
From Yahoo Data.
}
\examples{
head(data(spy))
}
\keyword{datasets}
