\name{priceunits}
\alias{priceunits}
\docType{data}
\title{
Price and Volume Simulated Dataset
}
\description{
A Price and Volume bivariate dataset.
}
\usage{data("priceunits")}
\format{
  The format is:
An ‘xts’ object on 1992-08-31/2000-11-30 containing:
  Data: num [1:100, 1:2] 4.82 4.93 4.86 4.83 4.62 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:2] "price" "units"
  Indexed by objects of class: [Date] TZ: UTC
  xts Attributes:  
 NULL
}
\examples{
data(priceunits)
## maybe str(priceunits) ; plot(priceunits) ...
}
\keyword{datasets}
