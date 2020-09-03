\name{gas}
\alias{gas}
\docType{data}
\title{
US finished motor gasoline products supplied 
}
\description{
US finished motor gasoline products supplied (in thousands of barrels per day) 
from February 1991 to May 2005
}
\usage{data("gas")}
\format{
  The format is:
An ‘xts’ object on 1991-02-01/2005-05-06 containing:
  Data: num [1:745, 1] 6621 6433 6582 7224 6875 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr "value"
  Indexed by objects of class: [Date] TZ: UTC
  xts Attributes:  
 NULL
}
\source{
EIA \url{https://www.eia.gov/dnav/pet/}
}
\examples{
data(gas)
plot(gas)
}
\keyword{datasets}