\name{austretail}
\alias{austretail}
\docType{data}
\title{
Australian Retail Data
}
\description{
Retail turnover in $Million AUD
}
\usage{data("austretail")}
\format{
  The format is:
An ‘xts’ object on 1982-04-30/2018-12-31 containing:
  Data: num [1:441, 1:152] 5.9 5.9 5.1 5.4 5 5.6 5.8 6.2 8.2 4.6 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:152] "ACF.CFPAR" "ACF.CLTH" "ACF.CRC" "ACF.CRTF" ...
  Indexed by objects of class: [Date] TZ: UTC
  xts Attributes:  
 NULL
}
\details{
The data is taken from the tsibbledata package of O'Hara-Wild, Hyndman and Wang, reformatted to
xts and abbreviated.
}
\source{
Australian Bureau of Statistics, catalogue number 8501.0, table 11.
}
\keyword{datasets}
