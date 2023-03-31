\name{country_holidays}
\alias{country_holidays}
\title{
Country holidays for a given data range for the US, UK and Japan.
}
\description{
Returns a table (xts or data.table) of holidays for 3 countries.
}
\usage{
country_holidays(country = "US", from = as.Date("2000-01-01"), 
to = as.Date(Sys.Date()), observed = TRUE, output = c("xts", "data.table"))
}
\arguments{
\item{country}{
the ISO 3166-1 alpha-2 country code.
}
\item{from}{
start date.
}
\item{to}{
end date (can be in the future).
}
\item{observed}{
whether to return actual or observed dates for the holidays.
}
\item{output}{
Either a data.table or xts object, with the latter being in long format and
the former in wide format.
}
}
\value{
A data.table or xts object with dates and holiday names.
}
\examples{
print(country_holidays("JP", from = "2022-11-01", to = "2022-12-31", output = "data.table"))
}
