country_holidays <- function(country = "US", from = as.Date("2000-01-01"), to = as.Date(Sys.Date()), observed = TRUE, output = c("xts","data.table"))
{
    `.` <- holiday <- NULL
    fpath <- system.file("extdata", "holidays.rds", package = "tsdatasets")
    .country <- toupper(country[1])
    x <- readRDS(fpath)
    if (any(unique(x$country) == country)) {
        x <- x[country == .country]
        if (observed) {
            x <- x[observed >= from & observed <= to]
            x <- x[,.(observed, holiday)]
            names(x)[1] <- "date" 
        } else {
            x <- x[date >= from & date <= to]
            x <- x[,.(date, holiday)]
        }
        # remove duplicates
        x <- unique(x, by = "date")
        if (output == "data.table") return(x)
        x <- dcast(x, date~holiday, value.var = "holiday", fun.aggregate = length)
        sequence_dates <- seq(from, to, by = "days")
        y <- data.table(date = sequence_dates)
        y <- merge(y, x, by = "date", all.y = TRUE, all.x = TRUE)
        y[is.na(y)] <- 0
        y <- xts(coredata(y[,-1]), as.Date(y$date))
        check <- apply(y, 2, max)
        if (any(check == 0)) warning("\nfound holidays with no events registered for this date range...please remove")
        return(y)
    } else {
        stop("\ncountry not found. Please make sure to use ISO 3166-1 alpha-2 codes for country.")
    }
}