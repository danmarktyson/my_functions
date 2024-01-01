ts_rbf <- function(date, alpha, month) {
  if(!is.numeric(month)) stop("'month' argument must be numeric")
  
  date <- lubridate::as_date(date)
  year <- lubridate::year(date)
  
  start_of_month <- lubridate::ymd(paste0(year, "-" , month, "-01"))
  yday_of_month_start <- lubridate::yday(start_of_month)
  mid_month <- yday_of_month_start + lubridate::days_in_month(start_of_month) / 2
  
  day_of_year <- lubridate::yday(date)
  
  return(exp(-1 / (2 * alpha) * (day_of_year - mid_month)^2))
}
