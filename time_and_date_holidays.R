time_and_date_holidays <- function (countries = "Venezuela", years = year(Sys.Date()), 
                                    quiet = FALSE, include_regions = FALSE) 
{
  results <- NULL
  if (any(!years %in% (year(Sys.Date()) - 20):(year(Sys.Date()) + 
                                               20))) {
    warning(paste("Only allowing ± 20 years from today. Check:", 
                  v2t(years)))
  }
  year  <- year(Sys.Date())
  years <- years[years %in% ((year - 20L):(year + 20L))]
  combs <- expand.grid(years, countries) %>% dplyr::rename(year = "Var1", 
                                                           country = "Var2")
  for (i in seq_len(nrow(combs))) {
    if (!quiet) {
      message(paste0(">>> Extracting ", combs$country[i], 
                     "'s holidays for ", combs$year[i]))
    }
    
    url <- paste0("https://www.timeanddate.com/holidays/", 
                  tolower(combs$country[i]), "/", combs$year[i])
    
    ret <- httr::content(httr::GET(url, httr::add_headers(`Accept-Language` = "en")))
    
    holidays <- ret %>%
      rvest::html_nodes(".table") %>%
      rvest::html_table(fill = TRUE) %>% 
      data.frame(.) %>%
      filter(!is.na(.data$Date)) %>% 
      select(-2L) %>%
      mutate(Date = paste(.data$Date, combs$year[i])) %>%
      .[-1L, ] %>%
      lares::removenacols(all = TRUE) %>%
      lares::removenarows(all = TRUE)
    
    colnames(holidays) <- if (include_regions & ncol(holidays) > 3) {
      c("Date", "Holiday", "Holiday.Type", "Holiday.Details")
    }
    else {
      c("Date", "Holiday", "Holiday.Type")
    }
    
    grep_comment <- grep("*", holidays$Date, fixed = TRUE)
    
    if (length(grep_comment) != 0L) {
      holidays <- holidays[-grep_comment, ]
    }
    
    holidays$Date <- tryCatch({
      lubridate::dmy(holidays$Date)
    }, error = function(cond) {
      stop("Unaccounted problem(s) occurred parsing the date column.\n Check sample: ", 
           v2t(head(holidays$Date, 3)))
    })
    
    result <- data.frame(holiday = holidays$Date, holiday_name = holidays$Holiday, 
                         holiday_type = holidays$Holiday.Type)
    
    if (include_regions) 
      result$holiday_details <- holidays$Holiday.Details
    result <- result %>%
      mutate(
        national   = grepl("National|Federal", 
                           holidays$Holiday.Type),
        observance = grepl("Observance", 
                           holidays$Holiday.Type),
        bank       = grepl("Bank", holidays$Holiday.Type), 
        nonwork.   = grepl("Non-working", holidays$Holiday.Type), 
        season     = grepl("Season", holidays$Holiday.Type), 
        hother     = !grepl("National|Federal|Observance|Season", 
                            holidays$Holiday.Type)
      ) %>% {
        if (length(unique(countries)) > 1L) {
          mutate(
            .,
            country = combs$country[i]
          )
        }
        else {
          .
        }
      }
    result$county <- combs$country[i]
    results <- bind_rows(results, result)
  }
  
  results <- results %>%
    filter(!is.na(.data$holiday)) %>% 
    lares::cleanNames() %>%
    as_tibble()
  
  return(results)
}
