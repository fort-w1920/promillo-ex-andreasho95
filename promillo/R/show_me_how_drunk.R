#' Plotting alcohol value during drinking time
#'
#' Plots the calculated alcohol level at 5min intervals over the entire drinking time.
#'
#' @inheritParams tell_me_how_drunk
#' @return qplot visualizing the alcohol value [in per mille] during drinking time
#' @example
#' promillo::show_me_how_drunk(age = 39, sex = "male", height = 190, weight = 87,
#'     drinking_time = as.POSIXct(c("2016-10-03 17:15:00", "2016-10-03 22:55:00")),
#'     drinks = c("massn" = 3, "schnaps" = 4))
#' @seealso \code{\link{tell_me_how_drunk}} for the underlying calculation of the alcohol level
#' @export

show_me_how_drunk <- function(age, sex, height, weight, drinking_time, drinks) {

  checkmate::assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  # Create 5min intervalls over entire drinking time
  five_min_seq <- seq(lubridate::ymd_hms(drinking_time[1]), lubridate::ymd_hms(drinking_time[2]), by = '5 mins')

  # Calculate alcohol level for each 5min intervall
  alcohol_values <- vector(mode = "numeric", length = length(five_min_seq))
  for (i in seq_along(five_min_seq)) {
    alcohol_values[i] <- tell_me_how_drunk(age = age, sex = sex, height = height, weight = weight,
           drinking_time = c(drinking_time[1], five_min_seq[i]), drinks = drinks)
  }

  ggplot2::qplot(five_min_seq, alcohol_values, geom = "line", xlab = "Time", ylab = "Alcohol value [in \211]")
}

