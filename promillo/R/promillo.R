#' Calculates blood alcohol level.
#'
#' Calculates the current blood alcohol level based on a variety of drinks.
#' Formula ist based on \url{https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/}
#'
#' @param age Number between 10 and 110.
#' @param sex Character that matches either "male" or "female".
#' @param  height Number between 100 and 230 [in cm]
#' @param  weight Number between 40 and 300 [in kg]
#' @param  drinking_time Vector of length 2 containing two posixct indicating start and end of drinking
#' @param  drinks List containing the drinks. Each list element must contain two elements namely the
#' name of the drink and the quantity consumend. The drinks must be a subset of ("massn", "hoibe", "wein", "schnaps")
#' @return Numeric value for blood alocohol level [in per mille]
#' @examples
#' tell_me_how_drunk(age = 39, sex = "male", height = 190, weight = 87,
#' drinking_time = as.POSIXct(c("2016-10-03 17:15:00", "2016-10-03 22:55:00")),
#' drinks = c("massn" = 3, "schnaps" = 4))
#'
#' @export
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}

# utilities --------------------------------------------------------------------

#' Calculates the absorbed alcohol mass
#' @inheritParams tell_me_how_drunk
#' @return Double containing the absorbed alcohol mass
get_alcohol <- function(drinks) {
  # homogenize inputs:
  drinks <- unlist(drinks)
  checkmate::assert_subset(names(drinks), choices = c("massn", "hoibe", "wein", "schnaps"),
                empty.ok = FALSE)
  checkmate::assert_numeric(drinks, lower = 0)

  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40)
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4)
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
        alcohol_concentration[names(drinks)] * alcohol_density)
}

#' Calculates the person's body water
#' @inheritParams tell_me_how_drunk
#' @return Estimated body water of the person
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex)

  checkmate::assert_number(age, lower = 10, upper = 110)
  if (age < 16 | age > 90) {
    warning("...ts ts ts, this at your age!")
  }
  checkmate::assert_number(height, lower = 100, upper = 230)
  checkmate::assert_number(weight, lower = 40, upper = 300)

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}

#' Calculates the alcohol level
#' @param alcohol_drunk Absorbed alcohol mass. Calculated in \code{\link{get_alcohol}}
#' @param bodywater Estimated bodywater of the person. Calculated in \code{\link{get_bodywater}}
#' @inheritParams tell_me_how_drunk
#' @return Numeric value for blood alocohol level [in per mille]
get_permille <- function(alcohol_drunk, bodywater, drinking_time){
  checkmate::assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille  - (max(0, partylength - 1) * sober_per_hour))
}

