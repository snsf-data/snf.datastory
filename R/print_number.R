#' Print numbers as characters with language specific format
#'
#' Function to format a given number with a specified language-specific
#' formatting (thousand separator and decimal separator)
#'
#' @param x The numeric to be transformed
#' @param lang Output format language, available is "en", "de", or "fr"
#'
#' @export
#'
#' @examples
#'  print_num(10000, "en")
#'  print_num(10000, "fr")
#'  print_num(10000, "de")
#'  print_num(pi, "en")
#'  print_num(pi, "fr")
#'  print_num(pi, "de")
#'  print_num(10000 * pi, "en")
#'  print_num(10000 * pi, "fr")
#'  print_num(10000 * pi, "de")

print_num <- function(x, lang = "en") {

  if (!(lang %in% c("en", "de", "fr"))) {
    stop("`lang` must match any of 'en', 'de', or 'fr'")
  }

  sapply(
    x,
    \(x)
    if (x >= 10000) {
      if (lang == "en")
        prettyNum(x, big.mark = ",", decimal.mark = ".")
      else if (lang %in% c("de", "fr"))
        prettyNum(x, big.mark = "'", decimal.mark = ",")
    } else {
      if (lang == "en")
        prettyNum(x, big.mark = "", decimal.mark = ".")
      else if (lang %in% c("de", "fr"))
        prettyNum(x, big.mark = "", decimal.mark = ",")
    }
  )
}
