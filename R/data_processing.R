#' (experimental) Translating research area names
#'
#' Function to translate the English research areas (SSH, MINT, LS) names into
#' the corresponding language versions (de, fr, en) as abbreviations or long
#' format
#' @param research_area Research area to be translated as English abbreviation,
#' either "SSH", "MINT", or "LS".
#' @param target_lang Target language as abbreviation ("de", "en", or "fr").
#' @param Whether the output string should be an abbreviation or the whole, long
#' name (possible values: "abbr" or "long").
#' @export
#' @importFrom dplyr case_when
#' @examples
#'  translate_research_area("SSH", target_lang = "de", abbr_or_long = "long")
translate_research_area <- function(research_area = "",
                                    target_lang = "en",
                                    abbr_or_long = "abbr") {
  string <- ""
  if (abbr_or_long == "abbr") {
    string <- case_when(
      (
        target_lang == "en" ~
          case_when(
            research_area == "SSH" ~ "SSH",
            research_area == "MINT" ~ "MINT",
            research_area == "LS" ~ "LS",
            TRUE ~ "UNKNOWN"
          )
      ),
      (
        target_lang == "de" ~
          case_when(
            research_area == "SSH" ~ "GSW",
            research_area == "MINT" ~ "MINT",
            research_area == "LS" ~ "LW",
            TRUE ~ "UNKNOWN"
          )
      ),
      (
        target_lang == "fr" ~
          case_when(
            research_area == "SSH" ~ "SHS",
            research_area == "MINT" ~ "MINT",
            research_area == "LS" ~ "SV",
            TRUE ~ "UNKNOWN"
          )
      ),
      TRUE ~ "UNKNOWN"
    )
  } else if (abbr_or_long == "long") {
    string <- case_when(
      (
        target_lang == "en" ~
          case_when(
            research_area == "SSH" ~ "Social sciences and humanities",
            research_area == "MINT" ~
              "Mathematics, natural and engineering sciences",
            research_area == "LS" ~ "Life sciences",
            TRUE ~ "UNKNOWN"
          )
      ),
      (
        target_lang == "de" ~
          case_when(
            research_area == "SSH" ~ "Geistes- und Sozialwissenschaften",
            research_area == "MINT" ~
              "Mathematik, Natur- und Ingenieurwissenschaften",
            research_area == "LS" ~ "Lebenswissenschaften",
            TRUE ~ "UNKNOWN"
          )
      ),
      (
        target_lang == "fr" ~
          case_when(
            research_area == "SSH" ~ "Sciences humaines et sociales",
            research_area == "MINT" ~
              "Mathématiques, sciences naturelles et de l'ingénieur",
            research_area == "LS" ~ "Sciences de la vie",
            TRUE ~ "UNKNOWN"
          )
      ),
      TRUE ~ "UNKNOWN"
    )
  } else {
    stop("'abbr' or 'long' are possible.")
  }
  string
}

#' Print numbers as characters with language specific format
#'
#' Function to format a given number with a specified language-specific
#' formatting (thousand separator and decimal separator)
#' @param x The numeric to be transformed
#' @param lang Output format language, available is "en", "de", or "fr"
#' @export
#' @examples
#'  print_num(10000, "en")
#'  print_num(10000, "fr")
#'  print_num(10000, "de")
#'  print_num(pi, "en")
#'  print_num(pi, "fr")
#'  print_num(pi, "de")
print_num <- function(x, lang = "en") {
  if (lang == "en")
    prettyNum(x, big.mark = ifelse(x >= 10000, ",", ""), decimal.mark = ".")
  else if (lang == "de")
    prettyNum(x, big.mark = ifelse(x >= 10000, "&nbsp;", ""), decimal.mark = ",")
  else if (lang == "fr")
    prettyNum(x, big.mark = ifelse(x >= 10000, "&nbsp;", ""), decimal.mark = ",")
}
