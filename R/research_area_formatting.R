#' (experimental) Translating research area names
#'
#' Function flexibly reformat research area names from/to long or short formats,
#' as well as from/to any of the three languages used (English, German, and
#' French). This function can be used flexibly to translate a research area,
#' reformat within the same language, or do both at the same time.
#'
#' @param research_area Research area to be translated as English abbreviation,
#' either "SSH", "MINT", or "LS".
#' @param from_lang The language of the input ("de", "en", or "fr").
#' @param to_lang The language of the output ("de", "en", or "fr").
#' @param from_format The format of the input ("short" or "long").
#' @param to_format The format of the output ("short" or "long").
#' @param output_as_factor Whether you want the output as factor (\code{TRUE}) or as
#' character (\code{FALSE}, default). If this is set to \code{TRUE}, levels will be ordered as
#' follows: SSH, MINT, and LS.
#'
#' @export
#'
#' @examples
#'  reformat_research_area(
#'      "SSH",
#'      from_lang = "en",
#'      to_lang = "de",
#'      from_format = "short",
#'      to_format = "long"
#'  )

reformat_research_area <- function(research_area,
                                   from_lang,
                                   to_lang,
                                   from_format,
                                   to_format,
                                   output_as_factor = FALSE) {

  # Build the function to call based "from_" parameters
  fn <-
    paste0(
      "translate_", from_format, "_",
      switch(from_lang, en = "english", de = "german", fr = "french"),
      "_research_area"
    )

  # Build the call with "to_" parameters
  fn_call <-
    call(
      fn,
      research_area = quote(research_area),
      output_lang = to_lang,
      output_format = to_format,
      output_as_factor = output_as_factor
    )

  eval(fn_call)

}

#' @importFrom cli cli_abort
#' @importFrom dplyr case_match
#' @importFrom forcats fct
#' @keywords internal

translate_short_english_research_area <- function(research_area,
                                                  output_lang,
                                                  output_format,
                                                  output_as_factor = FALSE) {

  if (mean(research_area %in% research_area_factor_order$en_short) != 1) {
    cli::cli_abort(
      c(
        "Invalid input passed to {.var research_area}",
        "i" = "Only the values {.val {research_area_factor_order$en_short}} are accepted"
      )
    )
  }

  research_area <-
    dplyr::case_match(
      research_area,

      research_areas_list$SSH_short_en ~
        research_areas_list[[paste0("SSH_", output_format, "_", output_lang)]],

      research_areas_list$MINT_short_en ~
        research_areas_list[[paste0("MINT_", output_format, "_", output_lang)]],

      research_areas_list$LS_short_en ~
        research_areas_list[[paste0("LS_", output_format, "_", output_lang)]],

      research_areas_list$MD_short_en ~
        research_areas_list[[paste0("MD_", output_format, "_", output_lang)]]
    )

  if (output_as_factor) {

    research_area <-
      forcats::fct(
        research_area,
        levels =
          research_area_factor_order[[paste0(output_lang, "_", output_format)]]
      )

  }

  return(research_area)

}

#' @importFrom cli cli_abort
#' @importFrom dplyr case_match
#' @importFrom forcats fct
#' @keywords internal

translate_long_english_research_area <- function(research_area,
                                                 output_lang,
                                                 output_format,
                                                 output_as_factor = FALSE) {

  en_long_legacy <-
    c(
      research_areas_list$SSH_long_legacy,
      research_areas_list$MINT_long_legacy,
      research_areas_list$LS_long_legacy
    )

  if (mean(research_area %in% en_long_legacy) == 1) {
    research_area <- reformat_long_english_legacy(research_area)
  }

  if (mean(research_area %in% research_area_factor_order$en_long) != 1) {
    cli::cli_abort(
      c(
        "Invalid input passed to {.var research_area}",
        "i" = "Only the values {.val {research_area_factor_order$en_long}} are accepted"
      )
    )
  }

  research_area <-
    dplyr::case_match(
      research_area,

      research_areas_list$SSH_long_en ~
        research_areas_list[[paste0("SSH_", output_format, "_", output_lang)]],

      research_areas_list$MINT_long_en ~
        research_areas_list[[paste0("MINT_", output_format, "_", output_lang)]],

      research_areas_list$LS_long_en ~
        research_areas_list[[paste0("LS_", output_format, "_", output_lang)]],

      research_areas_list$MD_long_en ~
        research_areas_list[[paste0("MD_", output_format, "_", output_lang)]]
    )

  if (output_as_factor) {

    research_area <-
      forcats::fct(
        research_area,
        levels =
          research_area_factor_order[[paste0(output_lang, "_", output_format)]]
      )

  }

  return(research_area)

}

#' @importFrom cli cli_abort
#' @importFrom dplyr case_match
#' @importFrom forcats fct
#' @keywords internal

translate_short_german_research_area <- function(research_area,
                                                 output_lang,
                                                 output_format,
                                                 output_as_factor = FALSE) {


  if (mean(research_area %in% research_area_factor_order$de_short) != 1) {
    cli::cli_abort(
      c(
        "Invalid input passed to {.var research_area}",
        "i" = "Only the values {.val {research_area_factor_order$de_short}} are accepted"
      )
    )
  }

  research_area <-
    dplyr::case_match(
      research_area,

      research_areas_list$SSH_short_de ~
        research_areas_list[[paste0("SSH_", output_format, "_", output_lang)]],

      research_areas_list$MINT_short_de ~
        research_areas_list[[paste0("MINT_", output_format, "_", output_lang)]],

      research_areas_list$LS_short_de ~
        research_areas_list[[paste0("LS_", output_format, "_", output_lang)]],

      research_areas_list$MD_short_de ~
        research_areas_list[[paste0("MD_", output_format, "_", output_lang)]]
    )

  if (output_as_factor) {

    research_area <-
      forcats::fct(
        research_area,
        levels =
          research_area_factor_order[[paste0(output_lang, "_", output_format)]]
      )

  }

  return(research_area)

}

#' @importFrom cli cli_abort
#' @importFrom dplyr case_match
#' @importFrom forcats fct
#' @keywords internal

translate_long_german_research_area <- function(research_area,
                                                output_lang,
                                                output_format,
                                                output_as_factor = FALSE) {


  if (mean(research_area %in% research_area_factor_order$de_long) != 1) {
    cli::cli_abort(
      c(
        "Invalid input passed to {.var research_area}",
        "i" = "Only the values {.val {research_area_factor_order$de_long}} are accepted"
      )
    )
  }

  research_area <-
    dplyr::case_match(
      research_area,

      research_areas_list$SSH_long_de ~
        research_areas_list[[paste0("SSH_", output_format, "_", output_lang)]],

      research_areas_list$MINT_long_de ~
        research_areas_list[[paste0("MINT_", output_format, "_", output_lang)]],

      research_areas_list$LS_long_de ~
        research_areas_list[[paste0("LS_", output_format, "_", output_lang)]],

      research_areas_list$MD_long_de ~
        research_areas_list[[paste0("MD_", output_format, "_", output_lang)]]
    )

  if (output_as_factor) {

    research_area <-
      forcats::fct(
        research_area,
        levels =
          research_area_factor_order[[paste0(output_lang, "_", output_format)]]
      )

  }

  return(research_area)

}

#' @importFrom cli cli_abort
#' @importFrom dplyr case_match
#' @importFrom forcats fct
#' @keywords internal

translate_short_french_research_area <- function(research_area,
                                                 output_lang,
                                                 output_format,
                                                 output_as_factor = FALSE) {


  if (mean(research_area %in% research_area_factor_order$fr_short) != 1) {
    cli::cli_abort(
      c(
        "Invalid input passed to {.var research_area}",
        "i" = "Only the values {.val {research_area_factor_order$fr_short}} are accepted"
      )
    )
  }

  research_area <-
    dplyr::case_match(
      research_area,

      research_areas_list$SSH_short_fr ~
        research_areas_list[[paste0("SSH_", output_format, "_", output_lang)]],

      research_areas_list$MINT_short_fr ~
        research_areas_list[[paste0("MINT_", output_format, "_", output_lang)]],

      research_areas_list$LS_short_fr ~
        research_areas_list[[paste0("LS_", output_format, "_", output_lang)]],

      research_areas_list$MD_short_fr ~
        research_areas_list[[paste0("MD_", output_format, "_", output_lang)]]
    )

  if (output_as_factor) {

    research_area <-
      forcats::fct(
        research_area,
        levels =
          research_area_factor_order[[paste0(output_lang, "_", output_format)]]
      )

  }

  return(research_area)

}

#' @importFrom cli cli_abort
#' @importFrom dplyr case_match
#' @importFrom forcats fct
#' @keywords internal

translate_long_french_research_area <- function(research_area,
                                                output_lang,
                                                output_format,
                                                output_as_factor = FALSE) {


  if (mean(research_area %in% research_area_factor_order$fr_long) != 1) {
    cli::cli_abort(
      c(
        "Invalid input passed to {.var research_area}",
        "i" = "Only the values {.val {research_area_factor_order$fr_long}} are accepted"
      )
    )
  }

  research_area <-
    dplyr::case_match(
      research_area,

      research_areas_list$SSH_long_fr ~
        research_areas_list[[paste0("SSH_", output_format, "_", output_lang)]],

      research_areas_list$MINT_long_fr ~
        research_areas_list[[paste0("MINT_", output_format, "_", output_lang)]],

      research_areas_list$LS_long_fr ~
        research_areas_list[[paste0("LS_", output_format, "_", output_lang)]],

      research_areas_list$MD_long_fr ~
        research_areas_list[[paste0("MD_", output_format, "_", output_lang)]]
    )

  if (output_as_factor) {

    research_area <-
      forcats::fct(
        research_area,
        levels =
          research_area_factor_order[[paste0(output_lang, "_", output_format)]]
      )

  }

  return(research_area)

}

#' @importFrom dplyr case_match
#' @keywords internal

reformat_long_english_legacy <- function(research_area) {

  research_area <-
    dplyr::case_match(
      research_area,
      research_areas_list$SSH_long_legacy ~  research_areas_list$SSH_long_en,
      research_areas_list$MINT_long_legacy ~  research_areas_list$MINT_long_en,
      research_areas_list$LS_long_legacy ~  research_areas_list$LS_long_en
    )

  return(research_area)
}

#' @keywords internal

research_areas_list <-
  list(
    SSH_short_en = "SSH",
    SSH_short_de = "GSW",
    SSH_short_fr = "SHS",
    SSH_long_en = "Social Sciences and Humanities",
    SSH_long_de = "Geistes- und Sozialwissenschaften",
    SSH_long_fr = "Sciences humaines et sociales",

    MINT_short_en = "MINT",
    MINT_short_de = "MINT",
    MINT_short_fr = "MINT",
    MINT_long_en = "Mathematics, Informatics, Natural Sciences and Technology",
    MINT_long_de = "Mathematik, Informatik, Naturwissenschaften und Technik",
    MINT_long_fr = "Math\u00E9matiques, informatique, sciences naturelles et technique", # nolint: line_length_integer

    LS_short_en = "LS",
    LS_short_de = "LW",
    LS_short_fr = "SV",
    LS_long_en = "Life Sciences",
    LS_long_de = "Lebenswissenschaften",
    LS_long_fr = "Sciences de la vie",

    SSH_long_legacy = "Humanities and Social Sciences",
    MINT_long_legacy = "Mathematics, Natural- and Engineering Sciences",
    LS_long_legacy = "Biology and Medicine",
    ID_long_legacy = "Interdisciplinary",
    MD_long_legacy = "Multi-domain",
    NA_long_legacy = "non-classifiable",

    MD_short_en = "Multi-domain",
    MD_short_de = "gebiets\u00fcbergreifend",
    MD_short_fr = "Multi-domaines",
    MD_long_en = "Multi-domain",
    MD_long_de = "gebiets\u00fcbergreifend",
    MD_long_fr = "Multi-domaines",

    NA_short_en = "non-classifiable",
    NA_short_de = "Nicht zuteilbar",
    NA_short_fr = "Non attribuable",
    NA_long_en = "non-classifiable",
    NA_long_de = "Nicht zuteilbar",
    NA_long_fr = "Non attribuable"
  )

#' @keywords internal

research_area_factor_order <-
  list(
    en_short =
      c(
        research_areas_list$SSH_short_en,
        research_areas_list$MINT_short_en,
        research_areas_list$LS_short_en,
        research_areas_list$MD_short_en
      ),
    en_long =
      c(
        research_areas_list$SSH_long_en,
        research_areas_list$MINT_long_en,
        research_areas_list$LS_long_en,
        research_areas_list$MD_short_en
      ),
    de_short =
      c(
        research_areas_list$SSH_short_de,
        research_areas_list$MINT_short_de,
        research_areas_list$LS_short_de,
        research_areas_list$MD_short_de
      ),
    de_long =
      c(
        research_areas_list$SSH_long_de,
        research_areas_list$MINT_long_de,
        research_areas_list$LS_long_de,
        research_areas_list$MD_long_de
      ),
    fr_short =
      c(
        research_areas_list$SSH_short_fr,
        research_areas_list$MINT_short_fr,
        research_areas_list$LS_short_fr,
        research_areas_list$MD_short_fr
      ),
    fr_long =
      c(
        research_areas_list$SSH_long_fr,
        research_areas_list$MINT_long_fr,
        research_areas_list$LS_long_fr,
        research_areas_list$MD_long_fr
      )
  )

#' @keywords internal
research_area <- function(research_area, lang, format) {

  research_areas_list[[paste(research_area, format, lang, sep = "_")]]

}
