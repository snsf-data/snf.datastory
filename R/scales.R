# Datastory colours for direct access (based on the SNSF corporate design:
# https://snf.styleguides.ch/manual/fr/1.1.1.0/home.html) ####

# Primary colours (100 percent, 70 percent, 30 percent, 10 percent) ----

#' @title Datastory dark blues
#'
#' @description 'SNSF dark blue' colours (100%, 70%, 30%, 10%).
#'
#' @export
#'
#' @examples
#' print(datastory_dark_blues)
datastory_dark_blues <- c("#5298BD", "#86B7D1", "#CBE0EB", "#EDF4F8")

#' @title Datastory blues
#'
#' @description 'SNSF blue' colours (100%, 70%, 30%, 10%).
#'
#' @export
#'
#' @examples
#' print(datastory_blues)
datastory_blues <- c("#83D0F5", "#A8DEF8", "#DAF1FC", "#F2FAFE")

#' @title Datastory dark reds
#'
#' @description 'SNSF dark red' colours (100%, 70%, 30%, 10%).
#'
#' @export
#'
#' @examples
#' print(datastory_dark_reds)
datastory_dark_reds <- c("#C95B40", "#D98C79", "#EFCDC5", "#F9EEEC")

#' @title Datastory reds
#'
#' @description 'SNSF red' colours (100%, 70%, 30%, 10%).
#'
#' @export
#'
#' @examples
#' print(datastory_reds)
datastory_reds <- c("#F08262", "#F4A791", "#FAD9D0", "#FDF2EF")

# Secondary colours (100 percent, 50 percent) ----

#' @title Datastory yellows
#'
#' @description 'SNSF yellow' colours (100%, 70%, 30%, 10%).
#'
#' @export
#'
#' @examples
#' print(datastory_yellow)
datastory_yellow <- c("#FBBE5E", "#FDDEAE")

#' @title Datastory greens
#'
#' @description 'SNSF green' colours (100%, 70%, 30%, 10%).
#'
#' @export
#'
#' @examples
#' print(datastory_green)
datastory_green <- c("#71B294", "#B8D8C9")

#' @title Datastory violets
#'
#' @description 'SNSF violet' colours (100%, 70%, 30%, 10%).
#'
#' @export
#'
#' @examples
#' print(datastory_violet)
datastory_violet <- c("#9D90B9", "#CEC7DC")

#' @title Datastory grays
#'
#' @description 'SNSF gray' colours (100%, 70%, 30%, 10%).
#'
#' @export
#'
#' @examples
#' print(datastory_grays)
datastory_grays <- c("#B2B1A7", "#D8D8D3")

#' @title Datastory single dark blue
#'
#' @description 'SNSF dark blue' colour (100%).
#'
#' @export
#'
#' @examples
#' print(snsf_single)
datastory_single <- "#5298BD"

# SNSF Schemes ----

#' @title Default Datastory colour schemes
#'
#' @description SNSF main palette composed of 12 colours. The first 6 colours
#' are based on SNSF secondary colours (position 1, 3, and 4), dark blue 100%
#' (position  2), red 100% (position 5), and blue 100% (position 6). The 6
#' remaining colours are a lighter version of the first 6.
#'
#' @export
#'
#' @examples
#' print(datastory_scheme)
datastory_scheme <- c(
  "#FBBE5E", # secondary yellow 100%
  "#5298BD", # primary dark blue 100%
  "#71B294", # secondary green 100%
  "#9D90B9", # secondary violet 100%
  "#F08262", # primary red 100%
  "#83D0F5", # primary blue 100%
  "#FDDEAE", # secondary yellow 50%
  "#A8CBDE",
  "#B8D8C9", # secondary green 50%
  "#CEC7DC", # secondary violet 50%
  "#F7C0B0",
  "#C1E7FA"

)

#' @title Alternative default datastory colour scheme
#'
#' @description Same palette as in `datastory_scheme`, but colours at position 2
#' and 6 are inverted, as well as colours at positions 7 and 12.
#'
#' @export
#'
#' @examples
#' print(datastory_scheme_waffles)
datastory_scheme_waffles <- c(
  "#FBBE5E", # secondary yellow 100%
  "#83D0F5", # primary blue 100%
  "#71B294", # secondary green 100%
  "#9D90B9", # secondary violet 100%
  "#F08262", # primary red 100%
  "#5298BD", # primary dark blue 100%
  "#FDDEAE", # secondary yellow 50%
  "#C1E7FA",
  "#B8D8C9", # secondary green 50%
  "#CEC7DC", # secondary violet 50%
  "#F7C0B0",
  "#A8CBDE"
)

#' @title Get the datastory colours you need
#'
#' @description Choose between different datastory colour palettes and get
#' additional colours through interpolation when needed.
#'
#' @param n_col The number of colours to return. If it exceeds the number of
#' colours in the chosen palette, additional colours are interpolated. Do not
#' set this argument if you want to get the original number of colours in the
#' chosen palette.
#' @param palette Choose between "default" and "waffles".
#' @param repeat_col Whether to repeat the colour scheme when there are more
#' colours requested (with `n_col`) than the colour scheme has to offer. When
#' set to `FALSE`, additional colours are interpolated.
#' @param reverse Whether the palette should be returned in reversed order. If
#' `n_col` is set and is smaller than the total number of colours in the
#' palette, the function takes the first `n_col` colours in the palette and
#' reverses their order. It does not reverse the whole palette and then take the
#' first `n_col` colours.
#'
#' @export
#'
#' @examples
#' # Default colours for charts
#' get_datastory_scheme()
#' # Colours for waffle charts
#' get_datastory_scheme(palette = "waffles")
get_datastory_scheme <- function(
  n_col = NULL,
  palette = "default",
  repeat_col = TRUE,
  reverse = FALSE
) {
  if (
    !any(
      chk::vld_whole_number(n_col) && chk::vld_gt(n_col, 0L),
      chk::vld_null(n_col)
    )
  ) {
    cli::cli_abort(
      paste0(
        "`n_col` must be a positive whole number (non-missing integer scalar ",
        "or double equivalent) or NULL."
      )
    )
  }
  chk::chk_string(palette)
  chk::chk_subset(palette, c("default", "waffles"))
  chk::chk_flag(repeat_col)
  chk::chk_flag(reverse)

  # Choose the right colour palette
  clrs <- switch(palette, default = snsf_scheme, waffles = snsf_scheme_waffles)

  # Interpolate colours if not enough are available
  if (!is.null(n_col)) {
    if (n_col > length(clrs)) {
      # Repeat (and not interpolate) the colour scheme when configured
      if (repeat_col) {
        clrs <- rep(clrs, ceiling(n_col / length(clrs)))[1L:n_col]
      } else {
        # Interpolate new colours
        clrs <- grDevices::colorRampPalette(clrs)(n_col)
      }
    }
    clrs <- clrs[1L:n_col]
    # Reverse colours before interpolating
    if (reverse) clrs <- rev(clrs)

  } else if (reverse) {
    clrs <- rev(clrs)
  }

  return(clrs)
}
