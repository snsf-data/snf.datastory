#==============================================================================#
# Datastory colours for direct access (based on the SNSF corporate design:
# https://snf.styleguides.ch/manual/fr/1.1.1.0/home.html)
#==============================================================================#

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
#' print(datastory_single)
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
  clrs <- switch(palette, default = datastory_scheme, waffles = datastory_scheme_waffles)

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

# Datastory scale functions ####

# The following functions are in majority inspired by the series of
# scale_*_viridis functions from the 'viridis' package. See the following page:
# https://github.com/sjmgarnier/viridis/blob/master/R/scales.R

#' @title Datastory colour palettes generator
#'
#' @description Wrapper function around [get_datastory_scheme()] to transform it
#' into a palette generator function compatible with
#' [discrete_scale()] or [scale_fill_gradient()]/[scale_fill_gradient2()].
#'
#' @details For more information, see [get_datastory_scheme()] for more
#' information on the datastory colour palettes available.
#'
#' @param palette Character string indicating the datastory colour palette to
#' use. Only those available in [get_datastory_scheme()] can be used.
#' @param repeat_col Logical indicating whether the colour should be repeated
#' sequentially (`TRUE`) when the palette does not return enough values (the
#' default). If set to `FALSE`, the number of colours will be extended using
#' interpolation.
#' @param reverse Logical indicating whether the scale should be reversed or
#' not (the default). Note that when more colours need to be generated, the
#' the colours are first reversed before before repeated/interpolated (this
#' allows to get, for a given palette size, consistent colours whatever it is
#' reversed or not).
#'
#' @keywords internal

datastory_pal <- function(
  palette = "default",
  repeat_col = TRUE,
  reverse = FALSE
) {
  # nolint start: return_linter
  function(n) {
    get_datastory_scheme(
      n,
      palette = palette,
      repeat_col = repeat_col,
      reverse = reverse
    )
  }
  # nolint end: return_linter
}

#' @title Datastory colour scales for ggplot2
#'
#' @description Fill and colour Datastory scale functions for [ggplot2].
#'
#' @param ... Parameters passed to external functions used to generate the
#' palette: [discrete_scale()], [scale_fill_gradient()],
#' [scale_fill_gradient2()], [scale_color_gradient()], or
#' [scale_color_gradient2()].
#'
#' @inheritParams datastory_pal
#' @param type Character string indicating the type of scale used to colour
#' the data. Four options are available: "qual" (default) for
#' qualitative/discrete data, "seq" for sequential qualitative/discrete data,
#' "cont" for continuous data, and "div" for diverging continuous data.
#' @param grad_col Character string indicating the colour of the scale. For
#' sequential scales, you can pick any of "blue", "red", "yellow", "green", or
#' "violet". For continuous scales, you can choose between "blue" (the default)
#' and "red": the gradient goes from blue/red 30% to dark blue/red 100%. For
#' diverging scale, there is a single option: from blue to red or vice versa.
#' @param aesthetics Character string or vector of character strings listing the
#' name(s) of the aesthetic(s) that this scale works with.
#'
#' @rdname scale_fill_datastory
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(forcats)
#'
#' Qualitative scale
#' mpg |>
#'   ggplot() +
#'   aes(y = class, fill = class) +
#'   geom_bar(aes(fill = class)) +
#'   scale_fill_datastory(type = "qual", guide = guide_legend(reverse = TRUE)) +
#'   get_datastory_theme(legend_position = "right")
#'
#' # Sequential scale
#' mpg |>
#'   summarise(mean = mean(cty), .by = cyl) |>
#'   mutate(cyl = fct_reorder(as.character(cyl), mean, .desc = TRUE)) |>
#'   ggplot() +
#'   aes(x = cyl, y = mean, fill = cyl) +
#'   geom_col() +
#'   scale_fill_datastory(type = "seq") +
#'   get_datastory_theme()
#'
#' # Continuous scale
#' mpg |>
#'   ggplot() +
#'   aes(x = cty, y = displ, color = hwy) +
#'   geom_point() +
#'   coord_flip() +
#'   scale_color_datastory(type = "cont") +
#'   get_datastory_theme()
#'
#' mpg |>
#'   mutate(hwy = (hwy - mean(hwy)) / sd(hwy)) |>
#'   ggplot() +
#'   aes(x = cty, y = displ, color = hwy) +
#'   geom_point() +
#'   scale_color_datastory(type = "div") +
#'   get_datastory_theme()

scale_fill_datastory <- function(...,
                            palette = "default",
                            repeat_col = TRUE,
                            reverse = FALSE,
                            type = "qual",
                            grad_col = "blue",
                            aesthetics = "fill") {

  # Get the list of arguments passed by the user (used for some checks)
  c_args <- as.list(match.call())

  chk::chk_string(type)
  chk::chk_subset(type, c("qual", "seq", "cont", "div"))
  chk::chk_string(grad_col)
  chk::chk_subset(grad_col, c("blue", "red", "green", "yellow", "violet"))

  # Inform the user that 'palette' and 'repeat_col are ignored when 'type' is
  # not set to "seq".
  if (!(grad_col %in% c("blue", "red"))  && type != "seq") {
    cli::cli_abort(
      paste0(
        "The colour {grad_col} can only be used for sequential scales (i.e. ",
        "when `type = \"seq\"`)."
      )
    )
  }

  # Inform the user that 'grad_col' is ignored when 'type = "seq"'
  if ("grad_col" %in% names(c_args) && type %in% c("qual", "div")) {
    cli::cli_inform(
      c(
        i = paste0(
          "The argument `grad_col` is not used when the scale is ",
          "qualitative or diverging. Value passed to that argument was thus ",
          "ignored."
        )
      )
    )
  }

  # Inform the user that 'palette' and 'repeat_col are ignored when 'type' is
  # not set to "seq".
  if (any(c("palette", "repeat_col") %in% names(c_args))  && type != "qual") {
    cli::cli_inform(
      c(
        i = paste0(
          "The argument `palette` and `repeat_col` are not used when the ",
          "scale is not qualitative. Any value passed to these arguments was ",
          "thus ignored."
        )
      )
    )
  }

  # Qualitative scale using an datastory scheme
  res_scale <- switch(
    type,
    qual = {
      if (utils::packageVersion("ggplot2") >= "3.5.0")
        ggplot2::discrete_scale(
          aesthetics,
          palette = datastory_pal(palette, repeat_col, reverse),
          ...
        )
      else
        ggplot2::discrete_scale(
          aesthetics,
          "datastory_scheme_qual",
          palette = datastory_pal(palette, repeat_col, reverse),
          ...
        )
    },
    cont = {
      low <- "#ececec"
      high <- switch(
        grad_col,
        blue = datastory_dark_blues[1L],
        red = datastory_dark_reds[1L]
      )
      ggplot2::scale_fill_gradient(
        low = ifelse(reverse, high, low),
        high = ifelse(reverse, low, high),
        aesthetics = aesthetics,
        ...
      )
    },
    div = ggplot2::scale_fill_gradient2(
      low = ifelse(reverse, datastory_dark_reds[1L], datastory_dark_blues[1L]),
      high = ifelse(reverse, datastory_dark_blues[1L], datastory_dark_reds[1L]),
      mid = "#ececec",
      aesthetics = aesthetics,
      ...
    ),
    seq = {
      seq_cols <- c(
        "#ececec",
        switch(
          grad_col,
          blue = datastory_dark_blues[1L],
          red = datastory_dark_reds[1L],
          green = datastory_green[1L],
          yellow = datastory_yellow[1L],
          violet = datastory_violet[1L]
        )
      )
      if (reverse) seq_cols <- rev(seq_cols)

      if (utils::packageVersion("ggplot2") >= "3.5.0")
        ggplot2::discrete_scale(
          aesthetics,
          palette = grDevices::colorRampPalette(seq_cols),
          ...
        )
      else
        ggplot2::discrete_scale(
          aesthetics,
          "datastory_scheme_seq",
          palette = grDevices::colorRampPalette(seq_cols),
          ...
        )
    }
  )

  return(res_scale)
}

#' @rdname scale_fill_datastory
#'
#' @export

scale_color_datastory <- function(...,
                             palette = "default",
                             repeat_col = TRUE,
                             reverse = FALSE,
                             type = "qual",
                             grad_col = "blue",
                             aesthetics = "color") {

  # Get the list of arguments passed by the user (used for some checks)
  c_args <- as.list(match.call())

  chk::chk_string(type)
  chk::chk_subset(type, c("qual", "seq", "cont", "div"))
  chk::chk_string(grad_col)
  chk::chk_subset(grad_col, c("blue", "red", "green", "yellow", "violet"))

  # Inform the user that 'palette' and 'repeat_col are ignored when 'type' is
  # not set to "seq".
  if (!(grad_col %in% c("blue", "red"))  && type != "seq") {
    cli::cli_abort(
      paste0(
        "The colour {grad_col} can only be used for sequential scales (i.e. ",
        "when `type = \"seq\"`)."
      )
    )
  }

  # Inform the user that 'grad_col' is ignored when 'type = "seq"'
  if ("grad_col" %in% names(c_args) && type %in% c("qual", "div")) {
    cli::cli_inform(
      c(
        i = paste0(
          "The argument `grad_col` is not used when the scale is ",
          "qualitative or diverging. Value passed to that argument was thus ",
          "ignored."
        )
      )
    )
  }

  # Inform the user that 'palette' and 'repeat_col are ignored when 'type' is
  # not set to "seq".
  if (any(c("palette", "repeat_col") %in% names(c_args))  && type != "qual") {
    cli::cli_inform(
      c(
        i = paste0(
          "The argument `palette` and `repeat_col` are not used when the ",
          "scale is not qualitative. Any value passed to these arguments was ",
          "thus ignored."
        )
      )
    )
  }

  res_scale <- switch(
    type,
    qual = {
      if (utils::packageVersion("ggplot2") >= "3.5.0")
        ggplot2::discrete_scale(
          aesthetics,
          palette = datastory_pal(palette, repeat_col, reverse),
          ...
        )
      else
        ggplot2::discrete_scale(
          aesthetics,
          "datastory_scheme_qual",
          palette = datastory_pal(palette, repeat_col, reverse),
          ...
        )
    },
    cont = {
      low <- "#ececec"
      high <- switch(
        grad_col,
        blue = datastory_dark_blues[1L],
        red = datastory_dark_reds[1L]
      )
      ggplot2::scale_fill_gradient(
        low = ifelse(reverse, high, low),
        high = ifelse(reverse, low, high),
        aesthetics = aesthetics,
        ...
      )
    },
    div = ggplot2::scale_fill_gradient2(
      low = ifelse(reverse, datastory_dark_reds[1L], datastory_dark_blues[1L]),
      high = ifelse(reverse, datastory_dark_blues[1L], datastory_dark_reds[1L]),
      mid = "#ececec",
      aesthetics = aesthetics,
      ...
    ),
    seq = {
      seq_cols <- c(
        "#ececec",
        switch(
          grad_col,
          blue = datastory_dark_blues[1L],
          red = datastory_dark_reds[1L],
          green = datastory_green[1L],
          yellow = datastory_yellow[1L],
          violet = datastory_violet[1L]
        )
      )
      if (reverse) seq_cols <- rev(seq_cols)
      if (utils::packageVersion("ggplot2") >= "3.5.0")
        ggplot2::discrete_scale(
          aesthetics,
          palette = grDevices::colorRampPalette(seq_cols),
          ...
        )
      else
        ggplot2::discrete_scale(
          aesthetics,
          "datastory_scheme_seq",
          palette = grDevices::colorRampPalette(seq_cols),
          ...
        )
    }
  )
  return(res_scale)
}

#' @rdname scale_fill_datastory
#'
#' @aliases scale_color_datastory
#'
#' @export

scale_colour_datastory <- scale_color_datastory

#' @param guide Argument used to silence by default the legend when plotting a
#' single colour.
#'
#' @rdname scale_fill_datastory
#'
#' @examples
#' library(ggplot2)
#'
#' mpg |>
#' ggplot() +
#'   aes(y = class, fill = "") +
#'   geom_bar() +
#'   scale_fill_datastory_1() +
#'   get_datastory_theme()
#'
#' @export

scale_fill_datastory_1 <- function(..., guide = NULL) {
  ggplot2::scale_fill_manual(values = datastory_single, guide = guide, ...) # nolint: return_linter
}

#' @param guide Argument used to silence by default the legend when plotting a
#' single colour.
#'
#' @rdname scale_fill_datastory
#'
#' @examples
#' library(ggplot2)
#'
#' mpg |>
#' ggplot() +
#'   aes(y = class, x = displ, color = "") +
#'   geom_point() +
#'   scale_color_datastory_1() +
#'   get_datastory_theme()
#'
#' @export

scale_color_datastory_1 <- function(..., guide = NULL) {
  ggplot2::scale_color_manual(values = datastory_single, guide = guide, ...) # nolint: return_linter
}

#' Modify x-axis scale to render nicely facets as bars
#'
#' @returns An object of class `<ggproto>`.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(y = class, fill = class)) +
#'   geom_bar() +
#'   scale_fill_datastory(guide = NULL) +
#'   # You need to modify the x-axis with the function below for nice alignment
#'   scale_x_facet_as_hbar() +
#'   facet_as_hbar(~class) +
#'   get_datastory_theme(
#'     grid_lines = NULL,
#'     font_size = "extra_large",
#'     # Make sure to indicate the theme you are using facet as bars
#'     facet_as_hbar = TRUE
#'   )
