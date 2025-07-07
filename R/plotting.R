#' @title Get the ggplot2 theme for data story plots
#'
#' @description The data plot theme is specially tailored for the clean look of
#' the data stories. By default, no axis titles and no grid lines are included,
#' they can be added when needed.
#'
#' @param legend_position Choose the position of the legend in regards to your
#' plot.
#' @param legend_margin ggplot \code{legend.margin} theme element.
#' @param gridline_axis TODO
#' @param title_axis Which axis titles should be shown, default X and Y:
#' \code{c("x", "y")}
#' @param text_axis Which axis text should be shown, default X and Y:
#' \code{c("x", "y")}
#' @param tick_axis On which axis the axis ticks should be displayed.
#' @param remove_plot_margin Whether all margin around the plot should be
#' removed. Default \code{FALSE}.
#' @param legend_key_size ggplot legend.key.width and legend.key.height theme
#' item: vector with first element width (mm) and second element height (mm).
#' @param family the name of the font to use to display text. Theinhardt is
#' used as default.
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#'  # TODO
get_datastory_theme <- function(legend_position = "top",
                                legend_margin = ggplot2::margin(0, 0, 0, 0),
                                gridline_axis = c(),
                                title_axis = c(),
                                text_axis = c("x", "y"),
                                tick_axis = c(),
                                remove_plot_margin = FALSE,
                                legend_key_size = c(),
                                family = "Theinhardt",
                                facet_as_hbar = FALSE) {
  # Arguments called by the user
  called_args <- as.list(match.call())

  ds_theme <- ggplot2::theme(
    text = ggplot2::element_text(color = "#22211d", family = family),
    legend.title = ggplot2::element_blank(),
    legend.text =
      ggplot2::element_text(
        size = 8,
        margin = ggplot2::margin(l = 2, r = 2, unit = "mm")
      ),
    legend.position = legend_position,
    legend.justification = "left",
    legend.margin = legend_margin,
    legend.spacing.x = ggplot2::unit(0, "mm"),
    legend.key = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", size = 10),
    plot.subtitle = ggplot2::element_text(size = 8),
    plot.caption = ggplot2::element_text(size = 6, hjust =  0.5, face = "bold"),
    panel.background = ggplot2::element_blank(),
    axis.title.x =
      ggplot2::element_text(
        size = 9,
        margin =
          ggplot2::margin(
            t = 10,
            r = 0,
            b = 5,
            l = 0
          )
      ),
    axis.title.y =
      ggplot2::element_text(
        size = 9,
        margin =
          ggplot2::margin(
            t = 0,
            r = 10,
            b = 0,
            l = 5
          )
      ),
    strip.text = ggplot2::element_text(size = 8, face = "bold"),
    strip.background = ggplot2::element_rect(fill = NA, color = NA)
  )

  # Inform that x "text_axis" is ignored when facet_as_hbar = TRUE
  if (
    "y" %in% text_axis && facet_as_hbar && ("text_axis" %in% names(called_args))
  )
    cli::cli_inform(
      c(
        paste0(
          "Including the y-axis in {.var text_axis} is ignored when ",
          "{.code facet_as_hbar = TRUE}."
        ),
        i = paste0(
          "To silence this message, remove the call to {.var text_axis} ",
          "or set {.code text_axis = NULL}."
        )
      )
    )

  if (facet_as_hbar) {
    ds_theme <- ds_theme %+replace% ggplot2::theme(
      strip.text.x = ggplot2::element_text(
        face = "plain",
        hjust = 0,
        vjust = 0,
        margin = ggplot2::margin(t = 0, r = 0, b = 0.5, unit = "lines")
      )
    )
  }

  if (length(legend_key_size) > 0) {
    if (length(legend_key_size) == 2) {
      ds_theme <- ds_theme %+replace%
        ggplot2::theme(
          legend.key.width = ggplot2::unit(legend_key_size[1], "mm"),
          legend.key.height = ggplot2::unit(legend_key_size[2], "mm")
        )
    } else {
      warning("legend_key_size vector has not length 2 (width, height in mm)")
    }
  }

  if (remove_plot_margin) {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "mm"))
  }

  # Add gridlines according to parameters
  if ("x" %in% gridline_axis) {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(
        panel.grid.major.x =
          ggplot2::element_line(
            color = "#AFAFAF",
            size = 0.2,
            linetype = "longdash"
          )
      )
  } else {
    theme <- ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }
  if ("y" %in% gridline_axis) {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(
        color = "#AFAFAF",
        size = 0.2,
        linetype = "longdash"
      ))
  } else {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }

  # Remove axis titles according to parameters
  if (!("x" %in% title_axis)) {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }
  if (!("y" %in% title_axis)) {
    ds_theme <- ds_theme %+replace%
      theme(axis.title.y = ggplot2::element_blank())
  }

  # Add axis text according to parameters
  if ("x" %in% text_axis) {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 8, color = "#4F4F4F")
      )
  } else {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
  }

  if (facet_as_hbar) {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }
  else if ("y" %in% text_axis) {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8, color = "#4F4F4F")
      )
  } else {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

  # Add ticks according to parameters
  if ("x" %in% tick_axis) {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_line(color = "#AFAFAF", size = 0.3)
      )
  } else {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
  }
  if ("y" %in% tick_axis) {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(
        axis.ticks.y = ggplot2::element_line(color = "#AFAFAF", size = 0.3)
      )
  } else {
    ds_theme <- ds_theme %+replace%
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  }
  return(ds_theme)
}

### Specific SNSF colors with different brightness levels (where available)

## Primary colors (100 percent, 70 percent, 30 percent, 10 percent)

#' @title "SNF Schwarzblau" primary color (100\%)
#'
#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_black)

snsf_black <- "#04293C"

#' @title "SNF Dunkelblau" colors (100\%, 70\%, 30\%, 10\%)
#'
#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_dark_blues)

snsf_dark_blues <- c("#5298BD", "#86B7D1", "#CBE0EB", "#EDF4F8")

#' @title "SNF Blau" colors (100\%, 70\%, 30\%, 10\%)
#'
#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_blues)

snsf_blues <- c("#83D0F5", "#A8DEF8", "#DAF1FC", "#F2FAFE")

#' @title "SNF Dunkelrot" colors (100\%, 70\%, 30\%, 10\%)
#'
#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_dark_reds)

snsf_dark_reds <- c("#C95B40", "#D98C79", "#EFCDC5", "#F9EEEC")

#' @title "SNF Rot" colors (100\%, 70\%, 30\%, 10\%)

#' @description Primary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_reds)

snsf_reds <- c("#F08262", "#F4A791", "#FAD9D0", "#FDF2EF")

## Secondary colors (100 percent, 50 percent)

#' @title "SNF Gelb" colors (100\%, 50\%)
#'
#' @description Secondary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_yellow)

snsf_yellow <- c("#FBBE5E", "#FDDEAE")

#' @title "SNF Grün" colors (100\%, 50\%)
#'
#' @description Secondary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_green)

snsf_green <- c("#71B294", "#B8D8C9")

#' @title SNF Violett" colors (100\%, 50\%)
#'
#' @description Secondary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_violet)

snsf_violet <- c("#9D90B9", "#CEC7DC")

#' @title "SNF Grau" colors (100\%, 50\%)
#'
#' @description Secondary colors CD 2021+
#'
#' @export
#'
#' @examples
#'  print(snsf_grays)

snsf_grays <- c("#B2B1A7", "D8D8D3")

#' @title The default datastory SNSF scheme (qualitative)
#'
#' @description Colors from the new Corporate Design of 2021.
#'
#' @export
#'
#' @examples
#'  # TODO

datastory_scheme_qualitative <- c(
  "#5298BD", # SNF Dunkelblau 100 percent
  # "#83D0F5", # SNF Blau 100 percent
  "#FBBE5E", # SNF Gelb 100 percent
  "#71B294", # SNF Grün 100 percent
  "#9D90B9", # SNF Violett 100 percent
  "#F08262", # SNF Rot 100 percent
  "#83D0F5", # SNF Blau 100 percent
  # Lighter colors:
  # "#86B7D1", # SNF Dunkelblau 70 percent
  "#FDDEAE", # SNF Gelb 50 percent
  "#B8D8C9", # SNF Grün 50 percent
  "#CEC7DC", # SNF Violett 50 percent
  "#F4A791" # SNF Rot 70 percent
  # "#A8DEF8" # SNF Blau 70 percent
)


#' @title SNSF blues (CD 2021)
#'
#' @description Colors from the new Corporate Design of 2021.
#'
#' @export
#'
#' @examples
#'  # TODO

datastory_scheme_blue_seq <-
  c("#5298BD", # SNF Dunkelblau 100 percent
    "#86B7D1", # SNF Dunkelblau 70 percent
    "#83D0F5", # SNF Blau 100 percent
    "#A8DEF8") # SNF Blau 70 percent

# c("#00349E", "#406AF5", "#90AAFF", "#CCD8FF")

#' @title SNSF greens (CD 2021)
#'
#' @description Colors from the new Corporate Design of 2021.
#'
#' @export
#'
#' @examples
#'  # TODO

datastory_scheme_green_seq <- snsf_green

# c("#00cf85", "#31d79f", "#62e0ba", "#93e8d4")

#' @title SNSF yellows (CD 2021)
#'
#' @export
#'
#' @examples
#'  # TODO

datastory_scheme_yellow_seq <- snsf_yellow

# c("#efc900", "#f3d740", "#f7e480", "#fbf2bf")

#' @title SNSF grays (CD 2021)
#'
#' @export
#'
#' @examples
#'  # TODO

datastory_scheme_gray_seq <- snsf_grays

#' @title Get your datastory SNSF scheme
#'
#' @description palettes : default (qualitative), qualitative, blue_seq,
#' green_seq, yellow_seq, gray_seq.
#'
#' @param palette Color palette to choose. Available: "default" (qualitative),
#' "qualitative", "blue_seq", "green_seq", "yellow_seq", "gray_seq"
#' @param n_col The number of colors to return. If it exceeds the number of
#' colors in the chosen palette, new colors are interpolated. Do not set this
#' argument if you want to get the original number of colors in the chosen
#' palette.
#'
#' @export
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#'  # TODO

get_datastory_scheme <- function(palette = "default", n_col = NULL) {
  # Choose the right color palette
  colors <- switch(palette,
                   default = datastory_scheme_qualitative,
                   qualitative = datastory_scheme_qualitative,
                   blue_seq = datastory_scheme_blue_seq,
                   green_seq = datastory_scheme_green_seq,
                   yellow_seq = datastory_scheme_yellow_seq,
                   gray_seq = datastory_scheme_gray_seq)

  if (length(colors) == 0) {
    warning(paste0("Palette '", palette, "' not found"))
    colors <- datastory_scheme_qualitative
  }

  # Interpolate colors if not enough are available
  if (!is.null(n_col)) {
    if (n_col > length(colors))
      colors <- grDevices::colorRampPalette(colors)(n_col)
    return(colors[1:n_col])
  } else
    return(colors)
}

#' @title Make text bold for plotly with HTML
#'
#' @description \code{plotly::ggplotly()} is convenient but does not support
#' every aspect of a ggplot plot when converting it into a plotly plot. One of
#' these aspects is the \code{fontface} parameter of \code{geom_text}. The font
#' face can still be controlled for plotly output when wrapping HTML tags around
#' the actual text in \code{geom_text}, this function does this when the wished
#' output string should be formatted for plotly.
#'
#' @param x Character string to be formatted
#' @param is_plotly Whether to apply the formatting or not
#' @param make_bold Bold or not?
#' @param make_italic Italic or not?
#'
#' @export
#'
#' @examples
#'  format_when_plotly("hallo!", is_plotly = TRUE, make_bold = TRUE)
#'
format_when_plotly <- function(x, is_plotly = TRUE,
                               make_bold = TRUE,
                               make_italic = FALSE) {
  if (is_plotly) {
    if (make_italic)
      x <- paste0("<i>", x, "</i>")
    if (make_bold)
      x <- paste0("<b>", x, "</b>")
    x
  } else {
    x
  }
}

#' @title Get ggiraph tooltip CSS
#'
#' @description Get uniform tooltip CSS style for ggiraph graphs.
#'
#' @param family the name of the font to use to display text. Theinhardt is
#' used as default.
#'
#' @export

get_ggiraph_tooltip_css <- function(family = "Theinhardt") {
  paste0("font-family:'", family, "';background-color:black;",
         "font-size: 0.8em;padding: 0.5em;color:white;")
}
