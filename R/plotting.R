#' (experimental) Get the ggplot2 theme for data story plots
#'
#' The data plot theme is specially tailored for the clean look of the data
#' stories. By default, no axis titles and no grid lines are included, they can
#' be added when needed.
#' @param legend_position TODO
#' @param legend_margin TODO
#' @param gridline_axis TODO
#' @param title_axis TODO
#' @param text_axis TODO
#' @param tick_axis TODO
#' @param remove_plot_margin TODO
#' @param legend_key_size TODO
#' @export
#' @examples
#'  # TODO
get_datastory_theme <- function(legend_position = "top",
                                legend_margin = margin(0, 0, 0, 0),
                                gridline_axis = c(),
                                title_axis = c(),
                                text_axis = c("x", "y"),
                                tick_axis = c(),
                                remove_plot_margin = FALSE,
                                legend_key_size = c()) {
  ds_theme <- theme(
    text = element_text(color = "#22211d", family = "Source Sans Pro"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8,
                               margin = margin(l = 2, r = 2, unit = "mm")),
    legend.position = legend_position,
    legend.justification = "left",
    legend.margin = legend_margin,
    legend.spacing.x = unit(0, "mm"),
    legend.key = element_blank(),
    plot.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 6, hjust =  0.5, face = "bold"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 9, margin = margin(
      t = 10,
      r = 0,
      b = 5,
      l = 0
    )),
    axis.title.y = element_text(size = 9, margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 5
    )),
    # axis.text.x = element_text(size = 7, color = "#4F4F4F"),
    # axis.text.y = element_blank(),
    strip.text = element_text(size = 8, face = "bold"),
    strip.background = element_rect(fill = NA, color = NA)
  )

  if (length(legend_key_size) > 0) {
    if (length(legend_key_size) == 2) {
      ds_theme <- ds_theme +
        theme(
          legend.key.width = unit(legend_key_size[1], "mm"),
          legend.key.height = unit(legend_key_size[2], "mm")
        )
    } else {
      warning("legend_key_size vector has not length 2 (width, height in mm)")
    }
  }

  if (remove_plot_margin) {
    ds_theme <- ds_theme +
      theme(plot.margin = margin(0, 0, 0, 0, "mm"))
  }

  # Add gridlines according to parameters
  if ("x" %in% gridline_axis) {
    ds_theme <- ds_theme +
      theme(panel.grid.major.x = element_line(
        color = "#D3D3D3",
        size = 0.2,
        linetype = "longdash"
      ))
  } else {
    theme <- theme(panel.grid.major.x = element_blank())
  }
  if ("y" %in% gridline_axis) {
    ds_theme <- ds_theme +
      theme(panel.grid.major.y = element_line(
        color = "#D3D3D3",
        size = 0.2,
        linetype = "longdash"
      ))
  } else {
    ds_theme <- ds_theme + theme(panel.grid.major.y = element_blank())
  }

  # Remove axis titles according to parameters
  if (!("x" %in% title_axis)) {
    ds_theme <- ds_theme + theme(axis.title.x = element_blank())
  }
  if (!("y" %in% title_axis)) {
    ds_theme <- ds_theme + theme(axis.title.y = element_blank())
  }

  # Add axis text according to parameters
  if ("x" %in% text_axis) {
    ds_theme <- ds_theme +
      theme(axis.text.x = element_text(size = 8, color = "#4F4F4F"))
  } else {
    ds_theme <- ds_theme + theme(axis.text.x = element_blank())
  }
  if ("y" %in% text_axis) {
    ds_theme <- ds_theme +
      theme(axis.text.y = element_text(size = 8, color = "#4F4F4F"))
  } else {
    ds_theme <- ds_theme + theme(axis.text.y = element_blank())
  }

  # Add ticks according to parameters
  if ("x" %in% tick_axis) {
    ds_theme <- ds_theme +
      theme(axis.ticks.x = element_line(color = "#d3d3d3", size = 0.3))
  } else {
    ds_theme <- ds_theme + theme(axis.ticks.x = element_blank())
  }
  if ("y" %in% tick_axis) {
    ds_theme <- ds_theme +
      theme(axis.ticks.y = element_line(color = "#d3d3d3", size = 0.3))
  } else {
    ds_theme <- ds_theme + theme(axis.ticks.y = element_blank())
  }
  ds_theme
}

#' (experimental) The datastory SNSF scheme (qualitative)
#'
#' The iterativ interpretation of the YAAY scheme for the data stories.
#' @export
#' @examples
#'  # TODO
datastory_scheme_qualitative <- c("#4159AC", "#4DD898", "#EFC900", "#406AF5")

#' (experimental) The datastory SNSF scheme (blue seq)
#'
#' The iterativ interpretation of the YAAY scheme for the data stories.
#' @export
#' @examples
#'  # TODO
datastory_scheme_blue_seq <- c("#00349E", "#406AF5", "#90AAFF", "#CCD8FF")

#' (experimental) The datastory SNSF scheme (green seq)
#'
#'
#' The iterativ interpretation of the YAAY scheme for the data stories.
#' @export
#' @examples
#'  # TODO
datastory_scheme_green_seq <- c("#00cf85", "#31d79f", "#62e0ba", "#93e8d4")

#' (experimental) The datastory SNSF scheme (yellow seq)
#'
#' The iterativ interpretation of the YAAY scheme for the data stories.
#' @export
#' @examples
#'  # TODO
datastory_scheme_yellow_seq <- c("#efc900", "#f3d740", "#f7e480", "#fbf2bf")

#' (experimental) The datastory SNSF scheme (gray seq)
#'
#' The iterativ interpretation of the YAAY scheme for the data stories.
#'
#' @export
#' @examples
#'  # TODO
datastory_scheme_gray_seq <- c("#4F4F4F", "#7E7E7E", "#AFAFAF", "#F0F0F0")

#' (experimental) Get your datastory SNSF scheme
#'
#' palettes : default (qualitative), qualitative, blue_seq, green_seq,
#' yellow_seq, gray_seq.
#'
#' @export
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
    colors <- datastory_scheme_all
  }

  # Interpolate colors if not enough are available
  if (!is.null(n_col)) {
    if (n_col > length(colors))
      colors <- colorRampPalette(colors)(n_col)
    return(colors[1:n_col])
  } else
    return(colors)
}

#' Make text bold for plotly with HTML
#'
#' \code{plotly::ggplotly()} is convenient but does not support every aspect of
#' a ggplot plot when converting it into a plotly plot. One of these aspects is
#' the \code{fontface} parameter of \code{geom_text}. The font face can still be
#' controlled for plotly output when wrapping HTML tags around the actual text
#' in \code{geom_text}, this function does this when the wished output string
#' should be formatted for plotly.
#' @param x Character string to be formatted
#' @param is_plotly Whether to apply the formatting or not
#' @param make_bold Bold or not?
#' @param make_italic Italic or not?
#' @export
#' @examples
#'  format_when_plotly("hallo!", is_plotly = TRUE, make_bold = TRUE)
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

#' Get ggiraph tooltip CSS
#'
#' Get uniform tooltip CSS style for ggiraph graphs.
#' @export
#' @examples
#'  ggiraph::girafe(
#'  ggobj = ggplot(mtcars),
#'  options = list(
#'    ggiraph::opts_tooltip(
#'      css = get_ggiraph_tooltip_css()
#'    )
#'  ))
get_ggiraph_tooltip_css <- function() {
  paste0("font-family:'Source Sans Pro';background-color:black;",
         "font-size: 0.8em;padding: 0.5em;color:white;")
}