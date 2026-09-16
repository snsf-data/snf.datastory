# snf.datastory 0.1.4 (2025-07-07)

- Remove unnecessary dependency to `showtext`
- R >= 4.1.0 is now required
- `ggplot2` >= 3.4.0 is now required
- When creating the ggplot Data Story theme, the argument `size` has been replaced with `linewidth` when calling `ggplot2::element_line()`
- Fix a problem preventing from properly detecting properly when the "Theinhardt" font is available

# snf.datastory 0.1.4 (2025-07-07)

- Update list of research domains (research areas)
- Small updates of the color schemes
- Add new `scale_fill_datastory()` and `scale_color_datastory()` functions to create ggplot2 scales based on data story color schemes
- New argument in the data story theme: `facet_as_hbar` allows to format the theme when facets are used as horizontal bars
- New function `facet_as_hbar()` which turns facets as horizontal bars (+ `scale_x_facet_as_hbar()` which sets the x axis)
- Enforce the use of "sans" as default ggplot2 font family when "Theinhardt" is not available
- Update the the number printing convention in `print_num()`


