#' Create a thumbnail for a data story
#'
#' Each data story has a thumbnail on the SNSF data portal data story main page.
#' This function automatize the creation a thumbnail which represent a figure
#' selected from the data story with a perspective effect and some blurring on
#' the edges. This functions is however not magic and will probably not generate
#' the best image by default for any picture. Each case is likely to require
#' some fine tuning, but default values are a good starting point.
#' @param img_path The path to the screenshot used to generate the thumbnail
#' (only PNG).
#' @param blur_steps Determine the smoothness of the blurring process on the
#' edges of the thumbnail (large values will requires more computing and then
#' will take more time. and values lower than 10 are likely to produce a not so
#' smooth result).
#' @param max_blur The intensity of the blur. The blur decreases as it moves
#' away from the edges.
#' @param width_blur The proportion of the image width that will be blurred.
#' For example, 0.40 means that 40% of the width (20% each side) will be
#' blurred.
#' @param height_blur The proportion of the image height that will be blurred.
#' For example, 0.40 means that 40% of the height (20% at the top and 20% at the
#' bottom) will be blurred.
#' @param width_crop The proportion of the image width that is saved when
#' cropping, after adding a perspective effect. For example, 0.60 means that 60%
#' of the image width is saved (20% of the image is removed on each side).
#' @param height_crop The proportion of the image height that is saved when
#' cropping, after adding a perspective effect. For example, 0.60 means that 60%
#' of the image height is saved (20% of the image is at the top and 20% at the
#' bottom).
#' @param legend_key_size ggplot legend.key.width and legend.key.height theme
#' item: vector with first element width (mm) and second element height (mm).
#' @keywords internal

create_ds_thumbnail <- function(img_path,
                                blur_steps = 15,
                                max_blur = 4,
                                width_blur = 0.40,
                                height_blur = 0.34,
                                width_crop = 0.58,
                                height_crop = 0.62) {

  if (!grepl("\\.png$", img_path)) {
    stop("'img_path' must point to a png file or the .png extension is missing")
  }

  if (blur_steps < 10) {
    warning(
      paste0(
        "When setting 'blur_steps' lower than 10, the output is likely to not ",
        "be as smooth as expected."),
      immediate. = TRUE
    )
  }

  if (blur_steps > 50) {
    warning(
      paste0(
        "Large values of 'blur_steps' require more computing and will slower ",
        "your code."),
      immediate. = TRUE
    )
  }

  img <- magick::image_read(img_path)

  # Retrieves the width of 'img'
  w <- dim(magick::image_data(img))[2]
  # Retrieves the height of 'img'
  h <- dim(magick::image_data(img))[3]
  # The level of blurring (the higher it is the smoothest will be the blurring,
  # but also the longest will be the process).
  sig <- seq(max_blur, 0, length.out = blur_steps)

  # The width of the blurring. For example, you set 'width_blur' to 0.32, it
  # involves that the 32% on the left and the 32% on the right of the image will
  # be blurred.
  n_w <- round(w * seq(0, width_blur, length.out = blur_steps))
  # The height of the blurring. For example, you set 'height_blur' to 0.32, it
  # involves that the 32% on the top and the 32% on the bottom of the image will
  # be blurred.
  n_h <- round(h * seq(0, height_blur, length.out = blur_steps))


  # In order to obtain a smooth gradient blur on the edges of the image, the
  # process require to 1) blur the original image, 2) then crop a little bit the
  # blurred image, then 3) combine the blurred image with the previous image. By
  # repeating this, you will obtain a smooth gradient blur on the edges.
  for (i in 1:blur_steps) {

    # Blur the original image with the blurring level for the corresponding step
    temp_blur <-
      img |>
      magick::image_blur(0, sig[i])

    # If it is the first step, no cropping involved
    if (i == 1) {

      cum_blur <- temp_blur

    } else {

      # After the first step, the original is cropped to based on width an
      # height values for the corresponding step.
      temp_blur_crop <-
        temp_blur |>
        magick::image_crop(
          geometry =
            magick::geometry_area(
              width = w - (n_w[i] * 2),
              height = h - (n_h[i] * 2),
              x_off = n_w[i],
              y_off = n_h[i]
            )
        )

      # After the first step, the newly blurred and cropped image is combine
      # with the previous ones.
      cum_blur <-
        magick::image_composite(
          cum_blur,
          temp_blur_crop,
          offset =
            paste0(
              "+", n_w[i],
              "+", n_h[i]
            )
        )
    }

  }

  # Take the image with the smooth gradient blur and distort it to create a
  # perspective effect, and then rotate it.
  final_img <-
    cum_blur |>
    magick::image_distort(
      'perspective',
      c(
        0, 0,         # Source Top Left
        -w * 0.15, 0, # Destination Top Left
        0, h,         # Source Bottom Left
        w * 0.15, h,  # Destination Bottom Left
        w, 0,         # Source Bottom Right
        w * 0.85, 0,  # Destination Bottom Right
        w, h,         # Source Top Right
        w * 1.15, h   # Destination Top Right
      )
    ) |>
    magick::image_rotate(-10) |>
    magick::image_crop(
      geometry =
        magick::geometry_area(
          width = w * width_crop,
          height = h * height_crop,
          x_off = w * 0.175,
          y_off = h * 0.17)
    )

  return(final_img)

}
