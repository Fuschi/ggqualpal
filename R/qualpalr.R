#' Qualitative palettes via qualpalr (helper)
#'
#' @description
#' Internal helper that returns a palette function for `ggplot2::discrete_scale()`.
#' The returned function takes a single integer `n` (the number of required
#' discrete colours) and calls [qualpalr::qualpal()] to generate a qualitative
#' palette of `n` colours. Any extra arguments in `...` are forwarded to
#' `qualpalr::qualpal()`, e.g. `colorspace`, `cvd`, `bg`, `metric`, `extend`, etc.
#'
#' @param direction Integer `1` (default) to keep the original order or `-1` to
#'   reverse the generated palette.
#' @param n_max Optional integer. If not `NULL`, it caps the requested `n` to
#'   `min(n, n_max)`. This can be helpful to keep colour assignments stable across
#'   plots with varying numbers of levels.
#' @param ... Additional arguments forwarded to [qualpalr::qualpal()].
#'
#' @return A function `f(n)` returning a character vector of length `n`
#'   with HEX colours.
#' @keywords internal
pal_qualpal <- function(direction = 1L, n_max = NULL, ...) {
  if (!direction %in% c(1L, -1L)) {
    stop("`direction` must be either 1 or -1.", call. = FALSE)
  }
  args <- list(...)
  function(n) {
    if (!is.null(n_max)) {
      if (!is.numeric(n_max) || length(n_max) != 1L || is.na(n_max) || n_max < 1) {
        stop("`n_max` must be a single positive integer or NULL.", call. = FALSE)
      }
      n <- min(n, as.integer(n_max))
    }
    pal <- tryCatch(
      do.call(qualpalr::qualpal, c(list(n = n), args)),
      error = function(e) {
        stop("qualpalr::qualpal() failed: ", conditionMessage(e), call. = FALSE)
      }
    )
    
    # qualpal() typically returns a list with a $hex vector.
    if (is.list(pal) && !is.null(pal$hex)) {
      cols <- unname(pal$hex)
    } else if (is.character(pal)) {
      # Some fallbacks/versions may return a character vector of hex colours
      cols <- unname(pal)
    } else {
      stop(
        "qualpalr::qualpal() returned an unrecognized format. ",
        "Expected an object with `$hex` or a character vector.",
        call. = FALSE
      )
    }
    
    if (length(cols) < n) {
      stop(
        "qualpalr returned fewer colours (", length(cols),
        ") than requested (", n, ").",
        call. = FALSE
      )
    }
    
    cols <- cols[seq_len(n)]
    if (direction == -1L) cols <- rev(cols)
    cols
  }
}

#' Discrete colour/fill scales using qualpalr
#'
#' @description
#' Discrete `ggplot2` scales powered by [qualpalr::qualpal()], ideal for
#' **qualitative** palettes. You can pass any argument accepted by `qualpal()`
#' (e.g., `colorspace`, `cvd`, `bg`, `metric`, `extend`).
#'
#' @details
#' These scales are direct wrappers around `ggplot2::discrete_scale()`. Internally,
#' they provide a palette function (see [pal_qualpal()]) that ggplot2 calls with
#' the required number of levels (`n`).
#'
#' Common `qualpal()` arguments:
#' - `colorspace`: HSL/LCH ranges as a list, a `"Source:Palette"` string, or an RGB matrix/data frame.
#' - `cvd`: named numeric vector `c(protan = ..., deutan = ..., tritan = ...)` in `[0, 1]`.
#' - `bg`: a background colour that should be avoided (hex or any valid R colour).
#' - `metric`: one of `"ciede2000"`, `"din99d"`, `"cie76"`.
#' - `extend`: a fixed set of initial colours (hex or RGB matrix/data frame).
#'
#' @param name Aesthetic name used by the scale and legend; defaults to
#'   [ggplot2::waiver()], meaning it will be inferred.
#' @param ... Additional arguments forwarded to [qualpalr::qualpal()], and/or
#'   accepted by [ggplot2::discrete_scale()] (e.g. `limits`, `breaks`, `labels`,
#'   `drop`, `na.translate`, `guide`).
#' @param direction Integer `1` to keep the order (default) or `-1` to reverse it.
#' @param n_max Optional integer to cap the number of colours requested from
#'   `qualpal()`. If `NULL` (default), no cap is applied.
#' @param aesthetics The ggplot2 aesthetics this scale works with. Usually
#'   `"colour"` (or `"color"`) or `"fill"`.
#'
#' @return A `ggplot2` scale object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   # Basic usage (colour)
#'   ggplot(mtcars, aes(factor(cyl), mpg, colour = factor(cyl))) +
#'     geom_point(size = 3) +
#'     scale_color_qualpal()
#'
#'   # Basic usage (fill)
#'   ggplot(mpg, aes(class, fill = class)) +
#'     geom_bar() +
#'     scale_fill_qualpal()
#'
#'   # Reverse order
#'   ggplot(mpg, aes(class, fill = class)) +
#'     geom_bar() +
#'     scale_fill_qualpal(direction = -1)
#'
#'   # Forward qualpal() arguments: custom HSL ranges + metric
#'   ggplot(mpg, aes(class, colour = class)) +
#'     geom_point(aes(y = displ), position = position_jitter(width = .2)) +
#'     scale_color_qualpal(
#'       colorspace = list(h = c(0, 360), s = c(0.3, 0.7), l = c(0.45, 0.85)),
#'       metric = "ciede2000"
#'     )
#'
#'   # Simulate partial protan deficiency and avoid white background
#'   ggplot(mpg, aes(class, fill = class)) +
#'     geom_bar() +
#'     scale_fill_qualpal(
#'       cvd = c(protan = 0.4, deutan = 0, tritan = 0),
#'       bg = "white"
#'     )
#'
#'   # Extend with fixed starting colours
#'   ggplot(mpg, aes(class, fill = class)) +
#'     geom_bar() +
#'     scale_fill_qualpal(extend = c("#2D6A4F", "#E63946"))
#' }
#' @name scale_qualpal
NULL

#' @export
#' @rdname scale_qualpal
scale_color_qualpal <- function(name = ggplot2::waiver(), ...,
                                direction = 1L,
                                n_max = NULL,
                                aesthetics = "colour") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    name = name,
    palette = pal_qualpal(direction = direction, n_max = n_max, ...),
    ...
  )
}

#' British spelling alias for colour scale
#' @export
#' @rdname scale_qualpal
scale_colour_qualpal <- function(name = ggplot2::waiver(), ...,
                                 direction = 1L,
                                 n_max = NULL,
                                 aesthetics = "colour") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    name = name,
    palette = pal_qualpal(direction = direction, n_max = n_max, ...),
    ...
  )
}

#' @export
#' @rdname scale_qualpal
scale_fill_qualpal <- function(name = ggplot2::waiver(), ...,
                               direction = 1L,
                               n_max = NULL,
                               aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    name = name,
    palette = pal_qualpal(direction = direction, n_max = n_max, ...),
    ...
  )
}