# =============================================================================
# Internal helpers
# =============================================================================


#' Resolve the final level set for a discrete vector
#'
#' @description
#' Resolve the categories used to build the palette.
#'
#' Resolution follows this order:
#'
#' - if `levels` is supplied, it is returned after validation;
#' - otherwise, factor levels are used for factors;
#' - if `drop = TRUE`, unused factor levels are removed;
#' - otherwise, the unique observed non-missing values of `x` are used in order
#'   of appearance.
#'
#' When `levels` is provided, it must include all non-missing categories
#' observed in `x`.
#'
#' @param x A vector treated as discrete. It can be a character vector, factor,
#'   or any other vector that can be converted to character labels.
#' @param levels Optional character vector specifying the complete set and order
#'   of levels to be used in the palette.
#' @param drop Logical; only relevant when `levels = NULL` and `x` is a factor.
#'   If `TRUE`, unused factor levels are dropped. If `FALSE`, all factor levels
#'   are kept.
#'
#' @return
#' A character vector containing the resolved level set.
#'
#' @keywords internal
.resolve_levels <- function(x, levels = NULL, drop = FALSE) {
  obs <- unique(as.character(x[!is.na(x)]))
  
  if (!is.null(levels)) {
    lv <- as.character(levels)
    
    missing_in_levels <- setdiff(obs, lv)
    if (length(missing_in_levels) > 0L) {
      cli::cli_abort(c(
        "{.arg levels} must include all categories present in {.arg x}.",
        "x" = "Missing in {.arg levels}: {missing_in_levels}"
      ))
    }
    
    return(lv)
  }
  
  if (is.factor(x)) {
    lv <- base::levels(x)
    
    if (isTRUE(drop)) {
      lv <- lv[lv %in% obs]
    }
    
    return(lv)
  }
  
  obs
}


#' Validate named colour assignments
#'
#' @description
#' Validate named colour vectors such as `fixed` and `override`.
#'
#' Validation checks that the input:
#'
#' - is `NULL`, a named vector, or a named list;
#' - has non-empty names;
#' - only refers to categories present in the resolved level set;
#' - contains valid colour values according to [grDevices::col2rgb()].
#'
#' @param x A named vector or named list of colours, or `NULL`.
#' @param lv Character vector containing the resolved level set.
#' @param arg Name of the argument being validated. Used in error messages.
#'
#' @return
#' `NULL` if `x` is `NULL`; otherwise a named vector containing the validated
#' colours, restricted to names present in `lv`.
#'
#' @keywords internal
.validate_named_colours <- function(x, lv, arg) {
  
  if (is.null(x)) {
    return(NULL)
  }
  
  arg <- match.arg(arg, c("fixed", "override"))
  
  if (is.list(x)) {
    x <- unlist(x, use.names = TRUE)
  }
  
  if (is.null(names(x)) || any(names(x) == "")) {
    cli::cli_abort("{.arg {arg}} must be a named vector or list.")
  }
  
  not_in_levels <- setdiff(names(x), lv)
  
  if (length(not_in_levels) > 0L) {
    cli::cli_abort(c(
      "Names in {.arg {arg}} must be a subset of the final level set.",
      "x" = "Unknown names in {.arg {arg}}: {not_in_levels}"
    ))
  }
  
  ok <- tryCatch(
    grDevices::col2rgb(x),
    error = function(e) NULL
  )
  
  if (is.null(ok)) {
    cli::cli_abort("{.arg {arg}} contains invalid colours.")
  }
  
  x[names(x) %in% lv]
}


#' Build a named palette map
#'
#' @description
#' Build a named mapping from categories to colours.
#'
#' The palette is built in three stages:
#'
#' - the final set of categories is determined from `x` and `levels`;
#' - colours in `fixed` are assigned first and used during palette generation;
#' - colours in `override` are replaced at the end.
#'
#' If `x` contains missing values and `na_color` is not `NULL`, the returned
#' mapping also includes a named `"NA"` entry.
#'
#' @param x A vector treated as discrete.
#' @param levels Optional character vector specifying the complete set and order
#'   of categories.
#' @param drop Logical; only used when `levels = NULL` and `x` is a factor.
#' @param na_color Colour associated with missing values when present in `x`.
#'   Use `NULL` to avoid adding a dedicated `"NA"` entry.
#' @param fixed Named vector or named list of colours to fix for selected
#'   categories. These colours are used during palette generation.
#' @param override Named vector or named list of colours to apply after palette
#'   generation. These values replace the final colours of the specified
#'   categories.
#' @param colorspace Passed to [qualpalr::qualpal()].
#' @param cvd Passed to [qualpalr::qualpal()].
#' @param bg Passed to [qualpalr::qualpal()].
#' @param metric Distance metric passed to [qualpalr::qualpal()]. Must be one of
#'   `"ciede2000"`, `"din99d"`, or `"cie76"`.
#'
#' @return
#' A named character vector mapping each resolved level to a colour. If relevant,
#' the vector may also include an `"NA"` entry.
#'
#' @keywords internal
.compute_palette_map <- function(
    x,
    levels = NULL,
    drop = FALSE,
    na_color = "#BEBEBE",
    fixed = NULL,
    override = NULL,
    colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
    cvd = c(protan = 0, deutan = 0, tritan = 0),
    bg = NULL,
    metric = c("ciede2000", "din99d", "cie76")
) {
  metric <- match.arg(metric)
  
  lv <- .resolve_levels(x = x, levels = levels, drop = drop)
  
  fixed <- .validate_named_colours(fixed, lv, arg = "fixed")
  override <- .validate_named_colours(override, lv, arg = "override")
  
  lv_fixed <- if (is.null(fixed)) character(0) else intersect(lv, names(fixed))
  lv_free <- setdiff(lv, lv_fixed)
  
  palette_map <- if (length(lv_fixed) > 0L) {
    stats::setNames(fixed[lv_fixed], lv_fixed)
  } else {
    character(0)
  }
  
  if (length(lv_free) > 0L) {
    seed_colours <- if (length(lv_fixed) > 0L) {
      unname(fixed[lv_fixed])
    } else {
      character(0)
    }

    n_generate <- length(seed_colours) + length(lv_free)
    n_request <- if (length(seed_colours) == 0L && n_generate == 1L) 2L else n_generate
    
    pal_all <- tryCatch(
      qualpalr::qualpal(
        n = n_request,
        colorspace = colorspace,
        cvd = cvd,
        bg = bg,
        metric = metric,
        extend = if (length(seed_colours) > 0L) seed_colours else NULL
      )$hex,
      error = function(e) {
        cli::cli_abort(c(
          "Failed while calling {.fn qualpalr::qualpal}.",
          "x" = conditionMessage(e)
        ))
      }
    )
    
    pal_hex <- pal_all[seq.int(from = length(seed_colours) + 1L, length.out = length(lv_free))]
    names(pal_hex) <- lv_free
    
    palette_map <- c(palette_map, pal_hex)
    palette_map <- palette_map[lv]
  }
  
  if (!is.null(override)) {
    palette_map[names(override)] <- override
  }
  
  if (any(is.na(x)) && !is.null(na_color)) {
    palette_map <- c(palette_map, `NA` = na_color)
  }
  
  palette_map
}


# =============================================================================
# Public mapping
# =============================================================================

#' Map discrete categories to colours using qualpal
#'
#' @description
#' Generates a colour mapping for a discrete vector using
#' [qualpalr::qualpal()]. The function returns a named character vector where
#' each category is associated with a colour.
#'
#' Colours can be generated automatically, fixed with `fixed`, or replaced with
#' `override`.
#'
#' @param x A vector treated as discrete. It may be a character vector, factor,
#'   or any vector coercible to character labels.
#' @param levels Optional character vector specifying the complete set and order
#'   of categories to include in the palette. All non-missing categories
#'   present in `x` must be included.
#' @param drop Logical; only used when `levels = NULL` and `x` is a factor.
#'   If `TRUE`, unused factor levels are removed. If `FALSE`, all factor levels
#'   are retained.
#' @param na_color Colour used for missing values. If `x` contains `NA` and
#'   `na_color` is not `NULL`, the returned palette includes an additional
#'   `"NA"` entry. Set to `NULL` to ignore missing values.
#' @param fixed Named vector or named list specifying colours to fix for
#'   selected categories. Names must correspond to category labels. These
#'   colours are used during palette generation.
#' @param override Named vector or named list specifying colours to assign
#'   after palette generation. Names must correspond to category labels.
#'   These colours replace the final values of the specified categories.
#' @param colorspace Passed to [qualpalr::qualpal()]. Defines the region of
#'   colour space used to generate colours.
#' @param cvd Passed to [qualpalr::qualpal()]. Can be used to simulate colour
#'   vision deficiencies.
#' @param bg Passed to [qualpalr::qualpal()]. Background colour used during
#'   palette generation.
#' @param metric Distance metric used by [qualpalr::qualpal()]. One of
#'   `"ciede2000"`, `"din99d"`, or `"cie76"`.
#'
#' @return
#' A named character vector mapping categories to colours.
#'
#' @details
#' Category resolution follows this order:
#'
#' - if `levels` is supplied, it defines the complete set and order of categories;
#' - otherwise, if `x` is a factor, the factor levels are used;
#' - otherwise, the unique non-missing values of `x` are used.
#'
#' Colour assignment follows this order:
#'
#' - categories specified in `fixed` keep the colours provided by the user;
#' - colours for the remaining categories are generated automatically using
#'   [qualpalr::qualpal()], taking the fixed colours into account.
#'
#' Finally, if `override` is supplied, those colours replace the corresponding
#' entries in the final palette.
#'
#' The order of the returned palette always follows the resolved category order.
#'
#' If `x` is empty but `levels` is supplied, the palette is generated from
#' `levels`.
#'
#' @seealso
#' [pal_qualpal()], [qualpalr::qualpal()]
#'
#' @examples
#' x <- c("A", "B", "C", "A")
#' map_qualpal(x)
#'
#' x <- c("A", "B", "Other", "Smaller")
#' map_qualpal(
#'   x,
#'   fixed = c(
#'     Other = "transparent",
#'     Smaller = "#F5F5DC"
#'   )
#' )
#'
#' x <- c("A", "B", "A", "C")
#' pal <- map_qualpal(x)
#' pal[as.character(x)]
#'
#' # Final replacement after palette generation
#' map_qualpal(
#'   x,
#'   override = c(A = "#000000")
#' )
#'
#' @export
map_qualpal <- function(
    x,
    levels = NULL,
    drop = FALSE,
    na_color = "#BEBEBE",
    fixed = NULL,
    override = NULL,
    colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
    cvd = c(protan = 0, deutan = 0, tritan = 0),
    bg = NULL,
    metric = c("ciede2000", "din99d", "cie76")
) {
  if (length(x) == 0L && is.null(levels)) {
    return(character(0))
  }
  
  .compute_palette_map(
    x = x,
    levels = levels,
    drop = drop,
    na_color = na_color,
    fixed = fixed,
    override = override,
    colorspace = colorspace,
    cvd = cvd,
    bg = bg,
    metric = metric
  )
}

# =============================================================================
# Palette generators
# =============================================================================

#' Qualpal discrete palette generator
#'
#' @description
#' Creates a discrete palette generator based on [qualpalr::qualpal()].
#'
#' The returned function takes a single argument `n` and generates `n`
#' qualitative colours.
#'
#' @param colorspace Passed to [qualpalr::qualpal()].
#' @param cvd Passed to [qualpalr::qualpal()].
#' @param bg Passed to [qualpalr::qualpal()].
#' @param metric Distance metric used by [qualpalr::qualpal()]. One of
#'   `"ciede2000"`, `"din99d"`, or `"cie76"`.
#' @param extend Passed to [qualpalr::qualpal()].
#'
#' @return
#' A function that generates `n` colours when called.
#'
#' @seealso
#' [map_qualpal()], [qualpalr::qualpal()]
#'
#' @examples
#' pal <- pal_qualpal()
#' pal(5)
#'
#' @export
pal_qualpal <- function(
    colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
    cvd = c(protan = 0, deutan = 0, tritan = 0),
    bg = NULL,
    metric = c("ciede2000", "din99d", "cie76"),
    extend = NULL
) {
  metric <- match.arg(metric)
  
  function(n) {
    if (n <= 0) return(character(0))
    
    qualpalr::qualpal(
      n = n,
      colorspace = colorspace,
      cvd = cvd,
      bg = bg,
      metric = metric,
      extend = extend
    )$hex
  }
}


# =============================================================================
# ggplot2 scales
# =============================================================================

ScaleDiscreteQualpal <- ggplot2::ggproto(
  "ScaleDiscreteQualpal",
  ggplot2::ScaleDiscrete,
  
  map = function(self, x, limits = self$get_limits()) {
    x_chr <- as.character(x)
    
    if (length(limits) == 0L) {
      return(rep(self$na.value, length(x_chr)))
    }
    
    palette_map <- map_qualpal(
      x = limits,
      levels = limits,
      drop = FALSE,
      na_color = self$na.value,
      fixed = self$fixed,
      override = self$override,
      colorspace = self$colorspace,
      cvd = self$cvd,
      bg = self$bg,
      metric = self$metric
    )
    
    out <- palette_map[x_chr]
    out[is.na(out)] <- self$na.value
    
    unname(out)
  }
)

#' Discrete ggplot2 scale based on qualpal
#'
#' @description
#' Internal helper for ggplot2 discrete scales based on [map_qualpal()] and
#' [pal_qualpal()].
#'
#' @param aesthetics Character vector of aesthetics to which the scale applies.
#' @param name The name of the scale. Used as the axis or legend title. If
#'   `waiver()`, the default ggplot2 label is used.
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()].
#' @param fixed Named vector or named list specifying colours to fix for
#'   selected categories. Names must correspond to category labels. These
#'   colours are used during palette generation.
#' @param override Named vector or named list specifying colours to assign
#'   after palette generation. Names must correspond to category labels.
#'   These colours replace the final values of the specified categories.
#' @param colorspace Passed to [qualpalr::qualpal()].
#' @param cvd Passed to [qualpalr::qualpal()].
#' @param bg Passed to [qualpalr::qualpal()].
#' @param metric Distance metric used by [qualpalr::qualpal()]. One of
#'   `"ciede2000"`, `"din99d"`, or `"cie76"`.
#' @param na.value Colour used for missing values.
#'
#' @return
#' A ggplot2 discrete scale.
#'
#' @seealso
#' [map_qualpal()], [pal_qualpal()], [ggplot2::discrete_scale()]
#'
#' @keywords internal
scale_discrete_qualpal <- function(
    aesthetics,
    name = ggplot2::waiver(),
    ...,
    fixed = NULL,
    override = NULL,
    colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
    cvd = c(protan = 0, deutan = 0, tritan = 0),
    bg = NULL,
    metric = c("ciede2000", "din99d", "cie76"),
    na.value = NA
) {
  metric <- match.arg(metric)
  
  scale <- ggplot2::discrete_scale(
    aesthetics = aesthetics,
    name = name,
    palette = pal_qualpal(
      colorspace = colorspace,
      cvd = cvd,
      bg = bg,
      metric = metric
    ),
    ...,
    na.value = na.value,
    super = ScaleDiscreteQualpal
  )
  
  scale$fixed <- if (is.null(fixed)) NULL else as.list(fixed)
  scale$override <- if (is.null(override)) NULL else as.list(override)
  scale$colorspace <- colorspace
  scale$cvd <- cvd
  scale$bg <- bg
  scale$metric <- metric
  
  scale
}


#' Discrete colour scales based on qualpal
#'
#' @description
#' Discrete ggplot2 scales that generate qualitative colours using
#' [qualpalr::qualpal()].
#'
#' These scales support fixed colours used during palette generation and final
#' colour replacements.
#'
#' @param name The name of the scale. Used as the axis or legend title. If
#'   `waiver()`, the default ggplot2 label is used.
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()].
#' @param fixed Named vector or named list specifying colours to fix for
#'   selected categories. Names must correspond to category labels. These
#'   colours are used during palette generation.
#' @param override Named vector or named list specifying colours to assign
#'   after palette generation. Names must correspond to category labels.
#'   These colours replace the final values of the specified categories.
#' @param colorspace Passed to [qualpalr::qualpal()].
#' @param cvd Passed to [qualpalr::qualpal()].
#' @param bg Passed to [qualpalr::qualpal()].
#' @param metric Distance metric used by [qualpalr::qualpal()]. One of
#'   `"ciede2000"`, `"din99d"`, or `"cie76"`.
#' @param na.value Colour used for missing values.
#'
#' @return
#' A ggplot2 discrete scale.
#'
#' @seealso
#' [map_qualpal()], [pal_qualpal()], [ggplot2::discrete_scale()]
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   x = 1:6,
#'   y = 1:6,
#'   g = factor(c("A", "B", "C", "A", "B", "C"))
#' )
#'
#' ggplot(df, aes(x, y, colour = g)) +
#'   geom_point(size = 3) +
#'   scale_color_qualpal()
#'
#' ggplot(df, aes(x, y, fill = g)) +
#'   geom_point(shape = 21, size = 4) +
#'   scale_fill_qualpal()
#'
#' df2 <- data.frame(
#'   x = 1:5,
#'   y = 1:5,
#'   g = factor(c("A", "Other", "B", "Smaller", "A"))
#' )
#'
#' ggplot(df2, aes(x, y, fill = g)) +
#'   geom_point(shape = 21, size = 5) +
#'   scale_fill_qualpal(
#'     fixed = c(
#'       Other = "transparent",
#'       Smaller = "#F5F5DC"
#'     )
#'   )
#'
#' ggplot(df2, aes(x, y, colour = g)) +
#'   geom_point(size = 4) +
#'   scale_color_qualpal(
#'     override = c(Other = "#000000")
#'   )
#'
#' @name scale_qualpal
NULL


#' @rdname scale_qualpal
#' @export
scale_color_qualpal <- function(
    name = ggplot2::waiver(),
    ...,
    fixed = NULL,
    override = NULL,
    colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
    cvd = c(protan = 0, deutan = 0, tritan = 0),
    bg = NULL,
    metric = c("ciede2000", "din99d", "cie76"),
    na.value = NA
) {
  scale_discrete_qualpal(
    aesthetics = "colour",
    name = name,
    ...,
    fixed = fixed,
    override = override,
    colorspace = colorspace,
    cvd = cvd,
    bg = bg,
    metric = metric,
    na.value = na.value
  )
}


#' @rdname scale_qualpal
#' @export
scale_colour_qualpal <- scale_color_qualpal


#' @rdname scale_qualpal
#' @export
scale_fill_qualpal <- function(
    name = ggplot2::waiver(),
    ...,
    fixed = NULL,
    override = NULL,
    colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
    cvd = c(protan = 0, deutan = 0, tritan = 0),
    bg = NULL,
    metric = c("ciede2000", "din99d", "cie76"),
    na.value = NA
) {
  scale_discrete_qualpal(
    aesthetics = "fill",
    name = name,
    ...,
    fixed = fixed,
    override = override,
    colorspace = colorspace,
    cvd = cvd,
    bg = bg,
    metric = metric,
    na.value = na.value
  )
}


# =============================================================================
# show colors qualpal
# =============================================================================


#' Display a colour palette
#'
#' @description
#' Displays a palette as a series of coloured rectangles.
#'
#' Works with unnamed colour vectors from [pal_qualpal()] and named colour
#' vectors from [map_qualpal()].
#'
#' @param x A character vector of colours.
#' @param labels Optional labels to display for each colour. If `NULL` and `x`
#'   is named, the names of `x` are used. If `NULL` and `x` is unnamed, no
#'   labels are shown.
#' @param border Colour used for rectangle borders.
#' @param cex Axis label size.
#' @param ... Additional arguments passed to [graphics::title()].
#'
#' @return
#' Invisibly returns `x`.
#'
#' @examples
#' # Unnamed palette
#' pal <- pal_qualpal()(6)
#' show_qualpal(pal)
#'
#' # Named palette
#' x <- c("A", "B", "C", "A")
#' pal <- map_qualpal(x)
#' show_qualpal(pal)
#'
#' @export
show_qualpal <- function(
    x,
    labels = NULL,
    border = "white",
    cex = 0.8,
    ...
) {
  if (length(x) == 0L) {
    return(invisible(x))
  }
  
  if (is.null(labels) && !is.null(names(x))) {
    labels <- names(x)
  }
  
  n <- length(x)
  
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  
  bottom_margin <- if (is.null(labels)) 2 else 6
  graphics::par(mar = c(bottom_margin, 1, 2, 1))
  
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, n), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  
  for (i in seq_len(n)) {
    graphics::rect(
      xleft = i - 1,
      ybottom = 0,
      xright = i,
      ytop = 1,
      col = x[i],
      border = border
    )
  }
  
  if (!is.null(labels)) {
    graphics::axis(
      side = 1,
      at = seq_len(n) - 0.5,
      labels = labels,
      tick = FALSE,
      las = 2,
      cex.axis = cex
    )
  }
  
  graphics::box()
  graphics::title(...)
  
  invisible(x)
}
