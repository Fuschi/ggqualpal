#' Qualitative palettes via qualpalr (helper)
#'
#' Thin wrapper around \link[qualpalr]{qualpal} that returns a discrete palette
#' for ggplot-like scales. All argument validation is delegated to
#' \code{qualpalr::qualpal()} itself; errors are caught and rethrown with
#' a concise message.
#'
#' @param colorspace See \link[qualpalr]{qualpal} (\emph{HSL/LCHab list},
#'   \emph{"Source:Palette"} string, or \emph{RGB} matrix/data frame).
#' @param cvd Named numeric vector for color vision deficiency adaptation
#'   (\code{protan}, \code{deutan}, \code{tritan}); passed through to
#'   \link[qualpalr]{qualpal}.
#' @param bg Background color (or \code{NULL}); passed through.
#' @param metric One of \code{"ciede2000"}, \code{"din99d"}, \code{"cie76"}; passed through.
#' @param extend Hex colors or RGB matrix/data frame (or \code{NULL}); passed through.
#' @param direction \code{1} to keep order, \code{-1} to reverse. Any other value defaults to \code{1}.
#'
#' @return A discrete palette object; \code{$fun(n)} returns \code{n} hex colors.
#'
#' @seealso \link[qualpalr]{qualpal}, \link[qualpalr:list_palettes]{list_palettes}
#' @keywords internal
pal_qualpal <- function(
    colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
    cvd = c(protan = 0, deutan = 0, tritan = 0),
    bg = NULL,
    metric = c("ciede2000", "din99d", "cie76"),
    extend = NULL,
    direction = 1L) {
  
  # --- Package availability
  if (!requireNamespace("qualpalr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg qualpalr} is required but not installed.",
      "i" = "Install with {.code install.packages('qualpalr')}."
    ))
  }
  
  # --- Normalize 'metric'
  metric <- match.arg(metric)
  
  # --- direction
  if (!is.numeric(direction) || length(direction) != 1 || !(direction %in% c(1, -1))) {
    cli::cli_abort("{.arg direction} must be either 1 or -1.")
  }
  
  # --- function
  fun <- function(n) {
    if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
      cli::cli_abort("You must request at least one color (n >= 1).")
    }
    
    pal <- tryCatch(
      qualpalr::qualpal(
        n = as.integer(n),
        colorspace = colorspace,
        cvd = cvd,
        bg = bg,
        metric = metric,
        extend = extend
      ),
      error = function(e) {
        cli::cli_abort(c(
          "Failed while calling {.fn qualpalr::qualpal}.",
          "x" = conditionMessage(e)
        ))
      }
    )
    
    if (direction == -1) rev(pal$hex) else pal$hex
  }
  
  scales::new_discrete_palette(fun = fun, type = "colour", nlevels = NA)
}

#' Discrete colour/fill scales using qualpalr
#'
#' @description
#' Discrete colour and fill scales for **ggplot2** based on
#' [qualpalr::qualpal()], which algorithmically generates sets of maximally
#' distinct qualitative colours. This makes them particularly suitable for
#' categorical (qualitative) variables.
#'
#' You can pass any argument accepted by [qualpalr::qualpal()],
#' such as:
#' - `colorspace`: defines the hue–saturation–lightness (HSL) or LCHab space,
#'   or selects a named palette (e.g. `"ColorBrewer:Set2"`).
#' - `cvd`: colour vision deficiency adaptation levels.
#' - `bg`: background colour to avoid when generating colours.
#' - `metric`: distance metric used for colour distinctness.
#' - `extend`: optional starting palette to extend.
#'
#' @details
#' These functions provide qualitative alternatives to ggplot2’s built-in
#' discrete scales (e.g., `scale_color_discrete()`), but leverage the
#' [qualpalr::qualpal()] algorithm to generate perceptually distinct colours,
#' even when dealing with a large number of categories.
#'
#' @param name Scale name for the legend/guide, or [ggplot2::waiver()] for default.
#' @param aesthetics Aesthetics this scale works with (e.g. `"color"` or `"fill"`).
#' @param ... Other arguments passed on to [ggplot2::discrete_scale()]
#'   (e.g., `breaks`, `labels`, `limits`, `drop`, `na.translate`, `guide`, `position`).
#' @inheritParams pal_qualpal
#'
#' @return A `ggplot2` discrete scale object.
#'
#' @seealso
#' [qualpalr::qualpal()],
#' [qualpalr::list_palettes()],
#' [ggplot2::scale_color_discrete()],
#' [ggplot2::discrete_scale()]
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:26, y = 1:26, g = factor(letters[1:26]))
#'
#' ggplot(df, aes(x, y, color = g)) +
#'   geom_point(size = 4) +
#'   scale_color_qualpal() +
#'   theme_bw()
#'
#' @name scale_qualpal
#' @aliases scale_color_qualpal scale_colour_qualpal scale_fill_qualpal
NULL

#' @export
#' @rdname scale_qualpal
scale_color_qualpal <- function(name = ggplot2::waiver(), 
                                aesthetics = "color",
                                ...,
                                colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
                                cvd = c(protan = 0, deutan = 0, tritan = 0),
                                bg = NULL,
                                metric = c("ciede2000", "din99d", "cie76"),
                                extend = NULL,
                                direction = 1L) {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    name = name,
    palette = pal_qualpal(colorspace = colorspace, cvd = cvd, bg = bg,
                          metric = metric, extend = extend, direction = direction),
    ...
  )
}

#' @export
#' @rdname scale_qualpal
scale_colour_qualpal <- function(name = ggplot2::waiver(), 
                                aesthetics = "color",
                                ...,
                                colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
                                cvd = c(protan = 0, deutan = 0, tritan = 0),
                                bg = NULL,
                                metric = c("ciede2000", "din99d", "cie76"),
                                extend = NULL,
                                direction = 1L) {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    name = name,
    palette = pal_qualpal(colorspace = colorspace, cvd = cvd, bg = bg,
                          metric = metric, extend = extend, direction = direction),
    ...
  )
}

#' @export
#' @rdname scale_qualpal
scale_fill_qualpal <- function(name = ggplot2::waiver(), 
                                aesthetics = "fill",
                                ...,
                                colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
                                cvd = c(protan = 0, deutan = 0, tritan = 0),
                                bg = NULL,
                                metric = c("ciede2000", "din99d", "cie76"),
                                extend = NULL,
                                direction = 1L) {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    name = name,
    palette = pal_qualpal(colorspace = colorspace, cvd = cvd, bg = bg,
                          metric = metric, extend = extend, direction = direction),
    ...
  )
}


#' Map a vector of categories to qualpalr colours
#'
#' Returns a character vector of hex colours aligned to `x`, using a
#' qualitative palette generated via [qualpalr::qualpal()].
#' If `levels` is supplied, it must include **all** non-`NA` categories
#' found in `x`, otherwise an error is thrown. Fixed colours in `fix`
#' must also be a subset of `levels`.
#'
#' @param x A vector (character, factor, numeric, etc.). Treated as discrete.
#' @param levels Optional character vector defining the full set **and order**
#'   of categories. If provided, it must contain all non-`NA` categories in `x`.
#'   If `NULL`, factor levels (optionally dropped) or unique values are used.
#' @param drop Logical; only used when `levels = NULL` and `x` is a factor.
#'   If `TRUE`, drop unused factor levels; if `FALSE` (default), keep them.
#' @param na_color Colour for `NA` values in `x` (default `"#BEBEBE"`). Use `NULL` to
#'   leave `NA` unchanged.
#' @param fix Named hex colours to pre-assign to specific categories
#'   (e.g. `c(A = "#CD0BBC", C = "#7acf79")`). Must be **hex `#RRGGBB`**
#'   (case-insensitive). When `levels` is provided, names in `fix` must be a
#'   subset of `levels`.
#' @param colorspace,cvd,bg,metric,extend,direction Passed to
#'   [qualpalr::qualpal()]. See its help for details.
#'
#' @return A character vector of hex colours (same length as `x`), with
#'   `attr(result, "palette")` = named mapping level → colour (includes `"NA"`
#'   if `na_color` is not `NULL` and `x` contains `NA`).
#'
#' @seealso [qualpalr::qualpal()]
#' @export
qualpal_colors <- function(
    x,
    levels = NULL,
    drop = FALSE,
    na_color = "#BEBEBE",
    fix = NULL,
    colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
    cvd = c(protan = 0, deutan = 0, tritan = 0),
    bg = NULL,
    metric = c("ciede2000", "din99d", "cie76"),
    extend = NULL,
    direction = 1L
) {
  if (length(x) == 0L) return(character(0))
  
  # Determine observed categories (non-NA) in x
  obs <- unique(as.character(x[!is.na(x)]))
  
  metric <- match.arg(metric)
  
  # Decide the set/order of levels
  if (!is.null(levels)) {
    lv <- as.character(levels)
    
    # STRICT: levels must cover all observed categories in x
    missing_in_levels <- setdiff(obs, lv)
    if (length(missing_in_levels) > 0L) {
      cli::cli_abort(c(
        "{.arg levels} must include all categories present in {.arg x}.",
        "x" = "Missing in {.arg levels}: {missing_in_levels}"
      ))
    }
  } else {
    if (is.factor(x)) {
      lv <- levels(x)
      if (drop) lv <- lv[lv %in% obs]
    } else {
      lv <- obs
    }
  }
  
  # Normalise and validate 'fix'
  if (!is.null(fix)) {
    if (is.list(fix)) fix <- unlist(fix, use.names = TRUE)
    if (is.null(names(fix)) || any(names(fix) == "")) {
      cli::cli_abort("{.arg fix} must be a named vector/list (names are category labels).")
    }
    
    # If levels provided, enforce subset rule for fix
    not_in_levels <- setdiff(names(fix), lv)
    if (length(not_in_levels) > 0L) {
      cli::cli_abort(c(
        "Names in {.arg fix} must be a subset of {.arg levels}.",
        "x" = "Not in levels: {not_in_levels}"
      ))
    }
    
    # Keep only those present in the final level set (in case levels = NULL)
    fix <- fix[names(fix) %in% lv]
    
    # Optional lightweight colour validity check
    if (length(fix)) {
      ok <- TRUE
      res <- try(utils::capture.output(grDevices::col2rgb(fix)), silent = TRUE)
      if (inherits(res, "try-error")) ok <- FALSE
      if (!ok) cli::cli_abort("{.arg fix} contains invalid colours (not interpretable by col2rgb).")
    }
  }
  
  # Split into fixed vs free levels
  lv_fixed <- if (is.null(fix)) character(0) else intersect(lv, names(fix))
  lv_free  <- setdiff(lv, lv_fixed)
  
  # Start palette map with fixed colours
  palette_map <- if (length(lv_fixed)) stats::setNames(fix[lv_fixed], lv_fixed) else character(0)
  
  # Generate colours for remaining levels (no unknowns can appear later)
  if (length(lv_free) > 0L) {
    gen_cols <- qualpalr::qualpal(
      n = as.integer(length(lv_free)),
      colorspace = colorspace,
      cvd = cvd,
      bg = bg,
      metric = metric,
      extend = extend)$hex
    names(gen_cols) <- lv_free
    palette_map <- c(palette_map, gen_cols)[lv]  # honour 'lv' order
  }
  
  # Map x -> colours (no unknowns by construction)
  key <- as.character(x)
  out <- palette_map[key]
  
  # Handle NA in x
  is_na <- is.na(x)
  if (any(is_na)) {
    out[is_na] <- if (is.null(na_color)) NA_character_ else na_color
    if (!is.null(na_color)) palette_map <- c(palette_map, `NA` = na_color)
  }
  
  out
}


#' Build a qualitative palette (level -> colour) via qualpalr
#'
#' Constructs and returns a named vector `palette_map` using 
#' [qualpalr::qualpal()], mirroring the behaviour of
#' [qualpal_colors()] but returning the palette instead of colours aligned
#' to `x`. If `levels` is supplied, it must include **all** non-`NA`
#' categories found in `x`, otherwise an error is thrown. Fixed colours in
#' `fix` must also be a subset of `levels`.
#'
#' @param x A vector (character, factor, numeric, etc.). Treated as discrete
#'   for the purpose of inferring categories if `levels = NULL`.
#' @param levels Optional character vector defining the full set **and order**
#'   of categories. If provided, it must contain all non-`NA` categories in `x`.
#'   If `NULL`, factor levels (optionally dropped) or unique values are used.
#' @param drop Logical; only used when `levels = NULL` and `x` is a factor.
#'   If `TRUE`, drop unused factor levels; if `FALSE` (default), keep them.
#' @param na_color Colour for `NA` values (default `"#BEBEBE"`). If `x`
#'   contains `NA` and `na_color` is not `NULL`, the palette will include an
#'   `"NA"` entry with this colour.
#' @param fix Named hex colours to pre-assign to specific categories
#'   (e.g. `c(A = "#CD0BBC", C = "#7acf79")`). Colours are validated via
#'   `grDevices::col2rgb()`. When `levels` is provided, names in `fix` must be
#'   a subset of `levels`.
#' @param colorspace,cvd,bg,metric,extend,direction Passed to
#'   [qualpalr::qualpal()]. See its help for details.
#'
#' @return A named character vector of hex colours (level → colour). If `x`
#'   contains `NA` and `na_color` is not `NULL`, the palette includes an `"NA"`
#'   entry.
#'
#' @seealso [qualpal_colors()], [qualpalr::qualpal()]
#' @export
qualpal_palette <- function(
    x,
    levels = NULL,
    drop = FALSE,
    na_color = "#BEBEBE",
    fix = NULL,
    colorspace = list(h = c(0, 360), s = c(0.2, 0.5), l = c(0.6, 0.85)),
    cvd = c(protan = 0, deutan = 0, tritan = 0),
    bg = NULL,
    metric = c("ciede2000", "din99d", "cie76"),
    extend = NULL,
    direction = 1L
) {
  if (length(x) == 0L) return(character(0))
  
  # observed (non-NA)
  obs <- unique(as.character(x[!is.na(x)]))
  
  # metric
  metric <- match.arg(matrix)
  
  # strict levels
  if (!is.null(levels)) {
    lv <- as.character(levels)
    missing_in_levels <- setdiff(obs, lv)
    if (length(missing_in_levels) > 0L) {
      cli::cli_abort(c(
        "{.arg levels} must include all categories present in {.arg x}.",
        "x" = "Missing in {.arg levels}: {missing_in_levels}"
      ))
    }
  } else {
    if (is.factor(x)) {
      lv <- levels(x)
      if (drop) lv <- lv[lv %in% obs]
    } else {
      lv <- obs
    }
  }
  
  # validate 'fix' colours and subset rule
  if (!is.null(fix)) {
    if (is.list(fix)) fix <- unlist(fix, use.names = TRUE)
    if (is.null(names(fix)) || any(names(fix) == "")) {
      cli::cli_abort("{.arg fix} must be a named vector/list (names are category labels).")
    }

    not_in_levels <- setdiff(names(fix), lv)
    if (length(not_in_levels) > 0L) {
      cli::cli_abort(c(
        "Names in {.arg fix} must be a subset of {.arg levels}.",
        "x" = "Not in levels: {not_in_levels}"
      ))
    }
    
    if (length(fix)) {
      ok <- TRUE
      res <- try(utils::capture.output(grDevices::col2rgb(fix)), silent = TRUE)
      if (inherits(res, "try-error")) ok <- FALSE
      if (!ok) cli::cli_abort("{.arg fix} contains invalid colours (not interpretable by col2rgb).")
    }
    
    fix <- fix[names(fix) %in% lv]
  }
  
  # split fixed vs free
  lv_fixed <- if (is.null(fix)) character(0) else intersect(lv, names(fix))
  lv_free  <- setdiff(lv, lv_fixed)
  
  # start palette with fixed colours
  palette_map <- if (length(lv_fixed)) stats::setNames(fix[lv_fixed], lv_fixed) else character(0)
  
  # generate hex for remaining levels
  if (length(lv_free) > 0L) {
    pal_hex <- qualpalr::qualpal(
      n = as.integer(length(lv_free)),
      colorspace = colorspace,
      cvd = cvd,
      bg = bg,
      metric = metric,
      extend = extend
    )$hex
    if (identical(direction, -1L)) pal_hex <- rev(pal_hex)
    
    gen_cols <- pal_hex
    names(gen_cols) <- lv_free
    palette_map <- c(palette_map, gen_cols)[lv]  # honour 'lv' order
  }
  
  # include NA mapping if needed
  if (any(is.na(x)) && !is.null(na_color)) {
    palette_map <- c(palette_map, `NA` = na_color)
  }
  
  palette_map
}
