#' Set or Get Global Sample Design
#'
#' Manages a global sample design matrix/data frame by setting or retrieving it.
#' This object is typically a matrix with one column per input parameter and one row per sample, or
#' the output of [sensitivity::sensitivity] functions. It can be used as
#' default input in [eval_module()].
#'
#' @param data (matrix, data frame, or list, optional). Sample design to store
#'   globally. Accepts a matrix/data frame or a list with a matrix in element `X`
#'   (typically output of [sensitivity::sensitivity] functions). If `NULL`, returns the current
#'   global sample design. Default: `NULL`.
#'
#' @return Current or newly set sample design (`list` with elements `sa` and
#'   `X`) or `NULL` if no sample design has been set.
#'
#' @examples
#' # Get current sample design (NULL if not set)
#' current_sample_design <- set_sample_design()
#'
#' # Set sample design
#' X <- data.frame(a = c(0.1, 0.2), b = c(1, 2))
#' set_sample_design(X)
#'
#' # Reset sample design
#' reset_sample_design()
#'
#' @export
set_sample_design <- function(data = NULL) {
  if (is.null(data)) {
    if (!exists("sample_design", envir = .pkgglobalenv)) {
      assign("sample_design", NULL, envir = .pkgglobalenv)
    }
    return(get("sample_design", envir = .pkgglobalenv))
  }

  if (is.matrix(data) || is.data.frame(data)) {
    sample_design_obj <- list(
      sa = NULL,
      X = as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
    )
  } else if (is.list(data)) {
    if (!"X" %in% names(data)) {
      stop("sample_design list must contain element 'X'")
    }
    if (!(is.matrix(data$X) || is.data.frame(data$X))) {
      stop("sample_design$X must be a matrix or data frame")
    }
    sample_design_obj <- list(
      sa = data$sa,
      X = as.data.frame(data$X, stringsAsFactors = FALSE, check.names = FALSE)
    )
  } else {
    stop("sample_design must be a matrix, data frame, or list with element 'X'")
  }

  assign("sample_design", sample_design_obj, envir = .pkgglobalenv)
  message("sample_design set to ", deparse(substitute(data)))
}

#' Reset Global Sample Design
#'
#' Clears and resets the global sample design to `NULL`.
#'
#' @return `NULL` (invisibly). Clears global sample design.
#'
#' @examples
#' reset_sample_design()
#'
#' @export
reset_sample_design <- function() {
  assign("sample_design", NULL, envir = .pkgglobalenv)
  message("sample_design reset")
}

# Parse sample_space strings used in mctable.
#
# Supported formats:
# - "c(...)"            (vector; numeric length-2 is treated as bounds)
# - "key = val, ..."    (named list; numeric values parsed where possible)
#
# Returns a list with:
# - kind: "vector" | "named"
# - values: vector or named list
parse_sample_space <- function(ss) {
  ss <- trimws(as.character(ss))
  if (is.na(ss) || ss == "") {
    stop("sample_space must not be NA or empty")
  }

  if (grepl("^c\\s*\\(", ss)) {
    vals <- eval(parse(text = ss), envir = baseenv())
    return(list(kind = "vector", values = vals))
  }

  if (grepl("=", ss)) {
    parts <- unlist(strsplit(ss, ",\\s*"))
    keys <- trimws(sub("=.*$", "", parts))
    vals_chr <- trimws(sub("^[^=]*=", "", parts))

    vals <- lapply(vals_chr, function(x) {
      if (x %in% c("TRUE", "FALSE")) {
        return(as.numeric(x))
      }
      if (grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", x)) {
        return(as.numeric(x))
      }
      gsub("^['\"]|['\"]$", "", x)
    })
    names(vals) <- keys
    return(list(kind = "named", values = vals))
  }

  stop("Unsupported sample_space format")
}

# Parse numeric min/max bounds from a sample_space string.
# Returns c(min = ..., max = ...) or NULL if bounds are unavailable.
parse_sample_space_bounds <- function(ss) {
  ss <- trimws(as.character(ss))
  if (is.na(ss) || ss %in% c("", "NA")) {
    return(NULL)
  }

  parsed <- parse_sample_space(ss)

  if (identical(parsed$kind, "vector")) {
    vals <- parsed$values
    if (is.numeric(vals) && length(vals) == 2) {
      return(c(min = vals[1], max = vals[2]))
    }
    return(NULL)
  }

  vals <- parsed$values
  if (
    all(c("min", "max") %in% names(vals)) &&
      all(vapply(vals[c("min", "max")], is.numeric, logical(1)))
  ) {
    return(c(min = as.numeric(vals$min), max = as.numeric(vals$max)))
  }

  NULL
}

# Extract numeric bounds, with informative errors.
extract_numeric_bounds <- function(ss, node_name) {
  bounds <- parse_sample_space_bounds(ss)
  if (!is.null(bounds)) {
    return(bounds)
  }

  parsed <- parse_sample_space(ss)
  if (identical(parsed$kind, "vector")) {
    stop(sprintf(
      "Cannot extract numeric bounds from sample_space for '%s'. Use format 'c(min, max)' or 'min = X, max = Y' for Morris.",
      node_name
    ))
  }

  stop(sprintf(
    "Cannot extract numeric bounds from sample_space for '%s'. Use format 'min = X, max = Y' for Morris.",
    node_name
  ))
}

# Sample values from a sample_space definition (used for probing transforms).
sample_from_space <- function(ss, n) {
  parsed <- parse_sample_space(ss)

  if (identical(parsed$kind, "vector")) {
    vals <- parsed$values
    if (length(vals) == 0) {
      stop("sample_space vector cannot be empty")
    }
    if (is.numeric(vals) && length(vals) == 2) {
      return(stats::runif(n, min = vals[1], max = vals[2]))
    }
    return(sample(vals, size = n, replace = TRUE))
  }

  vals <- parsed$values
  vals_un <- unlist(vals, use.names = FALSE)

  if (
    all(vapply(vals, is.numeric, logical(1))) &&
      all(c("min", "max") %in% names(vals))
  ) {
    return(stats::runif(n, min = vals$min, max = vals$max))
  }

  if (all(vapply(vals, is.numeric, logical(1))) && length(vals_un) == 1) {
    return(rep(as.numeric(vals_un), n))
  }

  # Fallback: categorical sampling (useful only for probing transformations)
  sample(vals_un, size = n, replace = TRUE)
}

# Compute a fixed value from numeric bounds.
fixed_from_bounds <- function(bounds, if_not_sampled) {
  if (is.null(bounds)) {
    return(0)
  }
  switch(
    if_not_sampled,
    median = mean(bounds),
    mean = mean(bounds),
    max = bounds[["max"]],
    min = bounds[["min"]]
  )
}

# Filter an mctable by mc_names, and move missing/empty sample_space to "out".
split_mctable_for_sampling <- function(mctable, mc_names = NULL) {
  all_names <- as.character(mctable$mcnode)

  if (!is.null(mc_names)) {
    mc_names <- as.character(mc_names)
    invalid <- setdiff(mc_names, all_names)
    if (length(invalid) > 0) {
      stop(sprintf(
        "Invalid mc_names: %s not in mctable$mcnode",
        paste(invalid, collapse = ", ")
      ))
    }
    mctable_in <- mctable[mctable$mcnode %in% mc_names, , drop = FALSE]
    mctable_out <- mctable[!(mctable$mcnode %in% mc_names), , drop = FALSE]
  } else {
    mctable_in <- mctable
    mctable_out <- mctable[FALSE, , drop = FALSE]
  }

  ss_trim <- trimws(as.character(mctable_in$sample_space))
  moved <- is.na(mctable_in$sample_space) | ss_trim %in% c("", "NA")
  if (any(moved)) {
    mctable_out <- rbind(mctable_out, mctable_in[moved, , drop = FALSE])
    mctable_in <- mctable_in[!moved, , drop = FALSE]
  }

  list(mctable_in = mctable_in, mctable_out = mctable_out)
}

#' Extract Morris Bounds From mctable
#'
#' Extract the bounds required by [sensitivity::morris()] from an `mctable`.
#'
#' Supports sampling only a subset of nodes via `mc_names` and controls how
#' non-sampled nodes are handled via `if_not_sampled`. If `transformation` is
#' enabled and `mctable` includes a `transformation` column, the function
#' computes bounds on the transformed values.
#'
#' @param mctable (data frame). Table containing at least `mcnode` and
#'   `sample_space`; may also contain `transformation`. Default: [set_mctable()].
#' @param mc_names (character vector, optional). Node names to include. If
#'   `NULL`, all nodes in `mctable$mcnode` are used.
#' @param if_not_sampled (character). How to handle nodes not listed in
#'   `mc_names` (and nodes with missing or empty `sample_space`):
#'   `"exclude"`, `"median"`, `"mean"`, `"max"`, or `"min"`.
#'   Default: `"exclude"`.
#' @param transformation (logical). Whether to apply `transformation` rules.
#'   Default: `TRUE`.
#' @param n_probe (integer). Number of probe draws used to approximate bounds
#'   when `transformation = TRUE`. Default: 1000.
#'
#' @return A list `bounds` with:
#'   \itemize{
#'     \item `binf` numeric vector of lower bounds (same order as `factors`).
#'     \item `bsup` numeric vector of upper bounds (same order as `factors`).
#'     \item `factors` character vector of factor names.
#'     \item `fixed` named numeric vector with fixed values for non-sampled
#'       factors when `if_not_sampled != "exclude"`.
#'   }
#'
#' @export
mctable_bounds <- function(
  mctable = set_mctable(),
  mc_names = NULL,
  if_not_sampled = c("exclude", "median", "mean", "max", "min"),
  transformation = TRUE,
  n_probe = 1000
) {
  if_not_sampled <- match.arg(if_not_sampled)

  if (!all(c("mcnode", "sample_space") %in% names(mctable))) {
    stop("mctable must contain columns 'mcnode' and 'sample_space'")
  }

  spl <- split_mctable_for_sampling(mctable, mc_names = mc_names)
  mctable_in <- spl$mctable_in
  mctable_out <- spl$mctable_out

  factors <- as.character(mctable_in$mcnode)
  sample_space <- as.character(mctable_in$sample_space)

  # Apply transformations by probing and updating bounds on the transformed scale.
  if (isTRUE(transformation) && "transformation" %in% names(mctable)) {
    transformations <- as.character(mctable_in$transformation)

    for (i in seq_along(factors)) {
      transform_i <- transformations[i]
      if (!is.na(transform_i) && nzchar(trimws(transform_i))) {
        probe_vals <- sample_from_space(sample_space[i], n_probe)
        transformed_vals <- eval(
          parse(text = transform_i),
          envir = list2env(list(value = probe_vals), parent = baseenv())
        )

        if (is.logical(transformed_vals)) {
          transformed_vals <- as.numeric(transformed_vals)
        }

        sample_space[i] <- sprintf(
          "min = %g, max = %g",
          min(transformed_vals, na.rm = TRUE),
          max(transformed_vals, na.rm = TRUE)
        )
      }
    }
  }

  binf <- numeric(length(factors))
  bsup <- numeric(length(factors))
  for (i in seq_along(factors)) {
    b <- extract_numeric_bounds(sample_space[i], factors[i])
    binf[i] <- b[["min"]]
    bsup[i] <- b[["max"]]
  }

  fixed <- numeric(0)
  if (nrow(mctable_out) > 0 && if_not_sampled != "exclude") {
    for (i in seq_len(nrow(mctable_out))) {
      node_name <- as.character(mctable_out$mcnode[i])
      bounds_i <- parse_sample_space_bounds(mctable_out$sample_space[i])
      fixed_val <- fixed_from_bounds(bounds_i, if_not_sampled)

      if (isTRUE(transformation) && "transformation" %in% names(mctable_out)) {
        transform_val <- as.character(mctable_out$transformation[i])
        if (!is.na(transform_val) && nzchar(trimws(transform_val))) {
          fixed_val <- as.numeric(eval(
            parse(text = trimws(transform_val)),
            envir = list2env(list(value = fixed_val), parent = baseenv())
          ))
        }
      }

      fixed[[node_name]] <- fixed_val
    }
  }

  list(binf = binf, bsup = bsup, factors = factors, fixed = fixed)
}

#' Sobol sampling matrices from an mctable
#'
#' Create Sobol sampling matrices using [sensobol::sobol_matrices()] and an
#' `mctable` definition. The function generates quasi-random draws in \[0, 1\]
#' and then maps them to the target distributions defined in `mctable$mc_func`
#' (or `mctable$func`) and `mctable$sample_space`.
#'
#' If the distribution function is missing but numeric bounds are available in
#' `sample_space` (e.g. `min = 0, max = 1` or `c(0, 1)`), the function assumes a
#' uniform distribution (`stats::runif`).
#'
#' @param mctable (data frame). Table containing at least `mcnode` and
#'   `sample_space`; may also contain `mc_func` / `func`.
#' @param N (integer). Base sample size (see [sensobol::sobol_matrices()]).
#' @param matrices (character). Which Sobol matrices to create (see
#'   [sensobol::sobol_matrices()]). Default: `c("A", "B", "AB")`.
#' @param order (character). Either `"first"`,  `"second"`, `"third"`, or `"fourth"` (see
#'   [sensobol::sobol_matrices()]).
#' @param type (character). Sampling design used by `sensobol::sobol_matrices()`.
#'   In sensobol 1.1.6, options include `"QRN"` (default), `"LHS"`, and `"R"`.
#' @param mc_names (character vector, optional). Node names to include. If
#'   `NULL`, all nodes in `mctable$mcnode` are used.
#' @param ... Additional arguments passed to [sensobol::sobol_matrices()] (and
#'   potentially to `randtoolbox::sobol()` when `type = "QRN"`).
#'
#' @return A numeric matrix where each column is a model input distributed in
#'   (0, 1) **after mapping to the distributions defined in the `mctable`**,
#'   and each row is a sampling point. The matrix has the same layout/row
#'   binding as [sensobol::sobol_matrices()].
#'
#' @export
mctable_sobol_matrices <- function(
  mctable = set_mctable(),
  N,
  matrices = c("A", "B", "AB"),
  order = c("first", "second", "third", "fourth"),
  type = c("QRN", "LHS", "R"),
  mc_names = NULL,
  ...
) {
  if (!requireNamespace("sensobol", quietly = TRUE)) {
    stop(
      "This function needs the 'sensobol' package.\n\nInstall it using:\ninstall.packages('sensobol')"
    )
  }

  matrices <- as.character(matrices)
  order <- match.arg(order)
  type <- match.arg(type)

  if (!all(c("mcnode", "sample_space") %in% names(mctable))) {
    stop("mctable must contain columns 'mcnode' and 'sample_space'")
  }

  if (!"mc_func" %in% names(mctable) && "func" %in% names(mctable)) {
    mctable$mc_func <- mctable$func
  }

  spl <- split_mctable_for_sampling(mctable, mc_names = mc_names)
  mctable_in <- spl$mctable_in
  mctable_out <- spl$mctable_out

  factors <- as.character(mctable_in$mcnode)
  p <- length(factors)
  if (p == 0) {
    stop(
      "No sampled factors: all nodes were excluded or have missing sample_space"
    )
  }

  # sensobol 1.1.6 interface:
  # sobol_matrices(matrices = c("A", "B", "AB"), N, params, order = "first", type = "QRN", ...)
  U <- sensobol::sobol_matrices(
    matrices = matrices,
    N = N,
    params = factors,
    order = order,
    type = type,
    ...
  )

  U <- as.matrix(U)

  # Clamp U for safety (qnorm(0/1) = +/-Inf)
  # NOTE: pmin/pmax can drop dimensions when there is a single column.
  eps <- 1e-12
  Uc <- pmin(1 - eps, pmax(eps, U))
  Uc <- matrix(Uc, nrow = nrow(U), ncol = ncol(U), dimnames = dimnames(U))

  mc_func <- if ("mc_func" %in% names(mctable_in)) {
    as.character(mctable_in$mc_func)
  } else {
    rep(NA_character_, p)
  }
  sample_space <- as.character(mctable_in$sample_space)

  X <- Uc

  for (j in seq_len(p)) {
    func_j <- mc_func[j]
    bounds_j <- parse_sample_space_bounds(sample_space[j])

    # If func missing but bounds exist, assume uniform.
    if (is.na(func_j) || !nzchar(trimws(func_j))) {
      if (!is.null(bounds_j)) {
        func_j <- "runif"
      }
    }

    if (identical(func_j, "runif")) {
      if (is.null(bounds_j)) {
        stop(sprintf(
          "Missing numeric bounds for '%s' (runif requires min/max)",
          factors[j]
        ))
      }
      X[, j] <- bounds_j[["min"]] +
        (bounds_j[["max"]] - bounds_j[["min"]]) * Uc[, j]
      next
    }

    if (identical(func_j, "rnorm")) {
      parsed_ss <- parse_sample_space(sample_space[j])
      if (identical(parsed_ss$kind, "named")) {
        vals <- parsed_ss$values
        if (
          all(c("mean", "sd") %in% names(vals)) &&
            all(vapply(vals[c("mean", "sd")], is.numeric, logical(1)))
        ) {
          X[, j] <- stats::qnorm(
            Uc[, j],
            mean = as.numeric(vals$mean),
            sd = as.numeric(vals$sd)
          )
          next
        }
      }

      # Fallback: if rnorm parameters are not available but numeric bounds are,
      # treat as uniform instead of erroring.
      if (!is.null(bounds_j)) {
        X[, j] <- bounds_j[["min"]] +
          (bounds_j[["max"]] - bounds_j[["min"]]) * Uc[, j]
        next
      }

      stop(sprintf(
        "sample_space for '%s' must provide mean and sd for rnorm",
        factors[j]
      ))
    }

    if (identical(func_j, "rpert")) {
      if (!requireNamespace("mc2d", quietly = TRUE)) {
        stop(
          "mc_func 'rpert' requires the 'mc2d' package for qpert().\n\nInstall it using:\ninstall.packages('mc2d')"
        )
      }

      parsed_ss <- parse_sample_space(sample_space[j])
      if (identical(parsed_ss$kind, "named")) {
        vals <- parsed_ss$values
        if (
          all(c("min", "mode", "max") %in% names(vals)) &&
            all(vapply(vals[c("min", "mode", "max")], is.numeric, logical(1)))
        ) {
          shape <- if ("shape" %in% names(vals) && is.numeric(vals$shape)) {
            as.numeric(vals$shape)
          } else {
            4
          }

          X[, j] <- mc2d::qpert(
            p = Uc[, j],
            min = as.numeric(vals$min),
            mode = as.numeric(vals$mode),
            max = as.numeric(vals$max),
            shape = shape
          )
          next
        }
      }

      # Fallback: if PERT parameters are incomplete (e.g. mode missing) but
      # numeric bounds are available, treat as uniform instead of erroring.
      if (!is.null(bounds_j)) {
        X[, j] <- bounds_j[["min"]] +
          (bounds_j[["max"]] - bounds_j[["min"]]) * Uc[, j]
        next
      }

      stop(sprintf(
        "sample_space for '%s' must provide min, mode, and max for rpert",
        factors[j]
      ))
    }

    stop(sprintf("Unsupported mc_func '%s' for '%s'", func_j, factors[j]))
  }

  X
}
