# Shared plotting and model-prediction helpers

# Add an alpha channel to one or more R colors.
add.alpha <- function(col, alpha = 1) {
  if (missing(col)) {
    stop("Please provide a vector of colors.")
  }

  apply(
    sapply(col, col2rgb) / 255,
    2,
    function(rgb_values) {
      rgb(rgb_values[1], rgb_values[2], rgb_values[3], alpha = alpha)
    }
  )
}

# Re-center a response curve at a specified x/y coordinate.
center <- function(column, x_range, center_x, center_y) {
  column - column[which(x_range == center_x)] + center_y
}

# Convert bootstrapped response curves into a median and 95% interval.
get_response_ci <- function(felm_mod_boot, x_range, x_bar, y_bar, x_full) {
  bootstrap_curves <- apply(
    felm_mod_boot$boot,
    2,
    function(curve) center(curve, x_range, round(x_bar), y_bar)
  )

  response <- tibble::tibble(
    x = x_range,
    y = apply(bootstrap_curves, 1, median),
    ci_low = apply(bootstrap_curves, 1, quantile, probs = 0.025),
    ci_high = apply(bootstrap_curves, 1, quantile, probs = 0.975)
  )

  ci <- tibble::tibble(
    px = c(response$x[1], response$x, rev(response$x)),
    py = c(response$ci_low[1], response$ci_high, rev(response$ci_low))
  )

  list(response = response, ci = ci)
}

# Calculate bin boundaries and heights for compact marginal histograms.
get_hst_obj <- function(values, min, max, width, cutoff = 1, type = "count") {
  breaks <- seq(min, max, width)
  histogram <- data.frame(
    left = breaks - width / 2,
    right = breaks + width / 2,
    count = NA_real_
  )

  for (index in seq_len(nrow(histogram))) {
    histogram$count[index] <- sum(
      values > histogram$left[index] & values <= histogram$right[index],
      na.rm = TRUE
    )
  }

  histogram$count[1] <- histogram$count[1] +
    sum(values < histogram$left[1], na.rm = TRUE)
  last <- nrow(histogram)
  histogram$count[last] <- histogram$count[last] +
    sum(values > histogram$right[last], na.rm = TRUE)

  if (type == "share") {
    histogram$count <- histogram$count / sum(histogram$count, na.rm = TRUE)
  }

  subset(histogram, right <= quantile(values, cutoff, na.rm = TRUE))
}

# Draw a compact histogram from get_hst_obj() output.
plotHist <- function(hst_obj, col, alpha, bottom, height, border.col = col) {
  rect(
    xleft = hst_obj$left,
    xright = hst_obj$right,
    ybottom = bottom,
    ytop = (hst_obj$count / max(hst_obj$count)) * height + bottom,
    col = add.alpha(col, alpha),
    border = border.col
  )
}

# Draw vertical error bars. The historical function name is retained because
# it is used by the recovered figure scripts.
error.bar <- function(x, y, upper, lower = upper, length = 0.04, col = "gray30") {
  arrows(x, upper, x, lower, angle = 90, code = 3, length = length, col = col)
}

# Prediction method for lfe::felm models. This routine was adapted in the
# original analysis from code by Kendon Bell (github.com/kendonB).
predict.felm <- function(
  object,
  newdata,
  se.fit = FALSE,
  interval = "none",
  level = 0.95
) {
  if (missing(newdata)) {
    stop("predict.felm requires newdata and assumes all group effects are zero.")
  }

  model_terms <- delete.response(terms(object))
  attr(model_terms, "intercept") <- 0
  model_matrix <- model.matrix(model_terms, data = newdata)
  fit <- data.frame(fit = as.vector(model_matrix %*% object$coef))

  if (se.fit || interval != "none") {
    covariance <- if (!is.null(object$clustervcv)) {
      object$clustervcv
    } else if (!is.null(object$robustvcv)) {
      object$robustvcv
    } else if (!is.null(object$vcv)) {
      object$vcv
    } else {
      stop("No covariance matrix is attached to the felm object.")
    }

    standard_error <- sqrt(diag(model_matrix %*% covariance %*% t(model_matrix)))
  }

  if (interval == "confidence") {
    critical_value <- qt((1 - level) / 2 + level, df = object$df.residual)
    fit$lwr <- fit$fit - critical_value * standard_error
    fit$upr <- fit$fit + critical_value * standard_error
  } else if (interval == "prediction") {
    stop("interval = 'prediction' is not implemented")
  }

  if (se.fit) {
    list(fit = fit, se.fit = standard_error)
  } else {
    fit
  }
}
