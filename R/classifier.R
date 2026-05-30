
#' Create a Mahalanobis distance classifier from a reference corpus
#'
#' Computes feature vectors for a corpus of words, then derives the mean,
#' covariance matrix, and distance threshold needed for classification.
#'
#' @param words Character vector of reference corpus words.
#' @param features Either a character vector of column names to extract from
#'   \code{feature_df} (requires \code{feature_df} to be supplied), or a named
#'   list of functions each taking a single word string and returning a numeric
#'   value (e.g. \code{list(lbf = lbf, old20 = old20)}).
#' @param feature_df Optional data frame with pre-computed features, which must
#'   include a \code{word} column matching entries in \code{words}. If supplied,
#'   \code{features} must be a character vector of column names to extract.
#' @param percentile Percentile of reference distances to use as threshold.
#'   Default 0.95 (95th percentile).
#' @param verbose Logical. Print progress messages? Default TRUE.
#'
#' @return A list with components:
#'   \describe{
#'     \item{center}{Named numeric vector of feature means.}
#'     \item{cov}{Covariance matrix of features.}
#'     \item{threshold}{Mahalanobis distance threshold.}
#'     \item{n_words}{Number of words used to build the classifier.}
#'     \item{features}{Character vector of feature names.}
#'   }
#'
#' @export
create_classifier <- function(words,
                              features,
                              feature_df = NULL,
                              percentile = 0.95,
                              verbose = TRUE)
{

  # if pre-computed features provided
  if (!is.null(feature_df)) {
    if (!is.character(features)) {
      stop("When feature_df is provided, features must be column names")
    }
    if (!"word" %in% names(feature_df)) {
      stop("feature_df must have a 'word' column")
    }

    # filter to words in corpus and extract features
    feature_matrix <- feature_df |>
      dplyr::filter(word %in% words) |>
      dplyr::select(dplyr::all_of(features)) |>
      as.matrix()

  } else {
    # compute features on the fly using function list
    if (!is.list(features) || is.null(names(features))) {
      stop("When feature_df is NULL, features must be a named list of functions")
    }

    n_words <- length(words)
    feature_matrix <- sapply(seq_along(words), function(i) {
      w <- words[i]
      if (verbose && (i %% 500 == 0 || i == 1 || i == n_words)) {
        message(sprintf("[%d / %d] %s", i, n_words, w))
      }
      sapply(features, function(fn) fn(w))
    }) |>
      t()

    colnames(feature_matrix) <- names(features)
  }

  # compute classifier components
  center <- colMeans(feature_matrix, na.rm = TRUE)
  cov_mat <- cov(feature_matrix, use = "complete.obs")
  distances <- mahalanobis(feature_matrix, center = center, cov = cov_mat)
  threshold <- unname(quantile(distances, percentile, na.rm = TRUE))

  if (verbose) {
    message("Classifier built:")
    message("  N words: ", nrow(feature_matrix))
    message("  Features: ", paste(colnames(feature_matrix), collapse = ", "))
    message("  Threshold (", percentile * 100, "th percentile): ",
            round(threshold, 3))
  }

  list(
    center = center,
    cov = cov_mat,
    threshold = threshold,
    n_words = nrow(feature_matrix),
    features = colnames(feature_matrix)
  )
}

#' Classify a string as word, pseudoword, or nonword
#'
#' Uses a Mahalanobis distance classifier to determine whether a string is
#' a dictionary word, a pseudoword (wordlike), or a nonword (unwordlike).
#'
#' @param string A string to classify.
#' @param classifier A classifier object created by `create_classifier()`,
#'   or NULL to use default SUBTLEX-UK classifier.
#' @param sig_index Signature index for dictionary lookup. Defaults to the
#'   package internal index.
#' @param full Logical. If TRUE returns full feature scores and distance
#'   alongside the label. Default FALSE.
#'
#' @return If full = FALSE, a character string: "word", "pseudoword", or
#'   "nonword". If full = TRUE, a named list with scores, distance, and label.
#' @export
classify_string <- function(string,
                            classifier = NULL,
                            sig_index = grams::sig_index,
                            full = FALSE) {

  # use default SUBTLEX classifier if none provided
  if (is.null(classifier)) {
    classifier <- list(
      center = grams::classifier_center,
      cov = grams::classifier_cov,
      threshold = grams::classifier_threshold,
      features = c("mlbf", "old20", "articulability", "vowel_ratio")
    )
  }

  # compute features (assume feature names match function names)
  scores <- sapply(classifier$features, function(feat) {
    fn <- get(feat, mode = "function")
    fn(string)
  })
  names(scores) <- classifier$features

  # compute distance and classify
  d <- mahalanobis(scores, center = classifier$center, cov = classifier$cov)

  label <- if (tolower(string) %in% unlist(sig_index)) {
    "word"
  } else if (d <= classifier$threshold) {
    "pseudoword"
  } else {
    "nonword"
  }

  if (full) {
    list(
      scores = round(scores, 4),
      distance = round(d, 3),
      label = label
    )
  } else {
    label
  }
}
