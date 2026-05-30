
#' Compute n-gram frequency for a word
#'
#' Internal function underlying \code{\link{slf}}, \code{\link{sbf}},
#' \code{\link{stf}}, \code{\link{mlbf}}, \code{\link{mllf}}, and
#' \code{\link{mltf}}. Looks up the frequency of each n-gram from a
#' precomputed cache and returns a summary value.
#'
#' @param word A character string. Will be converted to uppercase.
#' @param n Integer. N-gram size: 1 = letters, 2 = bigrams, 3 = trigrams.
#' @param cache A named list as produced by \code{\link{build_ngram_cache}}.
#'   Positional caches must contain \code{matrices} and \code{length_counts};
#'   non-positional caches must contain \code{by_length}, \code{overall}, and
#'   \code{length_counts}.
#' @param positional Logical. If \code{TRUE}, look up each n-gram at its
#'   position in the word. If \code{FALSE}, use position-independent
#'   frequencies.
#' @param scope One of \code{"by_length"} or \code{"overall"}. Whether to
#'   use length-specific or corpus-wide frequencies. Ignored when
#'   \code{positional = TRUE}.
#' @param count One of \code{"freq"} or \code{"prop"}. Whether to use raw
#'   counts or proportions relative to the corpus size.
#' @param normalize Logical. If \code{TRUE}, divide frequencies by the number
#'   of n-gram positions before aggregating.
#' @param log Logical. If \code{TRUE}, log-transform frequencies before
#'   aggregating.
#' @param zero_handling One of \code{"raw"}, \code{"na"}, \code{"laplace"},
#'   or \code{"minimum"}. How to handle zero-frequency n-grams. \code{"raw"}
#'   leaves zeros as-is; \code{"na"} replaces them with \code{NA};
#'   \code{"laplace"} adds 1 to all frequencies; \code{"minimum"} replaces
#'   zeros with the smallest observed non-zero frequency.
#' @param output One of \code{"sum"}, \code{"mean"}, or \code{"raw"}.
#'   Whether to return the sum, mean, or a named vector of individual
#'   n-gram frequencies.
#'
#' @return A single numeric value (\code{output = "sum"} or \code{"mean"}),
#'   or a named numeric vector of per-n-gram frequencies
#'   (\code{output = "raw"}).
#'
#' @keywords internal
compute_ngram_frequency <- function(word,
                                    n,
                                    cache,
                                    positional    = TRUE,
                                    scope         = c("by_length", "overall"),
                                    count         = c("freq", "prop"),
                                    normalize     = FALSE,
                                    log           = FALSE,
                                    zero_handling = c("raw", "na",
                                                      "laplace", "minimum"),
                                    output        = c("sum", "mean", "raw"))
{

  scope         <- match.arg(scope)
  count         <- match.arg(count)
  zero_handling <- match.arg(zero_handling)
  output        <- match.arg(output)

  word        <- toupper(word)
  wl          <- nchar(word)
  chars       <- strsplit(word, "", fixed = TRUE)[[1]]
  n_positions <- wl - (n - 1L)

  if (n_positions < 1L) stop("Word is too short for n = ", n, ".")

  ngrams <- vapply(seq_len(n_positions),
                   function(i) paste0(chars[i:(i + n - 1L)], collapse = ""),
                   character(1))

  # lookup
  if (positional) {
    if (!is.list(cache) || !"matrices" %in% names(cache)) {
      stop("Positional cache must be a list with 'matrices' and 'length_counts' elements.")
    }
    mat <- cache$matrices[[as.character(wl)]]
    if (is.null(mat)) {
      warning("No frequency data for word length ", wl, " — returning NA.")
      return(NA_real_)
    }
    freqs <- mat[cbind(ngrams, paste0("pos", seq_len(n_positions)))]
  } else {
    if (!is.list(cache) || !"by_length" %in% names(cache)) {
      stop("Non-positional cache must be a list with 'by_length', 'overall', ",
           "and 'length_counts' elements.")
    }
    if (scope == "by_length") {
      wl_cache <- cache$by_length[[as.character(wl)]]
      if (is.null(wl_cache)) stop("No frequency data for word length ", wl, ".")
      freqs <- wl_cache[ngrams]
    } else {
      freqs <- cache$overall[ngrams]
    }
  }

  freqs[is.na(freqs)] <- 0

  # proportions
  if (count == "prop") {
    if (scope == "overall") {
      n_words <- sum(cache$length_counts)  # total corpus size
    } else {
      n_words <- cache$length_counts[as.character(wl)]  # length specific
      if (is.na(n_words)) stop("No word count for length ", wl, ".")
    }
    freqs <- freqs / n_words
  }

  # normalize by n_positions
  if (normalize) freqs <- freqs / n_positions

  # zero handling
  freqs <- switch(zero_handling,
                  "raw"     = freqs,
                  "na"      = { freqs[freqs == 0] <- NA; freqs },
                  "laplace" = freqs + 1,
                  "minimum" = {
                    m <- min(freqs[freqs > 0], na.rm = TRUE)
                    if (is.infinite(m)) m <- 0
                    freqs[freqs == 0] <- m
                    freqs
                  }
  )

  # log transform
  if (log) {
    if (zero_handling == "raw")
      warning("zero_handling = 'raw' with log = TRUE produces -Inf values.")
    freqs <- log(freqs)
  }

  # aggregate
  switch(output,
         "sum"  = sum(freqs,  na.rm = TRUE),
         "mean" = mean(freqs, na.rm = TRUE),
         "raw"  = setNames(freqs, ngrams)
  )
}

#' Summed Bigram Frequency
#'
#' Computes the summed bigram frequency (SBF) for a word. Bigram frequencies
#' are looked up from a pre-built cache and summed across all positions.
#'
#' @param word A word or anagram as a string.
#' @param cache Optional cache. If NULL, uses the default subtlex_uk cache.
#' @param positional Logical. If TRUE (default), uses positional bigram frequencies.
#' @param scope One of "by_length" (default) or "overall". Only applies when
#'   positional = FALSE.
#' @param count One of "prop" (default) or "freq".
#' @param normalize Logical. If TRUE, divides by number of bigram positions.
#'   Default FALSE.
#' @param zero_handling One of "raw" (default), "na", "laplace", "minimum".
#'
#' @return Numeric SBF score.
#' @export
sbf <- function(word,
                cache         = NULL,
                positional    = TRUE,
                scope         = c("by_length", "overall"),
                count         = c("prop", "freq"),
                normalize     = FALSE,
                zero_handling = c("raw", "na", "laplace", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::subtlex_uk_bigram_cache
    else            grams::subtlex_uk_bigram_nonpos_cache
  }

  compute_ngram_frequency(word, n = 2L, cache = cache,
                          positional    = positional,
                          scope         = match.arg(scope),
                          count         = match.arg(count),
                          normalize     = normalize,
                          log           = FALSE,
                          zero_handling = match.arg(zero_handling),
                          output        = "sum")
}

#' Mean Log Bigram Frequency
#'
#' Computes the mean log bigram frequency (MLBF) for a word, equivalent to
#' the log of the geometric mean bigram frequency. Uses raw frequencies with
#' Laplace smoothing (adding 1 before logging), analogous to Zipf score
#' computation in SUBTLEX. More robust to skewed frequency distributions
#' than SBF.
#'
#' @param word A word or anagram as a string.
#' @param cache Optional cache. If NULL, uses the default subtlex_uk cache.
#' @param positional Logical. If TRUE (default), uses positional bigram frequencies.
#' @param scope One of "by_length" (default) or "overall". Only applies when
#'   positional = FALSE.
#' @param zero_handling One of "laplace" (default), "na", "minimum".
#'
#' @return Numeric LBF score.
#' @export
mlbf <- function(word,
                cache         = NULL,
                positional    = TRUE,
                scope         = c("by_length", "overall"),
                zero_handling = c("laplace", "na", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::subtlex_uk_bigram_cache
    else            grams::subtlex_uk_bigram_nonpos_cache
  }

  compute_ngram_frequency(word, n = 2L, cache = cache,
                          positional    = positional,
                          scope         = match.arg(scope),
                          count         = "freq",
                          normalize     = FALSE,
                          log           = TRUE,
                          zero_handling = match.arg(zero_handling),
                          output        = "mean")
}

#' Summed Letter Frequency
#'
#' Computes the summed letter frequency (SLF) for a word.
#'
#' @inheritParams sbf
#'
#' @return Numeric SLF score.
#' @export
slf <- function(word,
                cache         = NULL,
                positional    = TRUE,
                scope         = c("by_length", "overall"),
                count         = c("prop", "freq"),
                normalize     = FALSE,
                zero_handling = c("raw", "na", "laplace", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::subtlex_uk_letter_cache
    else            grams::subtlex_uk_letter_nonpos_cache
  }

  compute_ngram_frequency(word, n = 1L, cache = cache,
                          positional    = positional,
                          scope         = match.arg(scope),
                          count         = match.arg(count),
                          normalize     = normalize,
                          log           = FALSE,
                          zero_handling = match.arg(zero_handling),
                          output        = "sum")
}

#' Log Letter Frequency
#'
#' Computes the mean log letter frequency (MLLF) for a word.
#'
#' @inheritParams mlbf
#'
#' @return Numeric LLF score.
#' @export
mllf <- function(word,
                cache         = NULL,
                positional    = TRUE,
                scope         = c("by_length", "overall"),
                zero_handling = c("laplace", "na", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::subtlex_uk_letter_cache
    else            grams::subtlex_uk_letter_nonpos_cache
  }

  compute_ngram_frequency(word, n = 1L, cache = cache,
                          positional    = positional,
                          scope         = match.arg(scope),
                          count         = "freq",
                          normalize     = FALSE,
                          log           = TRUE,
                          zero_handling = match.arg(zero_handling),
                          output        = "mean")
}

#' Summed Trigram Frequency
#'
#' Computes the summed trigram frequency (STF) for a word.
#'
#' @inheritParams sbf
#'
#' @return Numeric STF score.
#' @export
stf <- function(word,
                cache         = NULL,
                positional    = TRUE,
                scope         = c("by_length", "overall"),
                count         = c("prop", "freq"),
                normalize     = FALSE,
                zero_handling = c("raw", "na", "laplace", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::subtlex_uk_trigram_cache
    else            grams::subtlex_uk_trigram_nonpos_cache
  }

  compute_ngram_frequency(word, n = 3L, cache = cache,
                          positional    = positional,
                          scope         = match.arg(scope),
                          count         = match.arg(count),
                          normalize     = normalize,
                          log           = FALSE,
                          zero_handling = match.arg(zero_handling),
                          output        = "sum")
}

#' Log Trigram Frequency
#'
#' Computes the mean log trigram frequency (LTF) for a word.
#'
#' @inheritParams mlbf
#'
#' @return Numeric LTF score.
#' @export
mltf <- function(word,
                cache         = NULL,
                positional    = TRUE,
                scope         = c("by_length", "overall"),
                zero_handling = c("laplace", "na", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::subtlex_uk_trigram_cache
    else            grams::subtlex_uk_trigram_nonpos_cache
  }

  compute_ngram_frequency(word, n = 3L, cache = cache,
                          positional    = positional,
                          scope         = match.arg(scope),
                          count         = "freq",
                          normalize     = FALSE,
                          log           = TRUE,
                          zero_handling = match.arg(zero_handling),
                          output        = "mean")
}

#' Bigram frequencies greater than zero
#'
#' Computes the proportion (or count) of possible bigram-position combinations
#' within a word that have a non-zero frequency in the corpus. Used as a metric
#' of anagram difficulty: words where more letter combinations form real bigrams
#' are easier to solve.
#'
#' All ordered letter pair permutations within the word are considered, not just
#' adjacent bigrams, reflecting the mental shuffling involved in solving.
#'
#' @param word A character string.
#' @param cache A named list of matrices as produced by
#'   \code{\link{build_ngram_cache}}. Defaults to the package cache.
#' @param output One of \code{"prop"}, \code{"norm"}, or \code{"count"}.
#'   \code{"prop"} returns the proportion of all bigram-position cells that are
#'   non-zero. \code{"norm"} returns the count normalised by word length.
#'   \code{"count"} returns the raw count of non-zero cells.
#'
#' @return A numeric value.
#'
#' @seealso \code{\link{sbf}}
#' @export
gtzero <- function(word,
                   cache  = grams::subtlex_uk_bigram_cache,
                   output = c("prop", "norm", "count"))
{
  output <- match.arg(output)
  word   <- toupper(word)
  wl     <- nchar(word)
  mat    <- cache$matrices[[as.character(wl)]]
  if (is.null(mat)) stop("No frequency data for word length ", wl, ".")

  chars       <- strsplit(word, "", fixed = TRUE)[[1]]
  n_positions <- wl - 1L

  # all ordered letter pair permutations within the word
  permutations        <- do.call(rbind, lapply(seq_along(chars), function(i) {
    cbind(chars[i], chars[-i])
  }))
  permutation_bigrams <- paste0(permutations[, 1], permutations[, 2])
  n_cells             <- length(permutation_bigrams) * n_positions

  # vectorised lookup of all bigram-position combinations at once
  bg_index  <- rep(permutation_bigrams, each = n_positions)
  pos_index <- rep(paste0("pos", seq_len(n_positions)),
                   times = length(permutation_bigrams))
  freqs     <- mat[cbind(bg_index, pos_index)]
  freqs[is.na(freqs)] <- 0
  gtz_count <- sum(freqs > 0)

  if (output == "count") return(gtz_count)
  if (output == "norm")  return(gtz_count / n_positions)
  if (output == "prop")  return(gtz_count / n_cells)
}
