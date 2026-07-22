
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
      warning("No frequency data for word length ", wl, " - returning NA.")
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

#' Compute phoneme n-phone frequency for a phoneme sequence
#'
#' Internal function underlying \code{\link{supf}}, \code{\link{sbpf}},
#' \code{\link{mlupf}}, and \code{\link{mlbpf}}. Looks up the frequency of
#' each phoneme n-phone from a precomputed cache and returns a summary value.
#'
#' ARPAbet stress markers (trailing 0, 1, 2) are stripped automatically before
#' lookup.
#'
#' @param phoneme_string A character string of space-separated ARPAbet phonemes
#'   (e.g. \code{"K AE1 T"}). Stress markers are stripped automatically.
#' @param n Integer. N-phone size: 1 = uniphones, 2 = biphones, 3 = triphones.
#' @param cache A named list as produced by \code{\link{build_nphone_cache}}.
#'   Positional caches must contain \code{matrices} and \code{length_counts};
#'   non-positional caches must contain \code{by_length}, \code{overall}, and
#'   \code{length_counts}.
#' @param positional Logical. If \code{TRUE}, look up each n-gram at its
#'   position in the sequence. If \code{FALSE}, use position-independent
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
compute_phoneme_frequency <- function(phoneme_string,
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

  tokens     <- tokenise_arpabet(phoneme_string)

  if (!all(tokens %in% ARPABET)) {
    invalid <- tokens[!tokens %in% ARPABET]
    stop("Input contains tokens that are not valid ARPAbet phonemes: ",
         paste(unique(invalid), collapse = ", "),
         ". This function expects a phoneme sequence (e.g. \"K AE T\"), not ",
         "an orthographic word. Use get_phoneme_score() to score a word directly.")
  }

  sl         <- length(tokens)
  n_positions <- sl - (n - 1L)

  if (n_positions < 1L) stop("Phoneme sequence is too short for n = ", n, ".")

  # build n-gram keys as hyphen-joined tokens
  ngram_keys <- vapply(seq_len(n_positions),
                       function(i) paste(tokens[i:(i + n - 1L)], collapse = "-"),
                       character(1))

  # lookup
  if (positional) {
    if (!is.list(cache) || !"matrices" %in% names(cache)) {
      stop("Positional cache must be a list with 'matrices' and 'length_counts' elements.")
    }
    mat <- cache$matrices[[as.character(sl)]]
    if (is.null(mat)) {
      warning("No frequency data for sequence length ", sl, " - returning NA.")
      return(NA_real_)
    }
    freqs <- mat[cbind(ngram_keys, paste0("pos", seq_len(n_positions)))]
  } else {
    if (!is.list(cache) || !"by_length" %in% names(cache)) {
      stop("Non-positional cache must be a list with 'by_length', 'overall', ",
           "and 'length_counts' elements.")
    }
    if (scope == "by_length") {
      sl_cache <- cache$by_length[[as.character(sl)]]
      if (is.null(sl_cache)) stop("No frequency data for sequence length ", sl, ".")
      freqs <- sl_cache[ngram_keys]
    } else {
      freqs <- cache$overall[ngram_keys]
    }
  }

  freqs[is.na(freqs)] <- 0

  # proportions
  if (count == "prop") {
    if (scope == "overall") {
      n_sequences <- sum(cache$length_counts)
    } else {
      n_sequences <- cache$length_counts[as.character(sl)]
      if (is.na(n_sequences)) stop("No sequence count for length ", sl, ".")
    }
    freqs <- freqs / n_sequences
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
         "raw"  = setNames(freqs, ngram_keys)
  )
}

#' Summed Uni-Phone Frequency
#'
#' Computes the summed positional uniphoneme frequency (SUPF) for an
#' ARPAbet phoneme sequence. Phoneme frequencies are looked up from a
#' pre-built cache and summed across all positions. Corresponds to the
#' positional segment frequency measure of Vitevitch and Luce (1998, 2004).
#'
#' @param phoneme_string A character string of space-separated ARPAbet phonemes
#'   (e.g. \code{"K AE1 T"}). Stress markers are stripped automatically.
#' @param cache Optional cache. If \code{NULL}, uses the default CMU phoneme
#'   unigram cache.
#' @param positional Logical. If \code{TRUE} (default), uses positional phoneme
#'   frequencies.
#' @param scope One of \code{"by_length"} (default) or \code{"overall"}. Only
#'   applies when \code{positional = FALSE}.
#' @param count One of \code{"prop"} (default) or \code{"freq"}.
#' @param normalize Logical. If \code{TRUE}, divides by number of phoneme
#'   positions. Default \code{FALSE}.
#' @param zero_handling One of \code{"raw"} (default), \code{"na"},
#'   \code{"laplace"}, \code{"minimum"}.
#'
#' @return Numeric SUPF score.
#'
#' @references
#' Vitevitch, M. S., & Luce, P. A. (1998). When words compete: Levels of
#' processing in perception of spoken words. \emph{Psychological Science,
#' 9}(4), 325--329.
#'
#' Vitevitch, M. S., & Luce, P. A. (2004). A web-based interface to calculate
#' phonotactic probability for words and nonwords in English.
#' \emph{Behavior Research Methods, Instruments, & Computers, 36}(3), 481--487.
#'
#' @export
supf <- function(phoneme_string,
                 cache         = NULL,
                 positional    = TRUE,
                 scope         = c("by_length", "overall"),
                 count         = c("prop", "freq"),
                 normalize     = FALSE,
                 zero_handling = c("raw", "na", "laplace", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::cmu_phoneme_cache
    else            grams::cmu_phoneme_nonpos_cache
  }

  compute_phoneme_frequency(phoneme_string, n = 1L, cache = cache,
                            positional    = positional,
                            scope         = match.arg(scope),
                            count         = match.arg(count),
                            normalize     = normalize,
                            log           = FALSE,
                            zero_handling = match.arg(zero_handling),
                            output        = "sum")
}

#' Mean Log Uniphone Frequency
#'
#' Computes the mean log positional uniphone frequency (MLUPF) for an
#' ARPAbet phoneme sequence. Uses raw frequencies with Laplace smoothing
#' (adding 1 before logging). More robust to skewed frequency distributions
#' than SUPF.
#'
#' @inheritParams supf
#' @param zero_handling One of \code{"laplace"} (default), \code{"na"},
#'   \code{"minimum"}.
#'
#' @return Numeric MLPUF score.
#'
#' @references
#' Vitevitch, M. S., & Luce, P. A. (1998). When words compete: Levels of
#' processing in perception of spoken words. \emph{Psychological Science,
#' 9}(4), 325--329.
#'
#' @export
mlupf <- function(phoneme_string,
                  cache         = NULL,
                  positional    = TRUE,
                  scope         = c("by_length", "overall"),
                  zero_handling = c("laplace", "na", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::cmu_phoneme_cache
    else            grams::cmu_phoneme_nonpos_cache
  }

  compute_phoneme_frequency(phoneme_string, n = 1L, cache = cache,
                            positional    = positional,
                            scope         = match.arg(scope),
                            count         = "freq",
                            normalize     = FALSE,
                            log           = TRUE,
                            zero_handling = match.arg(zero_handling),
                            output        = "mean")
}

#' Summed Bi-Phone Frequency
#'
#' Computes the summed positional biphone frequency (SBPF) for an ARPAbet
#' phoneme sequence. Biphone frequencies are looked up from a pre-built cache
#' and summed across all positions. Corresponds to the biphone frequency
#' measure of Vitevitch and Luce (1998, 2004).
#'
#' @inheritParams supf
#'
#' @return Numeric SBPF score.
#'
#' @references
#' Vitevitch, M. S., & Luce, P. A. (1998). When words compete: Levels of
#' processing in perception of spoken words. \emph{Psychological Science,
#' 9}(4), 325--329.
#'
#' Vitevitch, M. S., & Luce, P. A. (2004). A web-based interface to calculate
#' phonotactic probability for words and nonwords in English.
#' \emph{Behavior Research Methods, Instruments, & Computers, 36}(3), 481--487.
#'
#' @export
sbpf <- function(phoneme_string,
                 cache         = NULL,
                 positional    = TRUE,
                 scope         = c("by_length", "overall"),
                 count         = c("prop", "freq"),
                 normalize     = FALSE,
                 zero_handling = c("raw", "na", "laplace", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::cmu_biphone_cache
    else            grams::cmu_biphone_nonpos_cache
  }

  compute_phoneme_frequency(phoneme_string, n = 2L, cache = cache,
                            positional    = positional,
                            scope         = match.arg(scope),
                            count         = match.arg(count),
                            normalize     = normalize,
                            log           = FALSE,
                            zero_handling = match.arg(zero_handling),
                            output        = "sum")
}

#' Mean Log Bi-Phone Frequency
#'
#' Computes the mean log positional biphone frequency (MLBPF) for an ARPAbet
#' phoneme sequence. Uses raw frequencies with Laplace smoothing (adding 1
#' before logging). The phonotactic analogue of \code{\link{mlbf}}, and the
#' recommended measure for comparing phonotactic probability across sequences
#' of different lengths.
#'
#' @inheritParams supf
#' @param zero_handling One of \code{"laplace"} (default), \code{"na"},
#'   \code{"minimum"}.
#'
#' @return Numeric MLBPF score.
#'
#' @references
#' Vitevitch, M. S., & Luce, P. A. (1998). When words compete: Levels of
#' processing in perception of spoken words. \emph{Psychological Science,
#' 9}(4), 325--329.
#'
#' @export
mlbpf <- function(phoneme_string,
                 cache         = NULL,
                 positional    = TRUE,
                 scope         = c("by_length", "overall"),
                 zero_handling = c("laplace", "na", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::cmu_biphone_cache
    else            grams::cmu_biphone_nonpos_cache
  }

  compute_phoneme_frequency(phoneme_string, n = 2L, cache = cache,
                            positional    = positional,
                            scope         = match.arg(scope),
                            count         = "freq",
                            normalize     = FALSE,
                            log           = TRUE,
                            zero_handling = match.arg(zero_handling),
                            output        = "mean")
}

#' Summed Tri-Phone Frequency
#'
#' Computes the summed positional triphone frequency (STPF) for an ARPAbet
#' phoneme sequence. Triphone frequencies are looked up from a pre-built cache
#' and summed across all positions.
#'
#' @inheritParams supf
#'
#' @return Numeric STPF score.
#'
#' @export
stpf <- function(phoneme_string,
                 cache         = NULL,
                 positional    = TRUE,
                 scope         = c("by_length", "overall"),
                 count         = c("prop", "freq"),
                 normalize     = FALSE,
                 zero_handling = c("raw", "na", "laplace", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::cmu_triphone_cache
    else            grams::cmu_triphone_nonpos_cache
  }

  compute_phoneme_frequency(phoneme_string, n = 3L, cache = cache,
                            positional    = positional,
                            scope         = match.arg(scope),
                            count         = match.arg(count),
                            normalize     = normalize,
                            log           = FALSE,
                            zero_handling = match.arg(zero_handling),
                            output        = "sum")
}

#' Mean Log Tri-Phone Frequency
#'
#' Computes the mean log positional triphone frequency (MLTPF) for an ARPAbet
#' phoneme sequence. Uses raw frequencies with Laplace smoothing (adding 1
#' before logging).
#'
#' @inheritParams supf
#' @param zero_handling One of \code{"laplace"} (default), \code{"na"},
#'   \code{"minimum"}.
#'
#' @return Numeric MLTPF score.
#'
#' @export
mltpf <- function(phoneme_string,
                  cache         = NULL,
                  positional    = TRUE,
                  scope         = c("by_length", "overall"),
                  zero_handling = c("laplace", "na", "minimum"))
{

  if (is.null(cache)) {
    cache <- if (positional) grams::cmu_triphone_cache
    else            grams::cmu_triphone_nonpos_cache
  }

  compute_phoneme_frequency(phoneme_string, n = 3L, cache = cache,
                            positional    = positional,
                            scope         = match.arg(scope),
                            count         = "freq",
                            normalize     = FALSE,
                            log           = TRUE,
                            zero_handling = match.arg(zero_handling),
                            output        = "mean")
}
