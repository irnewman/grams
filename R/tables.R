#' Build and save n-gram frequency tables
#'
#' Computes n-gram frequency tables for a set of words across all combinations
#' of the supplied parameters, saves each table as a CSV file, and writes a
#' corresponding cache file for use by the frequency scoring functions. This is
#' typically a one-time setup step run before using \code{\link{sbf}},
#' \code{\link{slf}}, or \code{\link{stf}}.
#'
#' @param words A character vector of words to compute frequencies from.
#' @param n An integer vector of n-gram sizes to compute. \code{1} = letters,
#'   \code{2} = bigrams, \code{3} = trigrams. Defaults to \code{1:3}.
#' @param method A character vector of counting methods. \code{"type"} counts
#'   each word once regardless of corpus frequency; \code{"token"} weights by
#'   corpus frequency and requires \code{word_frequency} to be supplied.
#' @param corpus A string identifying the corpus (e.g. \code{"SUBTLEX-UK"}).
#'   Used to name output files and subdirectories.
#' @param word_frequency A numeric vector of corpus frequencies corresponding
#'   to \code{words}, required when \code{"token"} is included in
#'   \code{method}. Ignored for \code{"type"} counts.
#' @param word_length_range An integer vector of length 2 giving the minimum
#'   and maximum word lengths to include. Defaults to \code{c(3, 10)}.
#' @param positional Logical. If \code{TRUE}, compute position-specific
#'   frequencies. If \code{FALSE}, compute position-independent frequencies.
#'   Both can be requested by passing \code{c(TRUE, FALSE)}.
#' @param path Directory where CSV frequency tables will be saved. A
#'   subdirectory named after \code{corpus} will be created automatically.
#'   Defaults to \code{tables/} in the current working directory.
#' @param cache_path Directory where cache files will be saved. Defaults to
#'   \code{cache/} in the current working directory.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effects of
#'   writing CSV and cache files to disk.
#'
#' @seealso \code{\link{build_ngram_cache}}, \code{\link{tally_ngram_frequencies}}
#'
#' @examples
#' \dontrun{
#' build_ngram_tables(
#'   words          = my_words,
#'   n              = 1:3,
#'   method         = c("type", "token"),
#'   corpus         = "SUBTLEX-UK",
#'   word_frequency = my_frequencies
#' )
#' }
#'
#' @export
build_ngram_tables <- function(words,
                              n                 = 1:3,
                              method            = c("type", "token"),
                              corpus,
                              word_frequency    = NULL,
                              word_length_range = c(3, 10),
                              positional        = TRUE,
                              path              = file.path(getwd(), "tables"),
                              cache_path        = file.path(getwd(), "cache"))
{

  if (is.null(word_frequency) && "token" %in% method) {
    warning('`word_frequency` not provided - removing "token" from method.')
    method <- setdiff(method, "token")
  }

  params <- expand.grid(n          = n,
                        method     = method,
                        positional = positional,
                        stringsAsFactors = FALSE)

  cat(sprintf("Building frequency tables for '%s': %d parameter combination(s).\n",
              corpus, nrow(params)))

  ngram_names <- c("1" = "letter", "2" = "bigram", "3" = "trigram")

  for (i in seq_len(nrow(params))) {

    ngram_name <- if (as.character(params$n[i]) %in% names(ngram_names)) {
      ngram_names[[as.character(params$n[i])]]
    } else {
      paste0(params$n[i], "gram")
    }
    pos_label <- if (params$positional[i]) "" else "_nonpos"

    cat(sprintf("\n[%d / %d]  n=%d  method=%s  positional=%s\n",
                i, nrow(params), params$n[i], params$method[i],
                params$positional[i]))

    freq_table <- tally_ngram_frequencies(
      words             = words,
      n                 = params$n[i],
      method            = params$method[i],
      word_frequency    = if (params$method[i] == "token") word_frequency else NULL,
      word_length_range = word_length_range,
      corpus            = corpus,
      positional        = params$positional[i]
    )

    # save CSV
    filename <- paste0(corpus, "_", ngram_name, "_", params$method[i], pos_label)
    if (!dir.exists(file.path(path, corpus))) {
      dir.create(file.path(path, corpus), recursive = TRUE)
    }
    write.csv(freq_table,
              file      = file.path(path, corpus, paste0(filename, ".csv")),
              row.names = FALSE)

    # save cache
    build_ngram_cache(
      freq_table = freq_table,
      n          = params$n[i],
      method     = params$method[i],
      positional = params$positional[i],
      save       = TRUE,
      path       = cache_path
    )
  }

  cat(sprintf("\nDone. Tables saved to: %s/%s\nCaches saved to: %s\n",
              path, corpus, cache_path))

  invisible(NULL)
}

#' Tally n-gram frequencies across a set of words
#'
#' Counts how often each n-gram occurs across a set of words, either by type
#' (each word counted once) or by token (weighted by corpus frequency). Results
#' are stratified by word length and returned as a single data frame. This is
#' the core computation underlying \code{\link{build_ngram_tables}}.
#'
#' @param words A character vector of words to tally.
#' @param n Integer. N-gram size: \code{1} = letters, \code{2} = bigrams,
#'   \code{3} = trigrams. Defaults to \code{1L}.
#' @param method One of \code{"type"} or \code{"token"}. \code{"type"} counts
#'   each word once; \code{"token"} weights each word by its corpus frequency,
#'   requiring \code{word_frequency} to be supplied.
#' @param corpus An optional string identifying the corpus (e.g.
#'   \code{"SUBTLEX-UK"}). Stored as a metadata column in the output. If
#'   \code{NULL}, the corpus column will be \code{NA}.
#' @param word_frequency A numeric vector of corpus frequencies corresponding
#'   to \code{words}, required when \code{method = "token"}. Must be the same
#'   length as \code{words}.
#' @param word_length_range An optional integer vector of length 2 giving the
#'   minimum and maximum word lengths to include. Words outside this range are
#'   silently dropped before tallying.
#' @param positional Logical. If \code{TRUE}, frequencies are tallied
#'   separately for each position in the word, producing one column per
#'   position. If \code{FALSE}, frequencies are summed across all positions.
#'   Defaults to \code{TRUE}.
#'
#' @return A data frame with one row per n-gram per word length, containing
#'   metadata columns (\code{corpus}, \code{word_length}, \code{n_words},
#'   \code{ngram}, \code{method}) followed by either positional frequency
#'   columns (\code{pos1}, \code{pos2}, ...) when \code{positional = TRUE},
#'   or a single \code{freq} column when \code{positional = FALSE}.
#'
#' @seealso \code{\link{build_ngram_tables}}, \code{\link{build_ngram_cache}}
#'
#' @examples
#' \dontrun{
#' tally_ngram_frequencies(
#'   words  = my_words,
#'   n      = 2L,
#'   method = "type",
#'   corpus = "SUBTLEX-UK"
#' )
#' }
#'
#' @export
tally_ngram_frequencies <- function(words,
                                    n                 = 1L,
                                    method            = c("type", "token"),
                                    corpus            = NULL,
                                    word_frequency    = NULL,
                                    word_length_range = NULL,
                                    positional        = TRUE)
{

  method <- match.arg(method)

  if (!is.null(word_frequency) && method == "type") {
    warning('`word_frequency` provided but method = "type" - frequencies will not be used.')
  }

  if (n > 3L) {
    warning("n = ", n, " will produce ", 26^n,
            " n-gram combinations. ",
            "This may require substantial time and memory.")
  }

  # filter by word length
  if (!is.null(word_length_range)) {
    if (length(word_length_range) != 2L || !is.numeric(word_length_range)) {
      stop("`word_length_range` must be a numeric vector of length 2.")
    }
    keep <- nchar(words) >= word_length_range[1] &
      nchar(words) <= word_length_range[2]
    if (!is.null(word_frequency)) word_frequency <- word_frequency[keep]
    words <- words[keep]
    if (length(words) == 0L) stop("No words remain after applying `word_length_range`.")
  }

  # frequency weights
  if (method == "type") {
    weights <- rep(1, length(words))
  } else {
    if (is.null(word_frequency) || length(word_frequency) != length(words)) {
      stop("`word_frequency` must be a numeric vector the same length as `words`.")
    }
    weights <- word_frequency
  }

  # n-gram column name
  ngram_name <- switch(as.character(n),
                       "1" = "letter",
                       "2" = "bigram",
                       "3" = "trigram",
                       paste0(n, "gram"))

  # all possible n-grams
  letter_grid <- do.call(expand.grid, rep(list(LETTERS), n))
  all_ngrams  <- do.call(paste0, rev(letter_grid))

  # position columns
  max_word_length <- max(nchar(words))
  max_n_positions <- max_word_length - (n - 1L)
  if (max_n_positions < 1L) stop("Longest word is too short for n = ", n, ".")
  pos_cols <- paste0("pos", seq_len(max_n_positions))

  word_lengths   <- nchar(words)
  unique_lengths <- sort(unique(word_lengths))
  results        <- vector("list", length(unique_lengths))

  cat(sprintf("Processing %d words across %d word length(s)...\n",
              length(words), length(unique_lengths)))

  for (word_length in unique_lengths) {

    word_indices   <- which(word_lengths == word_length)
    words_subset   <- words[word_indices]
    weights_subset <- weights[word_indices]
    n_words        <- length(words_subset)
    n_positions    <- word_length - (n - 1L)

    cat(sprintf("  [length %d]  %d words, %d position(s)\n", # update: replace with verbose()
                word_length, n_words, n_positions))

    if (n_positions < 1L) {
      warning("Skipping words of length ", word_length,
              ": too short for n = ", n, ".")
      next
    }

    # raw frequency matrix, no normalization
    freq_mat <- matrix(0L,
                       nrow     = length(all_ngrams),
                       ncol     = n_positions,
                       dimnames = list(all_ngrams,
                                       paste0("pos", seq_len(n_positions))))

    milestones <- unique(floor(seq(1, n_words, length.out = 11)))

    for (i in seq_along(words_subset)) {
      if (i %in% milestones) {
        cat(sprintf("    %3.0f%% complete (%d / %d)\n",
                    i / n_words * 100, i, n_words))
      }
      word_letters <- toupper(strsplit(words_subset[i], "", fixed = TRUE)[[1]])
      word_weight  <- weights_subset[i]
      for (pos in seq_len(n_positions)) {
        chars <- word_letters[pos:(pos + n - 1L)]
        if (!all(chars %in% LETTERS)) next
        ngram <- paste0(chars, collapse = "")
        freq_mat[ngram, pos] <- freq_mat[ngram, pos] + word_weight
      }
    }

    # build result table
    if (positional) {
      result_table <- as.data.frame(freq_mat)
      missing_cols <- setdiff(pos_cols, names(result_table))
      result_table[missing_cols] <- NA
      result_table[[ngram_name]] <- rownames(result_table)
      rownames(result_table)     <- NULL
      result_table               <- result_table[, c(ngram_name, pos_cols)]
    } else {
      result_table <- data.frame(
        ngram = all_ngrams,
        freq  = rowSums(freq_mat),  # raw sum, no normalization
        row.names = NULL
      )
      names(result_table)[1] <- ngram_name
    }

    if (!is.null(corpus)) result_table$corpus <-
      corpus else result_table$corpus <- NA
    result_table$word_length <- word_length
    result_table$n_words     <- n_words
    result_table$ngram       <- ngram_name
    result_table$method      <- method

    meta_cols  <- intersect(c("corpus", "word_length", "n_words", "ngram",
                              ngram_name, "method"), names(result_table))
    other_cols <- setdiff(names(result_table), meta_cols)
    results[[as.character(word_length)]] <-
      result_table[, c(meta_cols, other_cols)]
  }

  do.call(rbind, Filter(Negate(is.null), results))
}

#' Build an n-gram frequency cache
#'
#' Converts a frequency table produced by \code{\link{tally_ngram_frequencies}}
#' into a structured cache used by the frequency scoring functions. Positional
#' caches store a matrix per word length; non-positional caches store
#' length-specific and corpus-wide frequency vectors. Optionally saves the
#' cache to disk as an \code{.rds} file.
#'
#' @param freq_table A data frame as returned by
#'   \code{\link{tally_ngram_frequencies}}.
#' @param n Integer. N-gram size: \code{1} = letters, \code{2} = bigrams,
#'   \code{3} = trigrams. Must match the n-gram size used to produce
#'   \code{freq_table}. Defaults to \code{2L}.
#' @param method One of \code{"type"} or \code{"token"}. Must match the method
#'   used to produce \code{freq_table}.
#' @param positional Logical. If \code{TRUE}, builds a positional cache (a
#'   named list of matrices, one per word length). If \code{FALSE}, builds a
#'   non-positional cache (named frequency vectors by length and overall).
#'   Defaults to \code{TRUE}.
#' @param save Logical. If \code{TRUE}, saves the cache to disk as an
#'   \code{.rds} file at \code{path}. Defaults to \code{FALSE}.
#' @param path Directory where the cache file will be saved when
#'   \code{save = TRUE}. Created automatically if it does not exist. Defaults
#'   to \code{cache/} in the current working directory.
#' @param filename Optional string giving the base filename (without
#'   extension). If \code{NULL}, a name is constructed from the corpus,
#'   n-gram type, method, and positional flag.
#'
#' @return When \code{save = FALSE}, returns the cache list. When
#'   \code{save = TRUE}, saves the cache to disk and invisibly returns the
#'   cache list. Positional caches contain \code{matrices} (a named list of
#'   matrices) and \code{length_counts} (a named numeric vector).
#'   Non-positional caches contain \code{by_length} (a named list of frequency
#'   vectors), \code{overall} (a corpus-wide frequency vector), and
#'   \code{length_counts}.
#'
#' @seealso \code{\link{tally_ngram_frequencies}}, \code{\link{build_ngram_tables}}
#'
#' @examples
#' \dontrun{
#' freq_table <- tally_ngram_frequencies(
#'   words  = my_words,
#'   n      = 2L,
#'   method = "type",
#'   corpus = "SUBTLEX-UK"
#' )
#' cache <- build_ngram_cache(freq_table, n = 2L, method = "type")
#' }
#'
#' @export
build_ngram_cache <- function(freq_table,
                              n          = 2L,
                              method     = c("type", "token"),
                              positional = TRUE,
                              save       = FALSE,
                              path       = file.path(getwd(), "cache"),
                              filename   = NULL)
{

  method     <- match.arg(method)
  ngram_name <- switch(as.character(n),
                       "1" = "letter",
                       "2" = "bigram",
                       "3" = "trigram",
                       paste0(n, "gram"))

  ft <- freq_table[freq_table$method == method, ]
  if (nrow(ft) == 0L) stop("No rows found for method = '", method, "'.")

  # length_counts used by both positional and non-positional for proportions
  length_counts <- setNames(
    vapply(split(ft, ft$word_length),
           function(x) x$n_words[1], numeric(1)),
    as.character(sort(unique(ft$word_length)))
  )

  if (positional) {
    # positional: named list of matrices + length_counts
    pos_cols      <- grep("^pos", names(ft), value = TRUE)
    mat           <- as.matrix(ft[, pos_cols])
    rownames(mat) <- ft[[ngram_name]]

    matrices <- vector("list", length(unique(ft$word_length)))
    names(matrices) <- as.character(sort(unique(ft$word_length)))

    for (wl in unique(ft$word_length)) {
      n_valid     <- wl - (n - 1L)
      rows        <- ft$word_length == wl
      m           <- mat[rows, seq_len(n_valid), drop = FALSE]
      rownames(m) <- ft[[ngram_name]][rows]
      matrices[[as.character(wl)]] <- m
    }

    cache <- list(
      matrices      = matrices,
      length_counts = length_counts
    )

  } else { # non-positional: by_length, overall, length_counts

    # per word length named vectors of raw frequencies
    by_length <- lapply(split(ft, ft$word_length), function(wl_ft) {
      setNames(wl_ft$freq, wl_ft[[ngram_name]])
    })

    # overall: sum raw frequencies across all word lengths
    overall <- tapply(ft$freq, ft[[ngram_name]], sum)

    cache <- list(
      by_length     = by_length,
      overall       = overall,
      length_counts = length_counts
    )
  }

  if (save) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    if (is.null(filename)) {
      corpus    <- if ("corpus" %in% names(ft)) ft$corpus[1] else "corpus"
      pos_label <- if (!positional) "_nonpos" else ""
      filename  <- paste0(corpus, "_", ngram_name, "_",
                          method, pos_label, "_cache")
    }
    saveRDS(cache, file = file.path(path, paste0(filename, ".rds")))
    cat(sprintf("Cache saved to %s/%s.rds\n", path, filename))
    invisible(cache)
  } else {
    return(cache)
  }
}

#' Build and save phoneme frequency tables
#'
#' Computes phoneme n-gram frequency tables for a set of ARPAbet phoneme
#' sequences across all combinations of the supplied parameters, saves each
#' table as a CSV file, and writes a corresponding cache file for use by the
#' phoneme frequency scoring functions. This is typically a one-time setup
#' step run before using \code{\link{sbpf}}, \code{\link{mlbpf}},
#' \code{\link{supf}}, or \code{\link{mlupf}}.
#'
#' @param phoneme_sequences A character vector of space-separated ARPAbet
#'   phoneme strings (e.g. \code{"K AE1 T"}). Stress markers are stripped
#'   automatically.
#' @param n An integer vector of n-gram sizes to compute. \code{1} = phonemes,
#'   \code{2} = biphones, \code{3} = triphones. Defaults to \code{1:3}.
#' @param method A character vector of counting methods. \code{"type"} counts
#'   each sequence once regardless of corpus frequency; \code{"token"} weights
#'   by corpus frequency and requires \code{sequence_frequency} to be supplied.
#' @param corpus A string identifying the corpus (e.g. \code{"CMU"}).
#'   Used to name output files and subdirectories.
#' @param sequence_frequency A numeric vector of corpus frequencies
#'   corresponding to \code{phoneme_sequences}, required when \code{"token"}
#'   is included in \code{method}. Ignored for \code{"type"} counts.
#' @param sequence_length_range An integer vector of length 2 giving the
#'   minimum and maximum phoneme sequence lengths to include. Defaults to
#'   \code{c(2, 10)}.
#' @param positional Logical. If \code{TRUE}, compute position-specific
#'   frequencies. If \code{FALSE}, compute position-independent frequencies.
#'   Both can be requested by passing \code{c(TRUE, FALSE)}.
#' @param path Directory where CSV frequency tables will be saved. A
#'   subdirectory named after \code{corpus} will be created automatically.
#'   Defaults to \code{tables/} in the current working directory.
#' @param cache_path Directory where cache files will be saved. Defaults to
#'   \code{cache/} in the current working directory.
#'
#' @return Invisibly returns \code{NULL}. Called for its side effects of
#'   writing CSV and cache files to disk.
#'
#' @seealso \code{\link{build_nphone_cache}},
#'   \code{\link{tally_nphone_frequencies}}
#'
#' @examples
#' \dontrun{
#' build_nphone_tables(
#'   phoneme_sequences  = my_phoneme_strings,
#'   n                  = 1:3,
#'   method             = c("type", "token"),
#'   corpus             = "CMU",
#'   sequence_frequency = my_frequencies
#' )
#' }
#'
#' @export
build_nphone_tables <- function(phoneme_sequences,
                                n                      = 1:3,
                                method                 = c("type", "token"),
                                corpus,
                                sequence_frequency     = NULL,
                                sequence_length_range  = c(2, 10),
                                positional             = TRUE,
                                path                   = file.path(getwd(), "tables"),
                                cache_path             = file.path(getwd(), "cache"))
{

  if (is.null(sequence_frequency) && "token" %in% method) {
    warning('`sequence_frequency` not provided - removing "token" from method.')
    method <- setdiff(method, "token")
  }

  params <- expand.grid(n          = n,
                        method     = method,
                        positional = positional,
                        stringsAsFactors = FALSE)

  cat(sprintf("Building phonotactic tables for '%s': %d parameter combination(s).\n",
              corpus, nrow(params)))

  ngram_names <- c("1" = "phoneme", "2" = "biphone", "3" = "triphone")

  for (i in seq_len(nrow(params))) {

    ngram_name <- if (as.character(params$n[i]) %in% names(ngram_names)) {
      ngram_names[[as.character(params$n[i])]]
    } else {
      paste0(params$n[i], "phone")
    }
    pos_label <- if (params$positional[i]) "" else "_nonpos"

    cat(sprintf("\n[%d / %d]  n=%d  method=%s  positional=%s\n",
                i, nrow(params), params$n[i], params$method[i],
                params$positional[i]))

    freq_table <- tally_nphone_frequencies(
      phoneme_sequences     = phoneme_sequences,
      n                     = params$n[i],
      method                = params$method[i],
      sequence_frequency    = if (params$method[i] == "token") sequence_frequency else NULL,
      sequence_length_range = sequence_length_range,
      corpus                = corpus,
      positional            = params$positional[i]
    )

    # save CSV
    filename <- paste0(corpus, "_", ngram_name, "_", params$method[i], pos_label)
    if (!dir.exists(file.path(path, corpus))) {
      dir.create(file.path(path, corpus), recursive = TRUE)
    }
    write.csv(freq_table,
              file      = file.path(path, corpus, paste0(filename, ".csv")),
              row.names = FALSE)

    # save cache
    build_nphone_cache(
      freq_table = freq_table,
      n          = params$n[i],
      method     = params$method[i],
      positional = params$positional[i],
      save       = TRUE,
      path       = cache_path
    )
  }

  cat(sprintf("\nDone. Tables saved to: %s/%s\nCaches saved to: %s\n",
              path, corpus, cache_path))

  invisible(NULL)
}

#' Tally phoneme n-gram frequencies across a set of ARPAbet sequences
#'
#' Counts how often each phoneme n-gram occurs across a set of ARPAbet phoneme
#' sequences, either by type (each sequence counted once) or by token (weighted
#' by corpus frequency). Results are stratified by phoneme sequence length and
#' returned as a single data frame. This is the core computation underlying
#' \code{\link{build_nphone_tables}}.
#'
#' Note that sequence length refers to the number of phonemes, not the number
#' of orthographic characters. A corpus entry filtered by
#' \code{sequence_length_range} is filtered on phoneme count.
#'
#' @param phoneme_sequences A character vector of space-separated ARPAbet
#'   phoneme strings. Stress markers are stripped automatically.
#' @param n Integer. N-gram size: \code{1} = phonemes, \code{2} = biphones,
#'   \code{3} = triphones. Defaults to \code{1L}.
#' @param method One of \code{"type"} or \code{"token"}. \code{"type"} counts
#'   each sequence once; \code{"token"} weights by corpus frequency, requiring
#'   \code{sequence_frequency} to be supplied.
#' @param corpus An optional string identifying the corpus (e.g. \code{"CMU"}).
#'   Stored as a metadata column in the output. If \code{NULL}, the corpus
#'   column will be \code{NA}.
#' @param sequence_frequency A numeric vector of corpus frequencies
#'   corresponding to \code{phoneme_sequences}, required when
#'   \code{method = "token"}. Must be the same length as
#'   \code{phoneme_sequences}.
#' @param sequence_length_range An optional integer vector of length 2 giving
#'   the minimum and maximum phoneme sequence lengths to include. Sequences
#'   outside this range are silently dropped before tallying.
#' @param positional Logical. If \code{TRUE}, frequencies are tallied
#'   separately for each position in the sequence, producing one column per
#'   position. If \code{FALSE}, frequencies are summed across all positions.
#'   Defaults to \code{TRUE}.
#'
#' @return A data frame with one row per n-gram per sequence length, containing
#'   metadata columns (\code{corpus}, \code{sequence_length},
#'   \code{n_sequences}, \code{ngram}, \code{method}) followed by either
#'   positional frequency columns (\code{pos1}, \code{pos2}, \dots) when
#'   \code{positional = TRUE}, or a single \code{freq} column when
#'   \code{positional = FALSE}.
#'
#' @seealso \code{\link{build_nphone_tables}}, \code{\link{build_nphone_cache}}
#'
#' @examples
#' \dontrun{
#' tally_nphone_frequencies(
#'   phoneme_sequences = my_phoneme_strings,
#'   n                 = 2L,
#'   method            = "type",
#'   corpus            = "CMU"
#' )
#' }
#'
#' @export
tally_nphone_frequencies <- function(phoneme_sequences,
                                     n                     = 1L,
                                     method                = c("type", "token"),
                                     corpus                = NULL,
                                     sequence_frequency    = NULL,
                                     sequence_length_range = NULL,
                                     positional            = TRUE)
{

  method <- match.arg(method)

  if (!is.null(sequence_frequency) && method == "type") {
    warning('`sequence_frequency` provided but method = "type" - frequencies will not be used.')
  }

  if (n > 3L) {
    warning("n = ", n, " will produce a large number of n-gram combinations. ",
            "This may require substantial time and memory.")
  }

  # tokenise all sequences and strip stress
  token_list <- lapply(phoneme_sequences, tokenise_arpabet)
  seq_lengths <- vapply(token_list, length, integer(1))

  # filter by sequence length
  if (!is.null(sequence_length_range)) {
    if (length(sequence_length_range) != 2L || !is.numeric(sequence_length_range)) {
      stop("`sequence_length_range` must be a numeric vector of length 2.")
    }
    keep <- seq_lengths >= sequence_length_range[1] &
      seq_lengths <= sequence_length_range[2]
    if (!is.null(sequence_frequency)) sequence_frequency <- sequence_frequency[keep]
    token_list  <- token_list[keep]
    seq_lengths <- seq_lengths[keep]
    if (length(token_list) == 0L) {
      stop("No sequences remain after applying `sequence_length_range`.")
    }
  }

  # frequency weights
  if (method == "type") {
    weights <- rep(1, length(token_list))
  } else {
    if (is.null(sequence_frequency) ||
        length(sequence_frequency) != length(token_list)) {
      stop("`sequence_frequency` must be a numeric vector the same length as `phoneme_sequences`.")
    }
    weights <- sequence_frequency
  }

  # n-gram column name
  ngram_name <- switch(as.character(n),
                       "1" = "phoneme",
                       "2" = "biphone",
                       "3" = "triphone",
                       paste0(n, "phone"))

  # all possible n-grams from ARPAbet inventory
  ngram_grid <- do.call(expand.grid,
                        rep(list(ARPABET), n),
                        quote = FALSE)
  all_ngrams <- apply(ngram_grid, 1, paste, collapse = "-")

  # position columns
  max_seq_length  <- max(seq_lengths)
  max_n_positions <- max_seq_length - (n - 1L)
  if (max_n_positions < 1L) stop("Longest sequence is too short for n = ", n, ".")
  pos_cols <- paste0("pos", seq_len(max_n_positions))

  unique_lengths <- sort(unique(seq_lengths))
  results        <- vector("list", length(unique_lengths))

  cat(sprintf("Processing %d sequences across %d sequence length(s)...\n",
              length(token_list), length(unique_lengths)))

  for (seq_length in unique_lengths) {

    seq_indices    <- which(seq_lengths == seq_length)
    tokens_subset  <- token_list[seq_indices]
    weights_subset <- weights[seq_indices]
    n_sequences    <- length(tokens_subset)
    n_positions    <- seq_length - (n - 1L)

    cat(sprintf("  [length %d]  %d sequences, %d position(s)\n",
                seq_length, n_sequences, n_positions))

    if (n_positions < 1L) {
      warning("Skipping sequences of length ", seq_length,
              ": too short for n = ", n, ".")
      next
    }

    freq_mat <- matrix(0L,
                       nrow     = length(all_ngrams),
                       ncol     = n_positions,
                       dimnames = list(all_ngrams,
                                       paste0("pos", seq_len(n_positions))))

    milestones <- unique(floor(seq(1, n_sequences, length.out = 11)))

    for (i in seq_along(tokens_subset)) {
      if (i %in% milestones) {
        cat(sprintf("    %3.0f%% complete (%d / %d)\n",
                    i / n_sequences * 100, i, n_sequences))
      }
      tokens      <- tokens_subset[[i]]
      word_weight <- weights_subset[i]
      for (pos in seq_len(n_positions)) {
        ngram_tokens <- tokens[pos:(pos + n - 1L)]
        if (!all(ngram_tokens %in% ARPABET)) next
        ngram_key <- paste(ngram_tokens, collapse = "-")
        freq_mat[ngram_key, pos] <- freq_mat[ngram_key, pos] + word_weight
      }
    }

    # build result table
    if (positional) {
      result_table <- as.data.frame(freq_mat)
      missing_cols <- setdiff(pos_cols, names(result_table))
      result_table[missing_cols] <- NA
      result_table[[ngram_name]] <- rownames(result_table)
      rownames(result_table)     <- NULL
      result_table               <- result_table[, c(ngram_name, pos_cols)]
    } else {
      result_table <- data.frame(
        ngram = all_ngrams,
        freq  = rowSums(freq_mat),
        row.names = NULL
      )
      names(result_table)[1] <- ngram_name
    }

    if (!is.null(corpus)) result_table$corpus <-
      corpus else result_table$corpus <- NA
    result_table$sequence_length <- seq_length
    result_table$n_sequences     <- n_sequences
    result_table$ngram           <- ngram_name
    result_table$method          <- method

    meta_cols  <- intersect(c("corpus", "sequence_length", "n_sequences",
                              "ngram", ngram_name, "method"),
                            names(result_table))
    other_cols <- setdiff(names(result_table), meta_cols)
    results[[as.character(seq_length)]] <-
      result_table[, c(meta_cols, other_cols)]
  }

  do.call(rbind, Filter(Negate(is.null), results))
}

#' Build a phoneme n-gram frequency cache
#'
#' Converts a frequency table produced by \code{\link{tally_nphone_frequencies}}
#' into a structured cache used by the phoneme frequency scoring functions.
#' Positional caches store a matrix per sequence length; non-positional caches
#' store length-specific and corpus-wide frequency vectors. Optionally saves
#' the cache to disk as an \code{.rds} file.
#'
#' @param freq_table A data frame as returned by
#'   \code{\link{tally_nphone_frequencies}}.
#' @param n Integer. N-gram size: \code{1} = phonemes, \code{2} = biphones,
#'   \code{3} = triphones. Must match the n-gram size used to produce
#'   \code{freq_table}. Defaults to \code{2L}.
#' @param method One of \code{"type"} or \code{"token"}. Must match the method
#'   used to produce \code{freq_table}.
#' @param positional Logical. If \code{TRUE}, builds a positional cache (a
#'   named list of matrices, one per sequence length). If \code{FALSE}, builds
#'   a non-positional cache (named frequency vectors by length and overall).
#'   Defaults to \code{TRUE}.
#' @param save Logical. If \code{TRUE}, saves the cache to disk as an
#'   \code{.rds} file at \code{path}. Defaults to \code{FALSE}.
#' @param path Directory where the cache file will be saved when
#'   \code{save = TRUE}. Created automatically if it does not exist. Defaults
#'   to \code{cache/} in the current working directory.
#' @param filename Optional string giving the base filename (without
#'   extension). If \code{NULL}, a name is constructed from the corpus,
#'   n-gram type, method, and positional flag.
#'
#' @return When \code{save = FALSE}, returns the cache list. When
#'   \code{save = TRUE}, saves the cache to disk and invisibly returns the
#'   cache list. Positional caches contain \code{matrices} (a named list of
#'   matrices) and \code{length_counts} (a named numeric vector).
#'   Non-positional caches contain \code{by_length} (a named list of frequency
#'   vectors), \code{overall} (a corpus-wide frequency vector), and
#'   \code{length_counts}.
#'
#' @seealso \code{\link{tally_nphone_frequencies}},
#'   \code{\link{build_nphone_tables}}
#'
#' @examples
#' \dontrun{
#' freq_table <- tally_nphone_frequencies(
#'   phoneme_sequences = my_phoneme_strings,
#'   n                 = 2L,
#'   method            = "type",
#'   corpus            = "CMU"
#' )
#' cache <- build_nphone_cache(freq_table, n = 2L, method = "type")
#' }
#'
#' @export
build_nphone_cache <- function(freq_table,
                               n          = 2L,
                               method     = c("type", "token"),
                               positional = TRUE,
                               save       = FALSE,
                               path       = file.path(getwd(), "cache"),
                               filename   = NULL)
{

  method     <- match.arg(method)
  ngram_name <- switch(as.character(n),
                       "1" = "phoneme",
                       "2" = "biphone",
                       "3" = "triphone",
                       paste0(n, "phone"))

  ft <- freq_table[freq_table$method == method, ]
  if (nrow(ft) == 0L) stop("No rows found for method = '", method, "'.")

  length_counts <- setNames(
    vapply(split(ft, ft$sequence_length),
           function(x) x$n_sequences[1], numeric(1)),
    as.character(sort(unique(ft$sequence_length)))
  )

  if (positional) {
    pos_cols      <- grep("^pos", names(ft), value = TRUE)
    mat           <- as.matrix(ft[, pos_cols])
    rownames(mat) <- ft[[ngram_name]]

    matrices <- vector("list", length(unique(ft$sequence_length)))
    names(matrices) <- as.character(sort(unique(ft$sequence_length)))

    for (sl in unique(ft$sequence_length)) {
      n_valid     <- sl - (n - 1L)
      rows        <- ft$sequence_length == sl
      m           <- mat[rows, seq_len(n_valid), drop = FALSE]
      rownames(m) <- ft[[ngram_name]][rows]
      matrices[[as.character(sl)]] <- m
    }

    cache <- list(
      matrices      = matrices,
      length_counts = length_counts
    )

  } else {

    by_length <- lapply(split(ft, ft$sequence_length), function(sl_ft) {
      setNames(sl_ft$freq, sl_ft[[ngram_name]])
    })

    overall <- tapply(ft$freq, ft[[ngram_name]], sum)

    cache <- list(
      by_length     = by_length,
      overall       = overall,
      length_counts = length_counts
    )
  }

  if (save) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    if (is.null(filename)) {
      corpus    <- if ("corpus" %in% names(ft)) ft$corpus[1] else "corpus"
      pos_label <- if (!positional) "_nonpos" else ""
      filename  <- paste0(corpus, "_", ngram_name, "_",
                          method, pos_label, "_cache")
    }
    saveRDS(cache, file = file.path(path, paste0(filename, ".rds")))
    cat(sprintf("Cache saved to %s/%s.rds\n", path, filename))
    invisible(cache)
  } else {
    return(cache)
  }
}
