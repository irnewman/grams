#' Solve nested anagrams from a set of letters
#'
#' Given a string of letters (typically a word), finds all valid dictionary
#' words that can be formed from any subset of those letters. Searches across
#' all subset lengths from \code{min_length} up to the full length of the input
#' (or a restricted set of lengths if \code{lengths} is specified).
#'
#' @param word A character string. The letters to search within.
#' @param sig_index A named list mapping letter signatures to character vectors
#'   of matching words, as produced by \code{build_sig_index()}.
#' @param min_length Integer. Minimum subset length to consider. Default is 3.
#' @param lengths Integer vector. If \code{NULL} (default), all lengths from
#'   \code{min_length} up to \code{nchar(word)} are searched. Otherwise, only
#'   the specified lengths are searched (values below \code{min_length} are
#'   silently ignored).
#' @param include_self Logical. If \code{TRUE}, the input word itself is
#'   included in results if it is a valid dictionary word. Default is
#'   \code{FALSE}.
#'
#' @return A character vector of unique words that can be formed from subsets
#'   of the letters in \code{word}, sorted alphabetically. Returns
#'   \code{character(0)} if no words are found.
#'
#' @details
#' The function enumerates all unique letter-combination signatures of each
#' target length, then looks each up in \code{sig_index}. This avoids
#' redundant lookups when repeated letters produce the same signature from
#' different positional subsets.
#'
#' @examples
#' \dontrun{
#' solve_nested_anagrams("triangle", sig_index)
#' solve_nested_anagrams("triangle", sig_index, min_length = 4)
#' solve_nested_anagrams("triangle", sig_index, lengths = c(4, 5))
#' solve_nested_anagrams("triangle", sig_index, include_self = TRUE)
#' }
#'
#' @importFrom utils combn
#' @export
solve_nested_anagrams <- function(word,
                                  sig_index = NULL,
                                  min_length  = 3L,
                                  lengths     = NULL,
                                  include_self = FALSE) {

  if (is.null(sig_index)) sig_index <- grams::sig_index

  word  <- tolower(trimws(word))
  chars <- strsplit(word, "")[[1]]
  n     <- length(chars)

  # Determine which lengths to search
  if (is.null(lengths)) {
    target_lengths <- seq(min_length, n)
  } else {
    target_lengths <- sort(unique(as.integer(lengths)))
    target_lengths <- target_lengths[target_lengths >= min_length &
                                       target_lengths <= n]
  }

  if (length(target_lengths) == 0L) {
    return(character(0))
  }

  results <- character(0)

  for (len in target_lengths) {
    # Get all unique combinations of positions of this length
    combos <- combn(n, len, simplify = FALSE)

    # Build unique signatures for this length (dedup before lookup)
    sigs <- unique(vapply(combos, function(idx) {
      paste(sort(chars[idx]), collapse = "")
    }, character(1)))

    # Look up each unique signature
    for (sig in sigs) {
      matches <- sig_index[[sig]]
      if (!is.null(matches)) {
        results <- c(results, matches)
      }
    }
  }

  results <- unique(results)

  if (!include_self) {
    results <- results[results != word]
  }

  sort(results)
}



#' Build a nested anagram solutions table
#'
#' Finds all nested anagram solutions for a set of words or letter strings and
#' returns a data frame with paired word-list and count columns for each subset
#' length. Input can be a user-supplied character vector or, if none is
#' provided, a set of candidate letter strings generated automatically via
#' \code{\link{generate_letter_sets}}.
#'
#' @param words A character vector of words or letter strings to process. If
#'   \code{NULL} (default), letter sets are generated automatically using
#'   \code{\link{generate_letter_sets}} with the arguments below.
#' @param sig_index A named list mapping letter signatures to character vectors
#'   of matching words. Defaults to \code{grams::sig_index} if \code{NULL}.
#' @param min_length Integer. Minimum nested anagram length to search. Default
#'   is \code{3}.
#' @param lengths Integer vector of specific nested anagram lengths to search,
#'   or \code{NULL} for all lengths from \code{min_length} up to the length of
#'   each input string. Default is \code{NULL}.
#' @param include_self Logical. Whether to include the input word itself if it
#'   is a valid dictionary word. Default is \code{FALSE}.
#' @param set_lengths Integer vector. Lengths of letter sets to generate when
#'   \code{words = NULL}. Default is \code{6:9}.
#' @param max_duplicates Integer. Maximum number of times any single letter may
#'   appear in a generated set. Ignored if \code{words} is supplied. Default
#'   is \code{2}.
#' @param vowel_ratio Numeric vector of length 2, \code{c(min, max)}, giving
#'   the allowable proportion of vowels in a generated set. Ignored if
#'   \code{words} is supplied. Default is \code{c(0.2, 0.6)}.
#' @param method Character. Either \code{"exhaustive"} or \code{"sample"}.
#'   Ignored if \code{words} is supplied. Default is \code{"sample"}.
#' @param n_sample Integer. Number of letter sets to sample when
#'   \code{method = "sample"}. Ignored if \code{words} is supplied. Default
#'   is \code{1000}.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility when
#'   generating letter sets. Ignored if \code{words} is supplied. Default is
#'   \code{NULL}.
#'
#' @return A data frame with one row per input word or letter string. The first
#'   column is \code{word}. Remaining columns are paired \code{words_N}
#'   (comma-separated matches) and \code{n_N} (integer count) for each subset
#'   length from \code{min_length} up to the maximum input string length.
#'   Lengths with no matches have \code{NA} in the words column and \code{0}
#'   in the count column.
#'
#' @examples
#' \dontrun{
#' # From a user-supplied word list
#' nested_anagram_table(c("triangle", "parking", "test"))
#'
#' # From a generated letter set (sampled)
#' nested_anagram_table(set_lengths = 7:8, n_sample = 500, seed = 42)
#'
#' # From a generated letter set (exhaustive, short lengths only)
#' nested_anagram_table(set_lengths = 5:6, method = "exhaustive")
#' }
#'
#' @export
nested_anagram_table <- function(words          = NULL,
                                 sig_index      = NULL,
                                 min_length     = 3L,
                                 lengths        = NULL,
                                 include_self   = FALSE,
                                 set_lengths    = 6:9,
                                 max_duplicates = 2L,
                                 vowel_ratio    = c(0.2, 0.6),
                                 method         = c("exhaustive", "sample"),
                                 n_sample       = 1000L,
                                 seed           = NULL) {

  method <- match.arg(method)

  if (is.null(sig_index)) sig_index <- grams::sig_index

  if (is.null(words)) {
    message("No word list supplied -- generating letter sets via generate_letter_sets().")
    words <- generate_letter_sets(
      lengths        = set_lengths,
      max_duplicates = max_duplicates,
      vowel_ratio    = vowel_ratio,
      method         = method,
      n_sample       = n_sample,
      seed           = seed
    )
  }

  # Determine the full set of length columns to produce
  max_len <- max(nchar(words))
  if (is.null(lengths)) {
    all_lengths <- seq(min_length, max_len)
  } else {
    all_lengths <- sort(unique(as.integer(lengths)))
    all_lengths <- all_lengths[all_lengths >= min_length & all_lengths <= max_len]
  }

  rows <- lapply(words, function(w) {
    found <- solve_nested_anagrams(
      w,
      sig_index    = sig_index,
      min_length   = min_length,
      lengths      = lengths,
      include_self = include_self
    )

    by_len <- split(found, nchar(found))

    row <- list(word = w)
    for (len in all_lengths) {
      matches <- by_len[[as.character(len)]]
      row[[paste0("words_", len)]] <- if (length(matches)) paste(matches, collapse = ",") else NA_character_
      row[[paste0("n_", len)]]     <- if (length(matches)) length(matches) else 0L
    }
    row
  })

  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}


#' Generate candidate letter sets for nested anagram exploration
#'
#' Produces letter strings of specified lengths from the 26-letter alphabet,
#' subject to constraints on duplicate letters and vowel ratio. Can enumerate
#' exhaustively (within constraints) or draw a random sample.
#'
#' @param lengths Integer vector. The length(s) of letter sets to generate.
#'   Default is \code{6:9}.
#' @param max_duplicates Integer. Maximum number of times any single letter may
#'   appear in a set. Default is \code{2}.
#' @param vowel_ratio Numeric vector of length 2 giving the \code{c(min, max)}
#'   proportion of vowels allowed. Default is \code{c(0.2, 0.6)}.
#' @param method Character. Either \code{"exhaustive"} to enumerate all valid
#'   combinations or \code{"sample"} to draw randomly. Default is
#'   \code{"exhaustive"}.
#' @param n_sample Integer. Number of letter sets to sample when
#'   \code{method = "sample"}. Default is \code{1000}.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility when
#'   \code{method = "sample"}. Default is \code{NULL}.
#'
#' @return A character vector of letter strings, each sorted alphabetically
#'   (i.e. in signature form), with duplicates removed across lengths.
#'
#' @details
#' Exhaustive enumeration uses \code{combn()} over the multiset of available
#' letters. For lengths above 8 or 9 this can be slow; \code{"sample"} is
#' recommended for exploratory use at longer lengths.
#'
#' Vowels are defined as \code{a, e, i, o, u}.
#'
#' @examples
#' \dontrun{
#' # All 6-letter sets with at most 2 of any letter
#' sets <- generate_letter_sets(lengths = 6)
#'
#' # Sample 500 sets of length 7-8
#' sets <- generate_letter_sets(lengths = 7:8, method = "sample", n_sample = 500)
#'
#' # Feed directly into nested_anagram_table
#' nested_anagram_table(sets, min_length = 4)
#' }
#'
#' @export
generate_letter_sets <- function(lengths       = 6:9,
                                 max_duplicates = 2L,
                                 vowel_ratio    = c(0.2, 0.6),
                                 method         = c("exhaustive", "sample"),
                                 n_sample       = 1000L,
                                 seed           = NULL) {

  method  <- match.arg(method)
  vowels  <- c("a", "e", "i", "o", "u")
  letters <- letters  # a-z

  # Build the expanded alphabet respecting max_duplicates
  expanded <- rep(letters, each = max_duplicates)

  is_valid <- function(combo) {
    # Check duplicate constraint
    tbl <- table(combo)
    if (any(tbl > max_duplicates)) return(FALSE)
    # Check vowel ratio
    vr <- sum(combo %in% vowels) / length(combo)
    vr >= vowel_ratio[1] && vr <= vowel_ratio[2]
  }

  if (!is.null(seed)) set.seed(seed)

  results <- character(0)

  for (len in lengths) {
    if (method == "exhaustive") {
      # Enumerate unique combinations from the expanded alphabet
      combos <- combn(expanded, len, simplify = FALSE)
      sigs   <- unique(vapply(combos, function(x) paste(sort(x), collapse = ""), character(1)))
      valid  <- Filter(function(s) {
        combo <- strsplit(s, "")[[1]]
        is_valid(combo)
      }, sigs)
      results <- c(results, valid)

    } else {
      # Sample randomly then filter
      sampled <- replicate(n_sample * 10L, {
        paste(sort(sample(letters, len, replace = TRUE)), collapse = "")
      })
      sampled <- unique(sampled)
      valid   <- Filter(function(s) {
        combo <- strsplit(s, "")[[1]]
        is_valid(combo)
      }, sampled)
      # Trim to n_sample
      if (length(valid) > n_sample) valid <- valid[seq_len(n_sample)]
      results <- c(results, valid)
    }
  }

  sort(unique(results))
}
