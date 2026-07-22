#' Extract the best-estimate phoneme sequence for a word
#'
#' Returns the package's best estimate of a word's pronunciation as a
#' space-separated ARPAbet phoneme string, sourced from CMU when available
#' (with multi-pronunciation handling controlled by \code{pronunciation}),
#' or from \code{\link{syllables}}'s gruut/g2p fallback otherwise.
#'
#' @param word A word or string of letters.
#' @param pronunciation One of \code{"first"} (default) or \code{"all"}.
#'   For CMU words with multiple listed pronunciations, \code{"first"} returns
#'   only the primary pronunciation as a single string. \code{"all"} returns
#'   every pronunciation as a character vector, leaving averaging or other
#'   handling to the caller (see \code{\link{get_phoneme_score}}).
#'
#' @return If \code{pronunciation = "first"}, a single character string of
#'   space-separated ARPAbet phonemes (or \code{NA_character_} if none could
#'   be estimated). If \code{pronunciation = "all"}, a character vector with
#'   one phoneme string per pronunciation.
#'
#' @seealso \code{\link{get_phoneme_score}}, \code{\link{syllables}}
#'
#' @export
get_phonemes <- function(word, pronunciation = c("first", "all")) {
  pronunciation <- match.arg(pronunciation)
  word <- tolower(word)

  if (word %in% grams::cmu$word) {
    phoneme_list <- grams::cmu$phonemes[grams::cmu$word == word]
    phoneme_strings <- vapply(phoneme_list, paste, character(1), collapse = " ")
    if (pronunciation == "first") return(phoneme_strings[1])
    return(phoneme_strings)
  }

  # non-CMU fallback
  syll_table <- syllables(word, output = "full")
  phones <- syll_table$phone
  if (length(phones) == 0L || all(is.na(phones))) return(NA_character_)
  paste(phones, collapse = " ")
}

#' Compute a phonotactic score for a word, averaging across pronunciations
#'
#' @inheritParams get_phonemes
#' @param score_fn A phonotactic scoring function, e.g. \code{\link{sbpf}},
#'   \code{\link{mlbpf}}, \code{\link{supf}}, \code{\link{mlupf}},
#'   \code{\link{stpf}}, \code{\link{mltpf}}. Defaults to \code{\link{mlbpf}}.
#' @param ... Additional arguments passed to \code{score_fn}.
#'
#' @return A single numeric score.
#'
#' @export
get_phoneme_score <- function(word,
                              score_fn = mlbpf,
                              pronunciation = "first",
                              ...)
{
  phonemes <- get_phonemes(word, pronunciation = pronunciation)
  if (length(phonemes) == 0L || all(is.na(phonemes))) return(NA_real_)
  scores <- sapply(phonemes, function(p) score_fn(p, ...))
  mean(scores)
}
