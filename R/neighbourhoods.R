#' Compute orthographic neighbourhood measures.
#'
#' Computes Orthographic Levenshtein Distance (OLD-N) and/or Edit Distance
#' neighbours (ED-N) for a given word against a dictionary.
#'
#' @param word A word or anagram in string format.
#' @param dictionary Character vector of dictionary words to compare against.
#' @param n Number of nearest neighbours for OLD-N. Default is 20.
#' @param method Distance method passed to stringdist. Default is "lv".
#' @param output One of "old", "ed", "both", or "neighbours".
#'
#' @return Numeric value, a data.frame if output = "both",
#'   or a character vector if output = "neighbours".
#' @export
orthographic_neighbours <- function(word,
                                    dictionary = NULL,
                                    n          = 20,
                                    method     = "lv",
                                    output     = "both")
{
  if (is.null(dictionary)) dictionary <- grams::subtlex_uk$word

  word       <- tolower(word)
  word_len   <- nchar(word)
  candidates <- dictionary[nchar(dictionary) %in%
                             (word_len - 1L):(word_len + 1L)]
  distances  <- stringdist::stringdist(word, candidates, method = method)

  if (output == "neighbours") {
    return(candidates[distances == 1L])
  }

  ED  <- sum(distances == 1L)
  d   <- sort(distances[distances > 0L])
  m   <- min(n, length(d))
  OLD <- mean(d[seq_len(m)])

  if (output == "old")  return(OLD)
  if (output == "ed")   return(ED)
  if (output == "both") return(data.frame(OLD = OLD, ED = ED))
}

#' Compute Orthographic Levenshtein Distance for N nearest neighbours.
#'
#' @param word A word or anagram in string format.
#' @param n Number of nearest neighbours. Default is 20.
#' @param dictionary Character vector of dictionary words to compare against.
#' @param method Distance method passed to stringdist. Default is "lv".
#'
#' @return Numeric OLD-N value.
#' @export
old_n <- function(word,
                  n          = 20,
                  dictionary = NULL,
                  method     = "lv")
{
  if (is.null(dictionary)) dictionary <- grams::subtlex_uk$word
  orthographic_neighbours(word, dictionary, n = n, method = method,
                          output = "old")
}

#' Compute Orthographic Levenshtein Distance for 20 nearest neighbours.
#'
#' Convenience wrapper around old_n() with n = 20.
#'
#' @param word A word or anagram in string format.
#' @param dictionary Character vector of dictionary words to compare against.
#' @param method Distance method passed to stringdist. Default is "lv".
#'
#' @return Numeric OLD20 value.
#' @export
old20 <- function(word,
                  dictionary = NULL,
                  method     = "lv")
{
  if (is.null(dictionary)) dictionary <- grams::subtlex_uk$word
  old_n(word, n = 20, dictionary = dictionary, method = method)
}

#' Compute the number of orthographic neighbours at edit distance N.
#'
#' @param word A word or anagram in string format.
#' @param n Edit distance threshold. Default is 1.
#' @param dictionary Character vector of dictionary words to compare against.
#' @param method Distance method passed to stringdist. Default is "lv".
#'
#' @return Numeric count of neighbours at distance N.
#' @export
ed_n <- function(word,
                 n          = 1,
                 dictionary = NULL,
                 method     = "lv")
{
  if (is.null(dictionary)) dictionary <- grams::subtlex_uk$word
  orthographic_neighbours(word, dictionary, n = n, method = method,
                          output = "ed")
}

#' Compute the number of orthographic neighbours at edit distance 1.
#'
#' Convenience wrapper around ed_n() with n = 1.
#'
#' @param word A word or anagram in string format.
#' @param dictionary Character vector of dictionary words to compare against.
#' @param method Distance method passed to stringdist. Default is "lv".
#'
#' @return Numeric count of neighbours at edit distance 1.
#' @export
ed1 <- function(word,
                dictionary = NULL,
                method     = "lv")
{
  if (is.null(dictionary)) dictionary <- grams::subtlex_uk$word
  ed_n(word, n = 1, dictionary = dictionary, method = method)
}

#' Return orthographic neighbours of a word at edit distance 1.
#'
#' @param word A word or anagram in string format.
#' @param dictionary Character vector of dictionary words to compare against.
#' @param method Distance method passed to stringdist. Default is "lv".
#'
#' @return Character vector of neighbouring words.
#' @export
orthographic_neighbours_list <- function(word,
                                         dictionary = NULL,
                                         method     = "lv")
{
  if (is.null(dictionary)) dictionary <- grams::subtlex_uk$word
  orthographic_neighbours(word, dictionary, n = 1, method = method,
                          output = "neighbours")
}

#' Encode an ARPAbet phoneme sequence as a single-character string
#'
#' Converts a vector of ARPAbet tokens (or a space-separated string) to a
#' single string where each phoneme is represented by one ASCII character,
#' using the official ARPAbet one-character encoding with custom extensions
#' for \code{UX} (\code{&}) and \code{AXR} (\code{\%}). This encoding is
#' used internally by phonological neighbourhood functions to enable
#' character-level Levenshtein distance computation via \code{stringdist},
#' where each character represents exactly one phoneme edit unit.
#'
#' @param phonemes Either a character vector of ARPAbet tokens (e.g.
#'   \code{c("K", "AE", "T")}) or a single space-separated ARPAbet string
#'   (e.g. \code{"K AE T"}).
#'
#' @return A single character string of encoded phonemes (e.g. \code{"k@t"}).
#'
#' @keywords internal
encode_arpabet <- function(phonemes) {
  tokens  <- tokenise_arpabet(phonemes)
  encoded <- ARPABET_TO_1CHAR[tokens]
  if (anyNA(encoded)) {
    unknown <- tokens[is.na(encoded)]
    warning("Unknown ARPAbet tokens (will be dropped): ",
            paste(unique(unknown), collapse = ", "))
    encoded <- encoded[!is.na(encoded)]
  }
  paste(encoded, collapse = "")
}

#' Convert ARPAbet phonemes to IPA
#'
#' Converts a vector of ARPAbet tokens (or a space-separated ARPAbet string)
#' to an IPA string. Stress markers are stripped automatically.
#'
#' @param phonemes Either a character vector of ARPAbet tokens (e.g.
#'   \code{c("K", "AE1", "T")}) or a single space-separated ARPAbet string
#'   (e.g. \code{"K AE1 T"}).
#'
#' @return A single IPA string (e.g. \code{"kæt"}).
#'
#' @export
arpabet_to_ipa <- function(phonemes) {
  tokens <- tokenise_arpabet(phonemes)
  ipa    <- ARPABET_TO_IPA[tokens]
  if (anyNA(ipa)) {
    unknown <- tokens[is.na(ipa)]
    warning("Unknown ARPAbet tokens (will be dropped): ",
            paste(unique(unknown), collapse = ", "))
    ipa <- ipa[!is.na(ipa)]
  }
  paste(ipa, collapse = "")
}

#' Compute phonological neighbourhood measures
#'
#' Computes Phonological Levenshtein Distance (PLD-N) and/or phonological
#' edit distance neighbours (PED-N) for a given word or phoneme sequence
#' against a reference lexicon. Phoneme sequences are encoded using the
#' ARPAbet one-character encoding before distance computation, so that each
#' character-level edit corresponds to exactly one phoneme substitution,
#' insertion, or deletion.
#'
#' @param phonemes Either a character vector of ARPAbet tokens, a
#'   space-separated ARPAbet string, or a pre-encoded one-character string
#'   (if \code{encoded = TRUE}).
#' @param lexicon A character vector of pre-encoded one-character phoneme
#'   strings to compare against. Defaults to \code{grams::cmu$phonemes_1char}.
#' @param n Number of nearest neighbours for PLD-N. Default is \code{20}.
#' @param method Distance method passed to \code{stringdist}. Default is
#'   \code{"lv"}.
#' @param output One of \code{"pld"}, \code{"ped"}, \code{"both"}, or
#'   \code{"neighbours"}.
#'
#' @return A numeric value, a \code{data.frame} if \code{output = "both"},
#'   or a character vector of encoded neighbour strings if
#'   \code{output = "neighbours"}.
#'
#' @seealso \code{\link{pld20}}, \code{\link{ped1}},
#'   \code{\link{orthographic_neighbours}}
#'
#' @export
phonological_neighbours <- function(phonemes,
                                    lexicon  = NULL,
                                    n        = 20,
                                    method   = "lv",
                                    output   = "both")
{
  if (is.null(lexicon)) lexicon <- grams::cmu$phonemes_1char

  target     <- encode_arpabet(phonemes)
  target_len <- nchar(target)
  candidates <- lexicon[nchar(lexicon) %in% (target_len - 1L):(target_len + 1L)]
  distances  <- stringdist::stringdist(target, candidates, method = method)

  if (output == "neighbours") {
    return(candidates[distances == 1L])
  }

  PED <- sum(distances == 1L)
  d   <- sort(distances[distances > 0L])
  m   <- min(n, length(d))
  PLD <- mean(d[seq_len(m)])

  if (output == "pld")  return(PLD)
  if (output == "ped")  return(PED)
  if (output == "both") return(data.frame(PLD = PLD, PED = PED))
}

#' Compute Phonological Levenshtein Distance for N nearest neighbours
#'
#' @param phonemes Either a character vector of ARPAbet tokens or a
#'   space-separated ARPAbet string.
#' @param n Number of nearest neighbours. Default is \code{20}.
#' @param lexicon Character vector of pre-encoded phoneme strings to compare
#'   against. Defaults to \code{grams::cmu$phonemes_1char}.
#' @param method Distance method passed to \code{stringdist}. Default is
#'   \code{"lv"}.
#'
#' @return Numeric PLD-N value.
#' @export
pld_n <- function(phonemes,
                  n       = 20,
                  lexicon = NULL,
                  method  = "lv")
{
  if (is.null(lexicon)) lexicon <- grams::cmu$phonemes_1char
  phonological_neighbours(phonemes, lexicon, n = n, method = method,
                          output = "pld")
}

#' Compute Phonological Levenshtein Distance for 20 nearest neighbours
#'
#' Convenience wrapper around \code{\link{pld_n}} with \code{n = 20}.
#' The phonological analogue of \code{\link{old20}}.
#'
#' @param phonemes Either a character vector of ARPAbet tokens or a
#'   space-separated ARPAbet string.
#' @param lexicon Character vector of pre-encoded phoneme strings to compare
#'   against. Defaults to \code{grams::cmu$phonemes_1char}.
#' @param method Distance method passed to \code{stringdist}. Default is
#'   \code{"lv"}.
#'
#' @return Numeric PLD20 value.
#' @export
pld20 <- function(phonemes,
                  lexicon = NULL,
                  method  = "lv")
{
  if (is.null(lexicon)) lexicon <- grams::cmu$phonemes_1char
  pld_n(phonemes, n = 20, lexicon = lexicon, method = method)
}

#' Compute the number of phonological neighbours at edit distance N
#'
#' @param phonemes Either a character vector of ARPAbet tokens or a
#'   space-separated ARPAbet string.
#' @param n Edit distance threshold. Default is \code{1}.
#' @param lexicon Character vector of pre-encoded phoneme strings to compare
#'   against. Defaults to \code{grams::cmu$phonemes_1char}.
#' @param method Distance method passed to \code{stringdist}. Default is
#'   \code{"lv"}.
#'
#' @return Numeric count of neighbours at distance N.
#' @export
ped_n <- function(phonemes,
                  n       = 1,
                  lexicon = NULL,
                  method  = "lv")
{
  if (is.null(lexicon)) lexicon <- grams::cmu$phonemes_1char
  phonological_neighbours(phonemes, lexicon, n = n, method = method,
                          output = "ped")
}

#' Compute the number of phonological neighbours at edit distance 1
#'
#' Convenience wrapper around \code{\link{ped_n}} with \code{n = 1}.
#' The phonological analogue of \code{\link{ed1}}.
#'
#' @param phonemes Either a character vector of ARPAbet tokens or a
#'   space-separated ARPAbet string.
#' @param lexicon Character vector of pre-encoded phoneme strings to compare
#'   against. Defaults to \code{grams::cmu$phonemes_1char}.
#' @param method Distance method passed to \code{stringdist}. Default is
#'   \code{"lv"}.
#'
#' @return Numeric count of neighbours at edit distance 1.
#' @export
ped1 <- function(phonemes,
                 lexicon = NULL,
                 method  = "lv")
{
  if (is.null(lexicon)) lexicon <- grams::cmu$phonemes_1char
  ped_n(phonemes, n = 1, lexicon = lexicon, method = method)
}

#' Return phonological neighbours at edit distance 1
#'
#' @param phonemes Either a character vector of ARPAbet tokens or a
#'   space-separated ARPAbet string.
#' @param lexicon Character vector of pre-encoded phoneme strings to compare
#'   against. Defaults to \code{grams::cmu$phonemes_1char}.
#' @param method Distance method passed to \code{stringdist}. Default is
#'   \code{"lv"}.
#'
#' @return Character vector of encoded neighbour strings at edit distance 1.
#' @export
phonological_neighbours_list <- function(phonemes,
                                         lexicon = NULL,
                                         method  = "lv")
{
  if (is.null(lexicon)) lexicon <- grams::cmu$phonemes_1char
  phonological_neighbours(phonemes, lexicon, n = 1, method = method,
                          output = "neighbours")
}
