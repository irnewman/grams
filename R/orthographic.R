
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
orthographic_neighbours <- function(word,
                                    dictionary = subtlex_uk$word,
                                    n          = 20,
                                    method     = "lv",
                                    output     = "both")
{
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
old_n <- function(word,
                  n          = 20,
                  dictionary = subtlex_uk$word,
                  method     = "lv")
{
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
old20 <- function(word,
                  dictionary = subtlex_uk$word,
                  method     = "lv")
{
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
ed_n <- function(word,
                 n          = 1,
                 dictionary = subtlex_uk$word,
                 method     = "lv")
{
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
ed1 <- function(word,
                dictionary = subtlex_uk$word,
                method     = "lv")
{
  ed_n(word, n = 1, dictionary = dictionary, method = method)
}

#' Return orthographic neighbours of a word at edit distance 1.
#'
#' @param word A word or anagram in string format.
#' @param dictionary Character vector of dictionary words to compare against.
#' @param method Distance method passed to stringdist. Default is "lv".
#'
#' @return Character vector of neighbouring words.
orthographic_neighbours_list <- function(word,
                                         dictionary = subtlex_uk$word,
                                         method     = "lv")
{
  orthographic_neighbours(word, dictionary, n = 1, method = method,
                          output = "neighbours")
}
