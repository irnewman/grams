
#' Build a signature index from a dictionary
#'
#' Groups dictionary words by their sorted-letter signature, enabling fast
#' anagram lookup via \code{\link{solve_anagram}}. By default the package
#' uses \code{grams::sig_index}, which is built from the internal dictionary
#' at load time. Use this function if you prefer to supply your own dictionary.
#'
#' @param dictionary A character vector of words. See \code{grams::internal_dict}
#'   for the format expected.
#'
#' @return A named list where each name is a sorted-letter signature and each
#'   element is a character vector of dictionary words sharing that signature.
#'
#' @examples
#' my_dict <- c("eat", "tea", "ate", "dog", "god")
#' my_index <- build_signature_index(my_dict)
#' solve_anagram("eat", sig_index = my_index)
#'
#' @export
build_signature_index <- function(dictionary = grams::internal_dict)
{
  sigs <- vapply(dictionary,
                 function(w) paste0(sort(strsplit(w, "")[[1]]), collapse = ""),
                 "")
  split(dictionary, sigs)
}

#' Solve an anagram to find matching dictionary words
#'
#' @param string Character string to solve
#' @param sig_index Signature index (default: grams::sig_index)
#' @param include_self Logical, whether to include the original string in results (default: FALSE)
#' @return Character vector of words that can be formed from the letters
#' @export
solve_anagram <- function(string,
                          sig_index = grams::sig_index,
                          include_self = FALSE)
{

  # compute signature of input string
  sig <- paste0(sort(strsplit(tolower(string), "")[[1]]), collapse = "")

  # find in signature index
  solutions <- sig_index[[sig]]

  if (is.null(solutions)) {
    return(character(0))  # No solutions found
  }

  # remove the original string unless requested
  if (!include_self) {
    solutions <- solutions[solutions != tolower(string)]
  }

  return(solutions)
}
