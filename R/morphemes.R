
#' Count or list morphemes embedded in a word.
#'
#' Finds morphemes from a provided list that appear in the word, removing
#' shorter morphemes that are fully embedded within longer found morphemes
#' (e.g. if "pre" and "re" are both found, only "pre" is kept).
#'
#' @param word A word or anagram in string format.
#' @param output One of "count", "list", or "indices". Default "list".
#' @param morph_list A data frame with a column named "morpheme". Defaults
#'   to the package morpheme list.
#'
#' @return Numeric count if output = "count", character vector of morphemes
#'   in left-to-right order (uppercase) if output = "list", or numeric vector
#'   of starting positions if output = "indices".
#' @export
find_morphemes <- function(word, output = "list", morph_list = NULL)
{
  if (is.null(morph_list)) morph_list <- morpheme_list
  word      <- tolower(word)
  morphemes <- morph_list$morpheme

  # find all matching morphemes and their positions
  positions <- sapply(morphemes, function(m) regexpr(m, word, fixed = TRUE)[1])
  found     <- positions != -1

  if (!any(found)) {
    if (output == "count")   return(0L)
    if (output == "list")    return(character(0))
    if (output == "indices") return(integer(0))
  }
  matches <- data.frame(
    morpheme = morphemes[found],
    start    = as.integer(positions[found]),
    end      = as.integer(positions[found]) + nchar(morphemes[found]) - 1L,
    keep     = TRUE,
    stringsAsFactors = FALSE
  )

  # sort by length descending so longer morphemes take priority
  matches <- matches[order(nchar(matches$morpheme), decreasing = TRUE), ]

  # remove morphemes fully embedded within any longer morpheme
  # e.g. "re" is dropped if "pre" is also found
  for (i in seq_len(nrow(matches))) {
    if (!matches$keep[i]) next
    for (j in seq_len(nrow(matches))) {
      if (i == j || !matches$keep[j]) next
      if (nchar(matches$morpheme[j]) < nchar(matches$morpheme[i]) &&
          matches$start[j] >= matches$start[i] &&
          matches$end[j]   <= matches$end[i]) {
        matches$keep[j] <- FALSE
      }
    }
  }

  # keep only morphemes that passed filtering
  kept_matches <- matches[matches$keep, ]
  # sort by start position for left-to-right order
  kept_matches <- kept_matches[order(kept_matches$start), ]

  if (output == "count")   return(nrow(kept_matches))
  if (output == "indices") return(kept_matches$start)
  if (output == "list")    return(tolower(kept_matches$morpheme))
}
