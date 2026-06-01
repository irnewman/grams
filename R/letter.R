
# package-level constant, defined once
.VOWELS <- c("A", "E", "I", "O", "U", "Y")
.UNCOMMON <- c("J", "K", "Q", "V", "W", "X", "Z") # Novick & Sherman, 2004

#' Count the number of vowels in a word.
#'
#' @param word A word or anagram in string format.
#'
#' @return Numeric count of vowels.
#' @export
n_vowels <- function(word)
{
  sum(strsplit(toupper(word), "")[[1]] %in% .VOWELS)
}

#' Count the number of consonants in a word.
#'
#' @param word A word or anagram in string format.
#'
#' @return Numeric count of consonants.
#' @export
n_consonants <- function(word)
{
  nchar(word) - n_vowels(word)
}

#' Compute the ratio of vowels to total letters in a word.
#'
#' @param word A word or anagram in string format.
#'
#' @return Numeric ratio of vowels to word length.
#' @export
vowel_ratio <- function(word)
{
  n_vowels(word) / nchar(word)
}

#' Identify whether the first letter of a word is a vowel or consonant.
#'
#' @param word A word or anagram in string format.
#'
#' @return Character string, either "vowel" or "consonant".
#' @export
first_letter <- function(word)
{
  ifelse(strsplit(toupper(word), "")[[1]][1] %in% .VOWELS, "vowel", "consonant")
}

#' Identify whether the word contains an infrequent letter: "J", "K", "Q", "V",
#' "W", "X", or "Z".
#'
#' @param word A word or anagram in string format.
#'
#' @return Character string, either "vowel" or "consonant".
#' @export
infreq_letter <- function(word)
{
  any(strsplit(toupper(word), "")[[1]] %in% .UNCOMMON)
}

#' Count the number of unique letters in a word.
#'
#' @param word A word or anagram in string format.
#'
#' @return Numeric count of unique letters.
#' @export
n_unique_letters <- function(word)
{
  length(unique(strsplit(toupper(word), "")[[1]]))
}

#' Compute the ratio of unique letters to total letters in a word.
#'
#' @param word A word or anagram in string format.
#'
#' @return Numeric ratio of distinct letters to word length.
#' @export
unique_letter_ratio <- function(word)
{
  n_unique_letters(word) / nchar(word)
}

#' Finds letters that are the same in solution and anagram at the same position.
#'
#' @param solution The solution word in string format.
#' @param anagram Anagram of the solution word in string format.
#' @param output One of "letters" or "indices". Default "letters".
#'
#' @return Character vector of intact letters (uppercase) in left-to-right order
#'   if output = "letters", or numeric vector of positions if output = "indices".
#' @export
intact_letters <- function(solution, anagram, output = "letters")
{
  # determine letters of solution and anagram
  sol_letters <- tolower(unlist(strsplit(solution, "")))
  ana_letters <- tolower(unlist(strsplit(anagram, "")))
  # find matching letters at each index of solution and anagram
  intact_idx <- which(sol_letters == ana_letters)

  if (length(intact_idx) == 0) {
    if (output == "letters")  return(NA_character_)
    if (output == "indices")  return(NA_integer_)
  }

  if (output == "indices") return(intact_idx)
  if (output == "letters") return(sol_letters[intact_idx])
}

#' Finds which bigrams appear in both solution and anagram.
#'
#' @param solution The solution word in string format.
#' @param anagram Anagram of the solution word in string format.
#' @param output One of "bigrams" or "indices". Default "bigrams".
#'
#' @return Character vector of preserved bigrams (uppercase) in left-to-right
#'   order if output = "bigrams", or numeric vector of starting positions if
#'   output = "indices".
#' @export
preserved_bg <- function(solution, anagram, output = "bigrams")
{
  sol <- strsplit(tolower(solution), "")[[1]]
  ana <- strsplit(tolower(anagram),  "")[[1]]
  n   <- length(sol) - 1L
  sol_bigrams <- paste0(sol[-length(sol)], sol[-1])
  ana_bigrams <- paste0(ana[-length(ana)], ana[-1])

  # find which solution bigrams appear anywhere in the anagram
  result <- sol_bigrams[sol_bigrams %in% ana_bigrams]

  if (length(result) == 0) {
    if (output == "bigrams") return(NA_character_)
    if (output == "indices") return(NA_integer_)
  }

  if (output == "indices") {
    # return positions in solution where those bigrams occur
    return(which(sol_bigrams %in% ana_bigrams))
  }
  if (output == "bigrams") return(result)
}
