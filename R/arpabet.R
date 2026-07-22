# ARPAbet phoneme inventory (44 phonemes, stress markers excluded).
ARPABET <- c(
  # vowels
  "AA", "AE", "AH", "AO", "AW", "AY",
  "EH", "ER", "EY",
  "IH", "IY",
  "OW", "OY",
  "UH", "UW",
  # auxiliary/reduced vowels
  "AX", "AXR", "IX", "UX",
  # consonants
  "B",  "CH", "D",  "DH", "F",  "G",
  "HH", "JH", "K",  "L",  "M",  "N",
  "NG", "P",  "R",  "S",  "SH", "T",
  "TH", "V",  "W",  "Y",  "Z",  "ZH",
  # glottal stop
  "Q"
)

ARPABET_TO_1CHAR <- c(
  # vowels
  "AA" = "a", "AE" = "@", "AH" = "A", "AO" = "c", "AW" = "W", "AY" = "Y",
  "EH" = "E", "ER" = "R", "EY" = "e", "IH" = "I", "IY" = "i",
  "OW" = "o", "OY" = "O", "UH" = "U", "UW" = "u",
  # auxiliary vowels
  "AX" = "x", "IX" = "X", "UX" = "&", "AXR" = "%",
  # consonants
  "B"  = "b", "CH" = "C", "D"  = "d", "DH" = "D", "F"  = "f", "G"  = "g",
  "HH" = "h", "JH" = "J", "K"  = "k", "L"  = "l", "M"  = "m", "N"  = "n",
  "NG" = "G", "P"  = "p", "Q"  = "Q", "R"  = "r", "S"  = "s", "SH" = "S",
  "T"  = "t", "TH" = "T", "V"  = "v", "W"  = "w", "Y"  = "y", "Z"  = "z",
  "ZH" = "Z"
)

ARPABET_TO_IPA <- c(
  # vowels
  "AA" = "\u0251", "AE" = "\u00e6", "AH" = "\u028c", "AO" = "\u0254",
  "AW" = "a\u028a", "AY" = "a\u026a", "EH" = "\u025b", "ER" = "\u025d",
  "EY" = "e\u026a", "IH" = "\u026a", "IY" = "i", "OW" = "o\u028a",
  "OY" = "\u0254\u026a", "UH" = "\u028a", "UW" = "u",
  # auxiliary vowels
  "AX" = "\u0259", "IX" = "\u0268", "UX" = "\u0289", "AXR" = "\u025a",
  # consonants
  "B"  = "b",         "CH" = "t\u0283", "D"  = "d",  "DH" = "\u00f0",
  "F"  = "f",         "G"  = "\u0261",  "HH" = "h",  "JH" = "d\u0292",
  "K"  = "k",         "L"  = "l",       "M"  = "m",  "N"  = "n",
  "NG" = "\u014b",    "P"  = "p",       "Q"  = "\u0294", "R" = "\u0279",
  "S"  = "s",         "SH" = "\u0283",  "T"  = "t",  "TH" = "\u03b8",
  "V"  = "v",         "W"  = "w",       "Y"  = "j",  "Z"  = "z",
  "ZH" = "\u0292"
)

#' Strip ARPAbet stress markers
#'
#' Removes trailing stress digits (0, 1, 2) from ARPAbet phoneme tokens.
#' For example, \code{"AE1"} becomes \code{"AE"} and \code{"AH0"} becomes
#' \code{"AH"}. Called internally before frequency lookups and table building.
#'
#' This function assumes standard ARPAbet stress notation, where a trailing
#' 0/1/2 digit marks vowel stress and no phoneme symbol otherwise ends in a
#' digit. It is not safe to apply to transcription systems that use trailing
#' digits for other purposes (e.g. tone or syllable marking in some non-English
#' phoneme sets), as this would silently strip meaningful information. IPA
#' input is unaffected, since IPA stress marks are not digits.
#'
#' @param phonemes A character vector of ARPAbet tokens.
#'
#' @return A character vector of the same length with stress markers removed.
#'
#' @keywords internal
strip_arpabet_stress <- function(phonemes) {
  gsub("[012]$", "", phonemes)
}

#' Tokenise an ARPAbet phoneme sequence into phoneme tokens
#'
#' Accepts either a single space-separated ARPAbet string (e.g.
#' \code{"K AE1 T"}) or an already-tokenized character vector (e.g.
#' \code{c("K", "AE1", "T")}, as produced by some g2p pipelines or stored in
#' list-columns) and returns a clean vector of phoneme tokens with stress
#' markers stripped. Tidies the formatting of phonemes for other functions.
#'
#' @param phonemes Either a single character string of space-separated
#'   ARPAbet phonemes, or a character vector of individual phoneme tokens.
#'
#' @return A character vector of phoneme tokens with stress markers removed.
#'
#' @keywords internal
tokenise_arpabet <- function(phonemes) {
  if (length(phonemes) == 1L) {
    tokens <- strsplit(trimws(phonemes), "\\s+")[[1]]
  } else {
    tokens <- phonemes
  }
  strip_arpabet_stress(tokens)
}
