
#' GRAMS database
#'
#' A data frame containing psycholinguistic indices for source words and their
#' anagram candidates, computed via \code{\link{compute_string_indices}} and
#' \code{\link{compute_anagram_indices}}. See \code{\link{load_grams_database}}
#' for the recommended way to access the full database.
#'
#' @format A data frame with 81 columns. See the README for a full description
#'   of all columns.
"GRAMS"

#' SUBTLEX-UK frequency norms
#'
#' Word frequency norms from the SUBTLEX-UK corpus (van Heuven et al., 2014).
#' Used as the reference corpus for frequency scoring and classifier training.
#'
#' @format A data frame with columns including \code{word}, \code{Zipf},
#'   \code{FreqCount}, \code{CD}, \code{DomPoS}, and others.
#'
#' @source \url{http://crr.ugent.be/archives/1423}
"subtlex_uk"

#' SUBTLEX-US frequency norms
#'
#' Word frequency norms from the SUBTLEX-US corpus (Brysbaert & New, 2009).
#'
#' @format A data frame with columns including \code{word}, \code{Zipf},
#'   \code{FREQcount}, \code{CDcount}, \code{Dom_PoS_SUBTLEX}, and others.
#'
#' @source \url{http://crr.ugent.be/archives/4}
"subtlex_us"

#' CMU Pronouncing Dictionary
#'
#' A data frame of words and their phonemic transcriptions from the Carnegie
#' Mellon University Pronouncing Dictionary, used for syllabification and
#' sonority scoring.
#'
#' @format A data frame with columns \code{word}, \code{pronunciation},
#'   \code{phonemes_stressed}, and \code{phonemes}.
#'
#' @source \url{http://www.speech.cs.cmu.edu/cgi-bin/cmudict}
"cmu"

#' GCIDE lexicon
#'
#' A data frame of words and parts of speech from the GNU Collaborative
#' International Dictionary of English, used as one of four sources for
#' building the internal dictionary.
#'
#' @format A data frame with columns \code{word} and \code{pos}.
"gcide"

#' WordNet lexicon
#'
#' A character vector of words from the WordNet lexical database, used as
#' one of four sources for building the internal dictionary.
#'
#' @format A data frame with column \code{word}.
#'
#' @source \url{https://wordnet.princeton.edu}
"wordnet"

#' Internal dictionary
#'
#' A character vector of approximately 89,000 English words used for anagram
#' lookup via \code{\link{solve_anagram}}. Built from four lexical sources
#' (CMU, GCIDE, WordNet, SUBTLEX-UK) filtered with Hunspell spell-checking.
#'
#' @format A character vector.
"internal_dict"

#' Signature index
#'
#' A named list mapping sorted-letter signatures to character vectors of
#' dictionary words sharing that signature. Built from \code{internal_dict}
#' via \code{\link{build_signature_index}} and used by
#' \code{\link{solve_anagram}} for fast anagram lookup.
#'
#' @format A named list of character vectors.
"sig_index"

#' Morpheme list
#'
#' A tibble of common English morphemes and their types, used by
#' \code{find_morphemes} to identify morphological structure in words.
#'
#' @format A tibble with 132 rows and 2 columns:
#'   \describe{
#'     \item{morpheme}{The morpheme string.}
#'     \item{type}{Morpheme type, currently \code{"suffix"}.}
#'   }
"morpheme_list"

#' HurtLex offensive lexicon
#'
#' A lexicon of harmful and offensive words used for censoring stimulus
#' candidates. From Bassignana et al. (2018).
#'
#' @format A data frame with columns \code{id}, \code{pos}, \code{category},
#'   \code{stereotype}, \code{lemma}, and \code{level}.
#'
#' @source \url{https://github.com/valeriobasile/hurtlex}
"hurtlex"

#' LDNOOBW offensive word list
#'
#' A list of offensive words used for censoring stimulus candidates.
#' "List of Dirty, Naughty, Obscene, and Otherwise Bad Words."
#'
#' @format A data frame with column \code{word}.
#'
#' @source \url{https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words}
"ldnoobw"

#' Mahalanobis classifier center
#'
#' Named numeric vector of feature means from the SUBTLEX-UK reference
#' distribution, used as the center point for Mahalanobis distance
#' classification in \code{\link{classify_string}}.
#'
#' @format A named numeric vector with elements \code{mlbf}, \code{old20},
#'   \code{articulability}, and \code{vowel_ratio}.
"classifier_center"

#' Mahalanobis classifier covariance matrix
#'
#' Covariance matrix of features from the SUBTLEX-UK reference distribution,
#' used in \code{\link{classify_string}}.
#'
#' @format A 4x4 numeric matrix with row and column names corresponding to
#'   \code{mlbf}, \code{old20}, \code{articulability}, and \code{vowel_ratio}.
"classifier_cov"

#' Mahalanobis classifier threshold
#'
#' The 95th percentile of Mahalanobis distances from the SUBTLEX-UK reference
#' distribution, used as the classification boundary in
#' \code{\link{classify_string}}.
#'
#' @format A single numeric value.
"classifier_threshold"

#' SUBTLEX-UK bigram frequency cache (positional, type)
#' @format A list with \code{matrices} and \code{length_counts}.
#' @keywords internal
"subtlex_uk_bigram_cache"

#' SUBTLEX-UK bigram frequency cache (non-positional, type)
#' @format A list with \code{by_length}, \code{overall}, and \code{length_counts}.
#' @keywords internal
"subtlex_uk_bigram_nonpos_cache"

#' SUBTLEX-UK bigram frequency cache (positional, token)
#' @format A list with \code{matrices} and \code{length_counts}.
#' @keywords internal
"subtlex_uk_bigram_token_cache"

#' SUBTLEX-UK letter frequency cache (positional, type)
#' @format A list with \code{matrices} and \code{length_counts}.
#' @keywords internal
"subtlex_uk_letter_cache"

#' SUBTLEX-UK letter frequency cache (non-positional, type)
#' @format A list with \code{by_length}, \code{overall}, and \code{length_counts}.
#' @keywords internal
"subtlex_uk_letter_nonpos_cache"

#' SUBTLEX-UK letter frequency cache (positional, token)
#' @format A list with \code{matrices} and \code{length_counts}.
#' @keywords internal
"subtlex_uk_letter_token_cache"

#' SUBTLEX-UK trigram frequency cache (positional, type)
#' @format A list with \code{matrices} and \code{length_counts}.
#' @keywords internal
"subtlex_uk_trigram_cache"

#' SUBTLEX-UK trigram frequency cache (non-positional, type)
#' @format A list with \code{by_length}, \code{overall}, and \code{length_counts}.
#' @keywords internal
"subtlex_uk_trigram_nonpos_cache"

#' SUBTLEX-UK trigram frequency cache (positional, token)
#' @format A list with \code{matrices} and \code{length_counts}.
#' @keywords internal
"subtlex_uk_trigram_token_cache"
