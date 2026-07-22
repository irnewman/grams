
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

#' GRAMS word-level index
#'
#' A data frame of psycholinguistic indices computed for every word in the
#' censored SUBTLEX-UK-derived word list, including orthographic frequency,
#' orthographic neighbourhood, phonemic frequency, phonemic neighbourhood,
#' articulability, syllable count, and other string-level properties. One
#' row per word.
#'
#' @format A data frame with one row per word and the following columns:
#'   \describe{
#'     \item{Word}{Lowercase orthographic word form.}
#'     \item{Zipf}{SUBTLEX-UK Zipf frequency.}
#'     \item{Length}{Orthographic word length in letters.}
#'     \item{SBF}{Summed bigram frequency.}
#'     \item{MLBF}{Mean log bigram frequency.}
#'     \item{OLD20}{Orthographic Levenshtein distance, 20 nearest neighbours.}
#'     \item{ED1}{Count of orthographic neighbours at edit distance 1.}
#'     \item{GTzero}{Proportion of letter-pair permutations forming attested bigrams.}
#'     \item{Articulability}{Articulability score.}
#'     \item{Nsyllables}{Estimated syllable count.}
#'     \item{SyllableSource}{Source of syllabification (cmu, gruut, or g2p).}
#'     \item{SBPF}{Summed biphone frequency.}
#'     \item{MLBPF}{Mean log biphone frequency.}
#'     \item{PLD20}{Phonological Levenshtein distance, 20 nearest neighbours.}
#'     \item{PED1}{Count of phonological neighbours at edit distance 1.}
#'     \item{Nphonemes}{Number of phonemes in the estimated pronunciation.}
#'     \item{Nvowels}{Number of vowels.}
#'     \item{Nconsonants}{Number of consonants.}
#'     \item{Vratio}{Ratio of vowels to total letters.}
#'     \item{FirstLetter}{Whether the first letter is a vowel or consonant.}
#'     \item{InfreqLetter}{Whether the word contains an infrequent letter
#'       (J, K, Q, V, W, X, Z).}
#'     \item{NuniqueLetters}{Number of unique letters.}
#'     \item{ULratio}{Ratio of unique letters to total letters.}
#'     \item{Morphemes}{Comma-separated list of identified morphemes.}
#'     \item{Nmorphemes}{Number of identified morphemes.}
#'     \item{SBFnp}{Non-positional summed bigram frequency.}
#'     \item{SBFt}{Token-weighted summed bigram frequency.}
#'     \item{MLBFnp}{Non-positional mean log bigram frequency.}
#'     \item{MLBFt}{Token-weighted mean log bigram frequency.}
#'     \item{SLF}{Summed letter frequency.}
#'     \item{STF}{Summed trigram frequency.}
#'     \item{MLLF}{Mean log letter frequency.}
#'     \item{MLTF}{Mean log trigram frequency.}
#'     \item{SUPF}{Summed phoneme unigram frequency.}
#'     \item{STPF}{Summed triphone frequency.}
#'     \item{MLUPF}{Mean log phoneme unigram frequency.}
#'     \item{MLTPF}{Mean log triphone frequency.}
#'     \item{HasSpellingVariant}{Whether the word has a homophonic spelling
#'       variant in SUBTLEX-UK.}
#'     \item{IsCompound}{Whether the word is a double-word compound entry
#'       in SUBTLEX-UK.}
#'     \item{HomophonicEntry}{Homophonic entry reference from SUBTLEX-UK.}
#'     \item{DoubleWordEntry}{Double-word entry reference from SUBTLEX-UK.}
#'   }
#'
#' @source Derived from SUBTLEX-UK, the CMU Pronouncing Dictionary, and the
#'   internal package dictionary, with censoring applied (see package
#'   vignette/methods documentation for details).
"GRAMS_index"

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
#' Mellon University Pronouncing Dictionary, used for syllabification,
#' sonority scoring, and phonological neighbourhood computation. Junk entries
#' (punctuation-naming artifacts and digit-leading strings) have been removed,
#' retaining 133,794 of the original 133,853 entries.
#'
#' @format A data frame with the following columns:
#'   \describe{
#'     \item{word}{Lowercase orthographic word form.}
#'     \item{pronunciation}{Space-separated ARPAbet string with stress markers
#'       (e.g. \code{"K AE1 T"}).}
#'     \item{phonemes_stressed}{List-column of ARPAbet tokens with stress
#'       markers retained.}
#'     \item{phonemes}{List-column of ARPAbet tokens with stress markers
#'       stripped (e.g. \code{c("K", "AE", "T")}).}
#'     \item{phonemes_1char}{Single-character encoded phoneme string for use
#'       in phonological neighbourhood computation via \code{stringdist}
#'       (e.g. \code{"k@t"}).}
#'     \item{ipa}{IPA transcription string (e.g. \code{"kæt"}).}
#'   }
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

#' CMU phoneme frequency cache (positional, type)
#' @format A list with \code{matrices} and \code{length_counts}.
#' @keywords internal
"cmu_phoneme_cache"

#' CMU phoneme frequency cache (non-positional, type)
#' @format A list with \code{by_length}, \code{overall}, and \code{length_counts}.
#' @keywords internal
"cmu_phoneme_nonpos_cache"

#' CMU biphone frequency cache (positional, type)
#' @format A list with \code{matrices} and \code{length_counts}.
#' @keywords internal
"cmu_biphone_cache"

#' CMU biphone frequency cache (non-positional, type)
#' @format A list with \code{by_length}, \code{overall}, and \code{length_counts}.
#' @keywords internal
"cmu_biphone_nonpos_cache"

#' CMU triphone frequency cache (positional, type)
#' @format A list with \code{matrices} and \code{length_counts}.
#' @keywords internal
"cmu_triphone_cache"

#' CMU triphone frequency cache (non-positional, type)
#' @format A list with \code{by_length}, \code{overall}, and \code{length_counts}.
#' @keywords internal
"cmu_triphone_nonpos_cache"
