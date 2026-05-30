
# phonotactic exceptions
.onset_exceptions <- c("SPL", "SPR", "STR", "SKR", "SKW")
.coda_exceptions  <- c("NGTHS", "MPST")

#' Phonotactic rules table
#'
#' A lookup table of English phonotactic constraints for onset and coda
#' clusters. Used by \code{check_phonotactics()} to identify illegal or
#' rare consonant clusters in syllabified strings. Rules are derived from
#' Stewart and Hadley (personal communication, 2026) and Hammond (1999).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{position}{Either "onset" or "coda"}
#'   \item{penalty}{Either "illegal" or "rare"}
#'   \item{cluster}{ARPAbet cluster string}
#' }
#' @export
phonotactic_rules <- tibble::tribble(
  ~position, ~penalty, ~cluster,

  # onset rules
  # --------------------------------------------

  # illegal onset: /ŋ/ word-initially
  "onset",   "illegal",  "NG",

  # illegal onset clusters
  "onset",   "illegal",  "SB",
  "onset",   "illegal",  "SD",
  "onset",   "illegal",  "SG",
  "onset",   "illegal",  "DL",
  "onset",   "illegal",  "TL",
  "onset",   "illegal",  "SR",
  "onset",   "illegal",  "ZR",
  "onset",   "illegal",  "ZB",
  "onset",   "illegal",  "ZD",
  "onset",   "illegal",  "ZG",

  # nasal assimilation violations
  "onset",   "illegal",  "NP",
  "onset",   "illegal",  "NGP",
  "onset",   "illegal",  "MT",
  "onset",   "illegal",  "NT",
  "onset",   "illegal",  "NK",
  "onset",   "illegal",  "ML",

  # additional illegal onsets
  "onset",   "illegal",  "KN",
  "onset",   "illegal",  "GN",

  # rare onsets
  "onset",   "rare",     "THR",
  "onset",   "rare",     "SHR",
  "onset",   "rare",     "PW",
  "onset",   "rare",     "BW",

  # coda rules
  # --------------------------------------------

  # illegal coda clusters
  "coda",    "illegal",  "SB",
  "coda",    "illegal",  "SD",
  "coda",    "illegal",  "SG",
  "coda",    "illegal",  "DL",
  "coda",    "illegal",  "TL",
  "coda",    "illegal",  "SR",
  "coda",    "illegal",  "ZR",
  "coda",    "illegal",  "PT",
  "coda",    "illegal",  "BD",
  "coda",    "illegal",  "GZ",

  # illegal word-final segments
  "coda",    "illegal",  "HH",
  "coda",    "illegal",  "W",
  "coda",    "illegal",  "Y",

  # rare coda clusters
  "coda",    "rare",     "MP",
  "coda",    "rare",     "NT",
  "coda",    "rare",     "LK",
  "coda",    "rare",     "LD",
  "coda",    "rare",     "RD",
  "coda",    "rare",     "GD"
)

#' Check phonotactic violations in a syllabified word
#'
#' Internal helper for \code{articulability()}. Checks onset and coda clusters
#' against the phonotactic rules table, respecting exceptions and cluster
#' length constraints.
#'
#' @param df A syllabified data frame as returned by
#'   \code{syllables(output = "full")}.
#' @param rules Phonotactic rules table. Defaults to
#'   \code{grams::phonotactic_rules}.
#'
#' @return A list with \code{defective_flag} (logical) and
#'   \code{syllable_violations} (named numeric vector of violation counts
#'   per syllable).
#' @noRd
check_phonotactics <- function(df, rules = grams::phonotactic_rules)
{
  if (is.null(df) || !is.data.frame(df)) {
    return(list(
      defective_flag = FALSE,
      syllable_violations = numeric(0)
    ))
  }

  # defective flag
  defective_flag <- "defective" %in% names(df) && any(df$defective, na.rm = TRUE)

  # check each pronunciation separately
  num_ids <- unique(df$num)

  # for each pronunciation, get per-syllable violations
  pron_violations <- lapply(num_ids, function(num_id) {
    pron_df <- df[df$num == num_id, ]
    syll_ids <- unique(pron_df$syll)

    syll_penalties <- sapply(syll_ids, function(syll_id) {
      syll  <- pron_df[pron_df$syll == syll_id & !is.na(pron_df$syll), ]
      onset <- syll$phone[syll$part == "onset"]
      coda  <- syll$phone[syll$part == "coda"]
      syll_total <- 0

      #  check onset
      if (length(onset) > 0) {
        cluster <- paste(onset, collapse = "")
        if (!cluster %in% .onset_exceptions) {
          if (length(onset) >= 3) {
            syll_total <- syll_total + 1L
          } else {
            onset_rules <- rules[rules$position == "onset", ]
            for (i in seq_len(nrow(onset_rules))) {
              rule_cluster <- onset_rules$cluster[i]
              if (grepl(rule_cluster, cluster, fixed = TRUE)) {
                syll_total <- syll_total + 1L
                break
              }
            }
          }
        }
      }

      # check coda
      if (length(coda) > 0) {
        cluster <- paste(coda, collapse = "")
        is_exception <- any(sapply(.coda_exceptions, function(e)
          grepl(e, cluster, fixed = TRUE)))
        if (!is_exception) {
          if (length(coda) >= 4) {
            syll_total <- syll_total + 1L
          } else {
            coda_rules <- rules[rules$position == "coda", ]
            for (i in seq_len(nrow(coda_rules))) {
              rule_cluster <- coda_rules$cluster[i]
              if (grepl(rule_cluster, cluster, fixed = TRUE)) {
                penalty <- if (coda_rules$penalty[i] == "illegal")
                  penalty_illegal else penalty_rare
                syll_total <- syll_total + 1L
                break
              }
            }
          }
        }
      }

      syll_total
    })

    names(syll_penalties) <- syll_ids
    syll_penalties
  })

  # choose pronunciation with minimum total violations
  pron_totals <- sapply(pron_violations, sum)
  if (all(pron_totals == 0)) {
    best_pron_violations <- numeric(0)
  } else {
    best_idx <- which.min(pron_totals)
    best_pron_violations <- pron_violations[[best_idx]]
  }

  list(
    defective_flag = defective_flag,
    syllable_violations = best_pron_violations
  )
}

# sonority values
.plosive   <- 0
.fricative <- 1
.nasal     <- 2
.lateral   <- 3
.rhotic    <- 4
.vowel     <- 5

#' Sonority table
#'
#' A lookup table mapping ARPAbet phonemes to sonority values and descriptive
#' information. Used internally by \code{sonority()} and \code{check_ssp()}.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{sonority}{Numeric sonority value (0 = plosive, 5 = vowel)}
#'   \item{sound}{Phoneme class label}
#'   \item{ipa}{IPA symbol}
#'   \item{example}{Example word}
#'   \item{arpabet}{ARPAbet symbol used by CMU and g2p}
#' }
#' @export
sonority_table <- tibble::tribble(
  ~sonority,   ~sound,        ~ipa,    ~example,          ~arpabet,
  .plosive,    "plosive",     "t",     "[t] tip",         "T",
  .plosive,    "plosive",     "p",     "[p] page",        "P",
  .plosive,    "plosive",     "k",     "[k] cat",         "K",
  .plosive,    "plosive",     "b",     "[b] bug",         "B",
  .plosive,    "plosive",     "d",     "[d] bad",         "D",
  .plosive,    "plosive",     "g",     "[g] egg",         "G",
  .fricative,  "fricative",   "f",     "[f] fat",         "F",
  .fricative,  "fricative",   "θ",     "[θ] thing",       "TH",
  .fricative,  "fricative",   "ʃ",     "[ʃ] sure",        "SH",
  .fricative,  "fricative",   "tʃ",    "[ch] charge",     "CH",
  .fricative,  "fricative",   "s",     "[s] sarah",       "S",
  .fricative,  "fricative",   "ð",     "[ð] with",        "DH",
  .fricative,  "fricative",   "ʒ",     "[ʒ] treasure",    "ZH",
  .fricative,  "fricative",   "v",     "[v] value",       "V",
  .fricative,  "fricative",   "dʒ",    "[dʒ] judge",      "JH",
  .fricative,  "fricative",   "z",     "[z] zebra",       "Z",
  .fricative,  "fricative",   "h",     "[h] high",        "HH",
  .nasal,      "nasal",       "n",     "[n] net",         "N",
  .nasal,      "nasal",       "m",     "[m] man",         "M",
  .nasal,      "nasal",       "ŋ",     "[ŋ] ring",        "NG",
  .lateral,    "lateral",     "l",     "[l] live",        "L",
  .rhotic,     "rhotic",      "r",     "[r] run",         "R",
  .vowel,      "high_vowel",  "i",     "[i] it",          "IH",
  .vowel,      "high_vowel",  "ʊ",     "[ʊ] look",        "UH",
  .vowel,      "low_vowel",   "e",     "[e] end",         "EH",
  .vowel,      "low_vowel",   "ə",     "[aʊ] about",      "AW",
  .vowel,      "low_vowel",   "ʌ",     "[ʌ] blood",       "AH",
  .vowel,      "low_vowel",   "æ",     "[æ] cat",         "AE",
  .vowel,      "low_vowel",   "ɑː",    "[ɑː] arm",        "AA",
  .vowel,      "low_vowel",   "aɪ",    "[aɪ] kite",       "AY",
  .vowel,      "low_vowel",   "ɔ",     "[ɔ] ought",       "AO",
  .vowel,      "low_vowel",   "ɝ",     "[ɝ] bird",        "ER",
  .vowel,      "high_vowel",  "eɪ",    "[eɪ] bait",       "EY",
  .vowel,      "high_vowel",  "i",     "[i] eat",         "IY",
  .vowel,      "high_vowel",  "oʊ",    "[oʊ] boat",       "OW",
  .vowel,      "high_vowel",  "ɔɪ",    "[ɔɪ] toy",        "OY",
  .vowel,      "high_vowel",  "u",     "[u] boot",        "UW",
  .vowel,      "glide",       "w",     "[w] what",        "W",
  .vowel,      "glide",       "j",     "[j] yacht",       "Y"
)

#' Check for Sonority Sequencing Principle violations
#'
#' Internal helper for \code{articulability()}. Computes per-syllable sonority
#' statistics and identifies SSP violations. The SSP states that sonority
#' should rise toward the nucleus and fall away from it.
#'
#' @param df A syllabified data frame as returned by
#'   \code{syllables(output = "full")}, joined with \code{sonority_table}.
#'
#' @return A data frame with one row per syllable containing mean sonority,
#'   nucleus sonority, violation flags, and violation type labels.
#' @noRd
check_ssp <- function(df)
{
  df |>
    dplyr::group_by(syll) |>
    dplyr::summarise(
      mean_sonority    = mean(sonority, na.rm = TRUE),
      nucleus_sonority = {
        n <- sonority[part == "nucleus"]
        if (length(n) == 0) NA_real_ else n[1]
      },
      nucleus_is_peak  = {
        if (is.na(nucleus_sonority)) NA
        else nucleus_sonority == max(sonority, na.rm = TRUE)
      },
      onset_increasing = {
        onset_vals <- sonority[part == "onset"]
        if (length(onset_vals) <= 1) TRUE
        else all(diff(c(onset_vals, nucleus_sonority)) > 0, na.rm = TRUE)
      },
      coda_decreasing  = {
        coda_vals <- sonority[part == "coda"]
        if (length(coda_vals) <= 1) TRUE
        else all(diff(c(nucleus_sonority, coda_vals)) < 0, na.rm = TRUE)
      },
      violation_type   = {
        if (is.na(nucleus_is_peak)) {
          "no_nucleus"
        } else {
          v <- c(
            if (isFALSE(nucleus_is_peak))     "nucleus_not_peak",
            if (isFALSE(onset_increasing)) "onset_not_increasing",
            if (isFALSE(coda_decreasing))   "coda_not_decreasing"
          )
          if (length(v) == 0) "none" else paste(v, collapse = "; ")
        }
      },
      ssp_violation    = violation_type != "none",
      .groups          = "drop"
    )
}

#' Compute an articulability score for a word
#'
#' Assesses phonological wellformedness using two complementary approaches:
#'
#' \enumerate{
#'   \item \strong{Sonority Sequencing Principle (SSP)} — penalizes syllables
#'     where sonority does not rise toward the nucleus and fall away from it.
#'   \item \strong{Phonotactic rules} — penalizes illegal or rare consonant
#'     clusters in onsets and codas, and strings that gruut cannot transcribe.
#' }
#'
#' Penalties are applied multiplicatively — each violation reduces the score
#' by a proportion of its current value, ensuring scores remain non-negative.
#' A penalty of 0 means no reduction; a penalty of 1 reduces the score to 0.
#'
#' Transcription hierarchy: CMU dictionary words are used when available.
#' For non-dictionary words, gruut (rule-based) is preferred as it preserves
#' consonant clusters. g2p (neural) is used as fallback when gruut flags a
#' transcription as defective.
#'
#' All penalty weights are provisional and subject to empirical calibration
#' against pronounceability ratings.
#'
#' @param word A word or anagram as a string.
#' @param syllable_data Optional precomputed syllable data frame from
#'   \code{syllables(output = "full")}. If NULL, syllables() is called
#'   internally.
#' @param penalty_onset Proportional penalty per SSP onset violation.
#'   Default 0.71.
#' @param penalty_coda Proportional penalty per SSP coda violation.
#'   Default 0.91.
#' @param penalty_illegal Proportional penalty per illegal phonotactic cluster.
#'   Default 0.85.
#' @param penalty_rare Proportional penalty per rare phonotactic cluster.
#'   Default 0.0.
#' @param penalty_defective Proportional penalty when syllabification fails
#'   or is flagged as defective. Default 0.5.
#' @param rules Phonotactic rules table. Defaults to
#'   \code{grams::phonotactic_rules}.
#'
#' @return Numeric articulability score between 0 and 5. Higher values indicate
#'   greater sonority and fewer phonotactic violations. Scores are averaged
#'   across pronunciations for words with multiple CMU entries.
#' @export
articulability <- function(word,
                           syllable_data     = NULL,
                           penalty_onset     = 0.71,
                           penalty_coda      = 0.91,
                           penalty_illegal   = 0.85,
                           penalty_rare      = 0.0,
                           penalty_defective = 0.5,
                           rules             = grams::phonotactic_rules)
{
  # get syllabification
  if (is.null(syllable_data)) {
    syllable_data <- tryCatch(
      syllables(word, output = "full"),
      error = function(e) NULL
    )
  }

  # handle syllabification failure
  if (is.null(syllable_data) || all(is.na(syllable_data$phone))) {
    return((1 - penalty_defective))
  }

  # join sonority table
  s <- syllable_data |>
    dplyr::rename(arpabet = phone) |>
    dplyr::left_join(sonority_table, by = "arpabet")

  # get phonotactic violations per syllable
  phon_result <- check_phonotactics(syllable_data, rules = rules)

  # SSP score per pronunciation
  pron_scores <- sapply(unique(s$num), function(num_id) {
    pron <- s[s$num == num_id, ]
    ssp  <- check_ssp(pron)

    # penalize each syllable individually
    syllable_scores <- sapply(1:nrow(ssp), function(i) {
      score <- ssp$mean_sonority[i]
      syll_id <- ssp$syll[i]

      # apply SSP penalties
      has_onset_viol <- grepl("onset_not_increasing", ssp$violation_type[i])
      has_coda_viol <- grepl("coda_not_decreasing", ssp$violation_type[i])

      if (has_onset_viol) score <- score * (1 - penalty_onset)
      if (has_coda_viol) score <- score * (1 - penalty_coda)

      # apply phonotactic penalties for this syllable
      if (length(phon_result$syllable_violations) > 0) {
        syll_id_char <- as.character(syll_id)
        if (syll_id_char %in% names(phon_result$syllable_violations)) {
          n_violations <- phon_result$syllable_violations[[syll_id_char]]
          if (n_violations > 0) {
            # apply illegal penalty n_violations times
            score <- score * (1 - penalty_illegal)^n_violations
          }
        }
      }

      score
    })

    mean(syllable_scores)
  })

  base_score <- mean(pron_scores, na.rm = TRUE)

  # defective penalty (word-level)
  if ("defective" %in% names(syllable_data) &&
      any(syllable_data$defective, na.rm = TRUE)) {
    base_score <- base_score * (1 - penalty_defective)
  }

  # apply defective penalty from phonotactics, if present
  if (phon_result$defective_flag) {
    base_score <- base_score * (1 - penalty_defective)
  }

  base_score
}
