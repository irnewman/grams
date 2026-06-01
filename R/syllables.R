
#' Syllabify a word and estimate syllable count.
#'
#' Looks up syllabification from the CMU Pronouncing Dictionary if available.
#' For non-dictionary words, runs both gruut (rule-based) and g2p (neural)
#' transcriptions and compares their outputs. Gruut is used for syllabification
#' structure (preserves consonant clusters); g2p is used for syllable count
#' estimation (reflects likely pronunciation). If gruut flags the transcription
#' as defective, g2p is used for both.
#'
#' @param word A word or anagram in string format.
#' @param output One of "count" or "full".
#'
#' @return If output = "count", a data frame with syllable count, source,
#'   defective flag, and agreement between gruut and g2p. If output = "full",
#'   a full syllabification data frame with phoneme-level detail.
#' @export
syllables <- function(word, output = "count")
{
  load_phonetics()
  word <- tolower(word)

  if (word %in% cmu$word) {
    # CMU
    source         <- "cmu"
    pronunciations <- cmu$pronunciation[grams::cmu$word == word]
    syllabified    <- lapply(pronunciations, syllabifyr::syllabify)
    syll_counts    <- sapply(syllabified, function(s) max(s$syll))
    s              <- mean(syll_counts)

    if (output == "count") {
      return(data.frame(
        syllables       = s,
        syllables_gruut = NA_real_,
        syllables_g2p   = NA_real_,
        source          = source,
        defective       = FALSE,
        agree           = NA,
        stringsAsFactors = FALSE
      ))
    }

    if (output == "full") {
      s_frame <- dplyr::bind_rows(lapply(seq_along(syllabified), function(i) {
        sf                  <- as.data.frame(syllabified[[i]])
        sf$source           <- source
        sf$num              <- i
        sf$defective        <- FALSE
        sf$syllables_g2p    <- NA_real_
        sf$syllables_gruut  <- NA_real_
        sf$g2p_phones       <- NA_character_
        sf
      }))
      return(s_frame)
    }
  } else {
    # non-CMU: run both gruut and g2p

    # gruut
    gruut_phones    <- get_phonemes_gruut(word)
    gruut_defective <- FALSE
    gruut_syl       <- NULL

    if (length(gruut_phones) > 0) {
      gruut_syl <- tryCatch(
        withCallingHandlers(
          syllabifyr::syllabify(gruut_phones),
          warning = function(w) {
            if (grepl("defective", conditionMessage(w)))
              gruut_defective <<- TRUE
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) NULL
      )
    }

    # g2p
    g2p_arpa <- tryCatch({
      raw <- as.character(.phonetics_cache$g2p(word))
      raw <- gsub("[0-9]", "", raw)
      raw[nchar(raw) > 0]
    }, error = function(e) character(0))

    g2p_syl <- if (length(g2p_arpa) > 0) {
      tryCatch(
        syllabifyr::syllabify(g2p_arpa),
        warning = function(w) invokeRestart("muffleWarning"),
        error   = function(e) NULL
      )
    } else {
      NULL
    }

    # syllable counts
    s_gruut <- if (!is.null(gruut_syl) && !gruut_defective)
      max(gruut_syl$syll) else NA_real_
    s_g2p   <- if (!is.null(g2p_syl)) max(g2p_syl$syll) else NA_real_

    # default syllable count: g2p when available, gruut as fallback
    s <- if (!is.na(s_g2p)) s_g2p else s_gruut

    # agreement between gruut and g2p
    agree <- if (!is.na(s_gruut) && !is.na(s_g2p)) s_gruut == s_g2p else NA

    # source for syllabification structure
    source <- if (!gruut_defective && !is.null(gruut_syl)) "gruut" else "g2p"

    if (output == "count") {
      return(data.frame(
        syllables       = s,
        syllables_gruut = s_gruut,
        syllables_g2p   = s_g2p,
        source          = source,
        defective       = gruut_defective,
        agree           = agree,
        stringsAsFactors = FALSE
      ))
    }

    if (output == "full") {
      # use gruut syllabification when available and not defective
      # fall back to g2p syllabification
      base_syl <- if (!gruut_defective && !is.null(gruut_syl)) {
        gruut_syl
      } else {
        g2p_syl
      }

      if (is.null(base_syl)) {
        s_frame <- data.frame(
          syll = NA, part = NA, phone = NA, stress = NA,
          source = source, num = 1L, defective = gruut_defective,
          syllables_g2p = s_g2p, syllables_gruut = s_gruut,
          g2p_phones = paste(g2p_arpa, collapse = " "),
          stringsAsFactors = FALSE
        )
      } else {
        s_frame                  <- as.data.frame(base_syl)
        s_frame$source           <- source
        s_frame$num              <- 1L
        s_frame$defective        <- gruut_defective
        s_frame$syllables_g2p    <- s_g2p
        s_frame$syllables_gruut  <- s_gruut
        s_frame$g2p_phones       <- paste(g2p_arpa, collapse = " ")
      }
    }
  }

  if (output == "full") return(s_frame)
}
