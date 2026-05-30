
# internal constants and cache
.phonetics_envname <- "r-phonetics"
.phonetics_cache        <- new.env(parent = emptyenv())
.phonetics_cache$ready  <- FALSE
.phonetics_cache$g2p    <- NULL      # g2p model
.phonetics_cache$gruut  <- FALSE     # gruut availability flag

# internal helper functions
.phonetics_python_bin <- function(envname = .phonetics_envname) {
  reticulate::virtualenv_python(envname)
}

.phonetics_env_active <- function(envname = .phonetics_envname) {
  if (!reticulate::py_available(initialize = FALSE)) return(FALSE)
  env_bin    <- .phonetics_python_bin(envname)
  active_bin <- reticulate::py_config()$python
  tryCatch(
    normalizePath(active_bin) == normalizePath(env_bin),
    error = function(e) FALSE
  )
}

.phonetics_nltk_ready <- function() {
  tryCatch({
    reticulate::py_run_string("
import nltk
for pkg in ['taggers/averaged_perceptron_tagger_eng', 'corpora/cmudict']:
    nltk.data.find(pkg)
")
    TRUE
  }, error = function(e) FALSE)
}

.g2p_ready <- function(envname = .phonetics_envname) {
  reticulate::virtualenv_exists(envname) &&
    file.exists(.phonetics_python_bin(envname)) &&
    reticulate::py_module_available("g2p_en") &&
    .phonetics_nltk_ready()
}

.gruut_ready <- function(envname = .phonetics_envname) {
  reticulate::virtualenv_exists(envname) &&
    file.exists(.phonetics_python_bin(envname)) &&
    reticulate::py_module_available("gruut")
}

# IPA to ARPAbet mapping
.gruut_py_code <- "
from gruut import sentences

def get_arpabet_gruut(text):
    mapping = {
        'p': 'P', 'b': 'B', 't': 'T', 'd': 'D', 'k': 'K', 'g': 'G',
        'ɡ': 'G',
        'm': 'M', 'n': 'N', 'ŋ': 'NG', 'f': 'F', 'v': 'V', 'θ': 'TH',
        'ð': 'DH', 's': 'S', 'z': 'Z', 'ʃ': 'SH', 'ʒ': 'ZH', 'h': 'HH',
        'ɹ': 'R', 'l': 'L', 'w': 'W', 'j': 'Y',
        'tʃ': 'CH', 't͡ʃ': 'CH',
        'dʒ': 'JH', 'd͡ʒ': 'JH',
        'i': 'IY', 'ɪ': 'IH', 'ɛ': 'EH', 'æ': 'AE', 'ɑ': 'AA', 'ɔ': 'AO',
        'ʊ': 'UH', 'u': 'UW', 'ʌ': 'AH', 'ə': 'AH', 'aɪ': 'AY', 'aʊ': 'AW',
        'ɔɪ': 'OY', 'eɪ': 'EY', 'oʊ': 'OW', 'ɝ': 'ER', 'ɚ': 'ER',
        'ː': '', 'r': 'R', 'x': 'K', 'ʔ': ''
    }
    try:
        for sentence in sentences(text, lang='en-us'):
            for word in sentence:
                ipa_list = [p.replace('ˈ', '').replace('ˌ', '')
                            for p in word.phonemes]
                result = [mapping.get(p, p) for p in ipa_list]
                # filter out empty strings from stripped symbols
                return [r for r in result if r]
    except:
        return []
    return []
"

#' Set up the phonetics Python environment.
#'
#' Creates a virtual environment and installs g2p_en, gruut, nltk, and torch,
#' then downloads required NLTK data. Only needs to be run once.
#'
#' @param envname Name of the virtual environment. Default is "r-phonetics".
#'
#' @return Invisible TRUE on success.
#' @export
phonetics_setup <- function(envname = .phonetics_envname)
{
  message("Creating/updating virtual environment '", envname, "'...")
  if (!reticulate::virtualenv_exists(envname)) {
    reticulate::virtualenv_create(envname)
  }
  message("Installing Python packages (g2p-en, gruut, nltk, torch)...")
  reticulate::py_install(
    c("g2p-en", "nltk", "torch", "gruut[en]"),
    envname = envname,
    pip     = TRUE
  )
  reticulate::use_virtualenv(envname, required = TRUE)
  message("Downloading NLTK data...")
  reticulate::py_run_string("
import nltk
for pkg in ['averaged_perceptron_tagger_eng', 'cmudict']:
    try:
        nltk.data.find(pkg)
    except LookupError:
        nltk.download(pkg)
")
  message("Testing gruut...")
  reticulate::py_run_string(.gruut_py_code)
  reticulate::py_run_string("
from gruut import sentences
test = 'testing'
for sentence in sentences(test, lang='en-us'):
    for word in sentence:
        print('gruut test:', get_arpabet_gruut(test))
")
  message("Phonetics setup complete: g2p-en and gruut are ready.")
  invisible(TRUE)
}

#' Load and initialise the phonetics models for use in the current session.
#'
#' Called automatically by phonetics-dependent functions. Checks if the
#' environment is ready and initialises both g2p and gruut if not already
#' cached. Runs phonetics_setup() automatically if the environment is
#' missing or incomplete.
#'
#' @param envname Name of the virtual environment. Default is "r-phonetics".
#'
#' @return Invisible TRUE on success.
#' @export
load_phonetics <- function(envname = .phonetics_envname)
{

  # cache hit — models already initialised this session
  if (isTRUE(.phonetics_cache$ready) &&
      !is.null(.phonetics_cache$g2p)) {
    return(invisible(TRUE))
  }

  # activate environment if it exists but Python not yet initialised
  if (reticulate::virtualenv_exists(envname) &&
      !reticulate::py_available(initialize = FALSE)) {
    reticulate::use_virtualenv(envname, required = TRUE)
  }

  # run setup if environment is missing or incomplete
  if (!.g2p_ready(envname) || !.gruut_ready(envname)) {
    suppressMessages(phonetics_setup(envname))
    reticulate::use_virtualenv(envname, required = TRUE)
    if (!.g2p_ready(envname)) {
      stop(
        "Phonetics setup ran but the environment is still not ready. ",
        "Try running `phonetics_setup()` manually for more details.",
        call. = FALSE
      )
    }
  }

  # initialise g2p model and cache it
  if (is.null(.phonetics_cache$g2p)) {
    g2p_en               <- reticulate::import("g2p_en")
    .phonetics_cache$g2p <- g2p_en$G2p()
  }

  # initialise gruut Python function
  if (!reticulate::py_has_attr(reticulate::import_main(),
                               "get_arpabet_gruut")) {
    reticulate::py_run_string(.gruut_py_code)
  }

  .phonetics_cache$gruut <- .gruut_ready(envname)
  .phonetics_cache$ready <- TRUE
  invisible(TRUE)
}

#' Get ARPAbet phonemes for a word using gruut.
#'
#' Internal function. Uses gruut's rule-based IPA transcription converted
#' to ARPAbet. More reliable than g2p for non-dictionary words as it
#' preserves consonant clusters rather than normalizing them.
#'
#' @param word A word or string as a character scalar.
#' @param envname Name of the virtual environment.
#'
#' @return Character vector of ARPAbet phonemes, or character(0) on failure.
#' @noRd
get_phonemes_gruut <- function(word, envname = .phonetics_envname)
{
  load_phonetics(envname)
  tryCatch(
    reticulate::py$get_arpabet_gruut(word),
    error = function(e) character(0)
  )
}

#' Get ARPAbet phonemes for a word using g2p.
#'
#' Internal function. Uses the g2p_en neural model for grapheme-to-phoneme
#' conversion. May normalize illegal consonant clusters by inserting vowels.
#'
#' @param word A word or string as a character scalar.
#' @param envname Name of the virtual environment.
#'
#' @return Character vector of ARPAbet phonemes, or character(0) on failure.
#' @noRd
get_phonemes_g2p <- function(word, envname = .phonetics_envname)
{
  load_phonetics(envname)
  tryCatch({
    raw <- .phonetics_cache$g2p(word)
    # strip stress markers and filter spaces
    raw <- unlist(raw)
    raw <- gsub("[0-9]", "", raw)
    raw[nchar(raw) > 0]
  }, error = function(e) character(0))
}
