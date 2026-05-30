
#' Build a summary table from anagram CSV files
#'
#' Reads all anagram CSV files in a directory and builds a summary table
#' with one row per source word, containing aggregate statistics.
#'
#' @param output_dir Directory containing anagram CSV files. Default
#'   "anagram_data".
#'
#' @return A data frame with one row per source word.
#' @export
build_word_summary <- function(output_dir = "anagram_data")
{
  csv_files <- list.files(output_dir, pattern = "\\.csv$",
                          full.names = TRUE, recursive = TRUE)
  n_total <- length(csv_files)
  message("Summarising ", n_total, " files...")
  results <- vector("list", n_total)

  for (i in seq_along(csv_files)) {
    df <- tryCatch(
      read.csv(csv_files[i], stringsAsFactors = FALSE),
      error   = function(e) { warning("Skipping ",
                                      csv_files[i], ": ", e$message); NULL },
      warning = function(w) { read.csv(csv_files[i], stringsAsFactors = FALSE) }
    )

    if (is.null(df) || nrow(df) == 0) next

    results[[i]] <- data.frame(
      Solution             = df$Solution[1],
      WordLength           = nchar(df$Solution[1]),  # Compute from Solution
      # solution ranks
      sSBFrank = {
        idx <- which(df$Anagram == df$Solution[1])
        if (length(idx) > 0) df$SBFrank[idx] else NA_integer_
      },
      sMLBFrank = {
        idx <- which(df$Anagram == df$Solution[1])
        if (length(idx) > 0) df$MLBFrank[idx] else NA_integer_
      },
      # classification counts
      Nwords               = sum(df$aClassification == "word",
                                 na.rm = TRUE),
      Npseudowords         = sum(df$aClassification == "pseudoword",
                                 na.rm = TRUE),
      Nnonwords            = sum(df$aClassification == "nonword",
                                 na.rm = TRUE),
      # SBF
      aSBFmean             = mean(df$aSBF, na.rm = TRUE),
      aSBFrange            = max(df$aSBF,  na.rm = TRUE) - min(df$aSBF,
                                                               na.rm = TRUE),
      aSBFmin              = min(df$aSBF,  na.rm = TRUE),
      aSBFmax              = max(df$aSBF,  na.rm = TRUE),
      # MLBF
      aMLBFmean            = mean(df$aMLBF, na.rm = TRUE),
      aMLBFrange           = max(df$aMLBF,  na.rm = TRUE) - min(df$aMLBF,
                                                                na.rm = TRUE),
      aMLBFmin             = min(df$aMLBF,  na.rm = TRUE),
      aMLBFmax             = max(df$aMLBF,  na.rm = TRUE),
      # OLD20
      aOLD20mean           = mean(df$aOLD20, na.rm = TRUE),
      aOLD20range          = max(df$aOLD20,  na.rm = TRUE) - min(df$aOLD20,
                                                                 na.rm = TRUE),
      aOLD20min            = min(df$aOLD20,  na.rm = TRUE),
      aOLD20max            = max(df$aOLD20,  na.rm = TRUE),
      # articulability
      aArticulabilitymean        = mean(df$aArticulability, na.rm = TRUE),
      aArticulabilityrange       = max(df$aArticulability,  na.rm = TRUE) -
        min(df$aArticulability, na.rm = TRUE),
      aArticulabilitymin         = min(df$aArticulability,  na.rm = TRUE),
      aArticulabilitymax         = max(df$aArticulability,  na.rm = TRUE),
      # moves
      Movesmin             = min(df$Moves, na.rm = TRUE),
      Movesmax             = max(df$Moves, na.rm = TRUE),
      # intact letters
      Nintactmin           = min(df$Nintact, na.rm = TRUE),
      Nintactmax           = max(df$Nintact, na.rm = TRUE),
      # preserved bigrams
      NpreservedBGmin      = min(df$NpreservedBG, na.rm = TRUE),
      NpreservedBGmax      = max(df$NpreservedBG, na.rm = TRUE),
      # morphemes
      aNmorphemesmin       = min(df$aNmorphemes, na.rm = TRUE),
      aNmorphemesmax       = max(df$aNmorphemes, na.rm = TRUE),
      stringsAsFactors = FALSE
    )

    if (i %% 100 == 0 || i == n_total) {
      message(sprintf("[%d / %d] %s", i, n_total,
                      tools::file_path_sans_ext(basename(csv_files[i]))))
    }
  }

  dplyr::bind_rows(results)
}


#' Compute psycholinguistic indices for any string or set of strings
#'
#' Computes a comprehensive set of psycholinguistic measures for any string
#' or character vector.
#'
#' @param strings A character vector of strings to score.
#' @param sig_index Signature index for dictionary lookup.
#'   Defaults to grams::sig_index.
#' @param classify Logical. If TRUE (default), includes classification label.
#' @param full Logical. If \code{TRUE}, returns additional diagnostic columns
#'   alongside the standard indices. Default \code{FALSE}.
#'
#' @return A data frame with one row per string and columns for each measure.
#' @export
compute_string_indices <- function(strings,
                                   sig_index = grams::sig_index,
                                   classify  = TRUE,
                                   full = FALSE)
{

  strings <- as.character(strings)

  # precompute expensive operations
  syllable_data <- lapply(strings, function(s) {
    tryCatch(
      syllables(s, output = "full"),
      error = function(e) NULL
    )
  })
  morpheme_list <- lapply(strings, function(p) find_morphemes(p, output = "list"))

  # build data frame
  df <- data.frame(
    string          = strings,

    # orthographic frequency
    SBF             = sapply(strings, sbf),
    MLBF            = sapply(strings, mlbf),

    # orthographic neighbourhood
    OLD20           = sapply(strings, old_n),
    ED1             = sapply(strings, ed1),

    # gtzero
    GTzero          = sapply(strings, function(s) gtzero(s, output = "prop")),

    # phonological
    Articulability        = mapply(function(s, sd) articulability(s, syllable_data = sd),
                             strings, syllable_data),

    Nsyllables = sapply(syllable_data, function(x) {
      if (is.null(x)) return(NA_integer_)
      # use syllables_g2p if available, fall back to max syll
      if (!is.null(x$syllables_g2p) && !is.na(x$syllables_g2p[1]))
        return(x$syllables_g2p[1])
      # check if syll column has any non-NA values before taking max
      if (all(is.na(x$syll))) return(NA_integer_)
      max(x$syll, na.rm = TRUE)
    }),

    SyllableSource = sapply(syllable_data, function(x)
      if (is.null(x)) NA_character_ else x$source[1]),

    # string properties
    Nvowels          = sapply(strings, n_vowels),
    Nconsonants      = sapply(strings, n_consonants),
    Vratio           = sapply(strings, vowel_ratio),
    FirstLetter      = sapply(strings, first_letter),
    InfreqLetter     = sapply(strings, infreq_letter),
    NuniqueLetters   = sapply(strings, n_unique_letters),
    ULratio          = sapply(strings, unique_letter_ratio),
    Morphemes        = sapply(morpheme_list, function(x) paste(x,
                                                               collapse = ",")),
    Nmorphemes       = sapply(morpheme_list, length),

    stringsAsFactors = FALSE,
    row.names        = NULL
  )

  # classification
  if (classify) {
    df$Classification <- sapply(strings, function(s)
      classify_string(s, sig_index = sig_index))
  }

  # non-default
  if (full) {
    df$SBFnp  <- sapply(strings, function(s) sbf(s, positional = FALSE))
    df$SBFt   <- sapply(strings, function(s)
      sbf(s, cache = grams::subtlex_uk_bigram_token_cache))
    df$MLBFnp <- sapply(strings, function(s) mlbf(s, positional = FALSE))
    df$MLBFt  <- sapply(strings, function(s)
      mlbf(s, cache = grams::subtlex_uk_bigram_token_cache))
    df$SLF    <- sapply(strings, slf)
    df$STF    <- sapply(strings, stf)
    df$MLLF    <- sapply(strings, mllf)
    df$MLTF    <- sapply(strings, mltf)
  }

  df
}

#' Compute anagram indices relative to a solution word
#'
#' Computes all psycholinguistic measures for a set of anagram permutations,
#' including measures relative to the source word such as letter moves,
#' intact letters, and preserved bigrams.
#'
#' @param anagram A character vector of anagram permutations to score.
#' @param solution The source word the permutations are derived from.
#' @param sig_index Signature index for dictionary lookup.
#'   Defaults to grams::sig_index.
#'
#' @return A data frame with one row per permutation and columns for each
#'   measure.
#' @export
compute_anagram_indices <- function(anagram,
                                    solution,
                                    sig_index = grams::sig_index)
{
  # handle both single strings and vectors
  if (length(anagram) == 1 && length(solution) > 1) {
    stop("When providing a single anagram, provide a single solution")
  }
  if (length(solution) == 1 && length(anagram) > 1) {
    # recycle solution for all anagrams (common case: many anagrams of one word)
    solution <- rep(solution, length(anagram))
  }

  # standalone indices, call compute string indices
  df <- compute_string_indices(anagram, sig_index = sig_index)

  # rename string column to anagram
  names(df)[names(df) == "string"] <- "Anagram"

  # add leading "a" prefix (denoting anagram) to columns that need it
  rename_map <- c(
    "SBF" = "aSBF",
    "MLBF" = "aMLBF",
    "OLD20" = "aOLD20",
    "ED1" = "aED1",
    "Articulability" = "aArticulability",
    "Nsyllables" = "aNsyllables",
    "SyllableSource" = "aSyllableSource",
    "FirstLetter" = "aFirstLetter",
    "Morphemes" = "aMorphemes",
    "Nmorphemes" = "aNmorphemes",
    "Classification" = "aClassification"
  )

  for (old_name in names(rename_map)) {
    names(df)[names(df) == old_name] <- rename_map[[old_name]]
  }

  # precompute expensive operations
  intact_list   <- lapply(seq_along(anagram), function(i)
    intact_letters(solution[i], anagram[i]))
  bigram_list   <- lapply(seq_along(anagram), function(i)
    preserved_bg(solution[i], anagram[i]))

  # add source-relative columns
  df$Solution          <- solution
  df$SBFrank           <- rank(-df$aSBF, ties.method = "min")
  df$MLBFrank          <- rank(-df$aMLBF, ties.method = "min")
  df$Moves             <- sapply(seq_along(anagram), function(i)
    count_moves(solution[i], anagram[i]))
  df$Intact            <- sapply(intact_list,
                                 function(x)
                                   if (all(is.na(x))) NA_character_
                                 else paste(x, collapse = ","))
  df$Nintact           <- sapply(intact_list,
                                 function(x)
                                   if (all(is.na(x))) 0L else length(x))
  df$PreservedBG       <- sapply(bigram_list,
                                 function(x)
                                   if (all(is.na(x))) NA_character_
                                 else paste(x, collapse = ","))
  df$NpreservedBG      <- sapply(bigram_list,
                                 function(x)
                                   if (all(is.na(x))) 0L else length(x))
  df$SameFirstLetter   <- sapply(seq_along(anagram), function(i)
    substr(anagram[i], 1, 1) == substr(solution[i], 1, 1))

  # reorder columns
  df <- df |>
    dplyr::relocate(Solution, Anagram)

  df
}

#' Generate and save anagram candidates with psycholinguistic indices.
#'
#' For a given word, generates up to \code{total} permutations using
#' \code{generate_anagram_candidates()}, computes psycholinguistic indices
#' for each via \code{compute_anagram_indices()}, and saves the result as a CSV
#' file. Files are organized into subfolders by word length. Existing files
#' are skipped by default.
#'
#' @param word A word in string format.
#' @param output_dir Path to the output directory. Default is "anagram_data".
#' @param total Target number of permutations to generate. Default 100.
#' @param top_n Number of top and bottom SBF permutations to include. Default 5.
#' @param n_iter Number of iterations per simulated annealing restart. Default 5000.
#' @param n_restarts Number of simulated annealing restarts. Default 200.
#' @param overwrite Logical. If TRUE, regenerates and overwrites existing files.
#'   Default FALSE.
#' @param sig_index Signature index for dictionary lookup. Defaults to the
#'   package internal index.
#'
#' @return Invisibly returns the path to the saved CSV file.
#' @export
generate_word_csv <- function(word,
                              output_dir = "anagram_data",
                              total      = 100,
                              top_n      = 5,
                              n_iter     = 5000,
                              n_restarts = 200,
                              overwrite  = FALSE,
                              sig_index  = grams::sig_index)
{
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # add underscore prefix to filename
  out_path <- file.path(output_dir, paste0("_", tolower(word), ".csv"))

  if (file.exists(out_path) && !overwrite) {
    message("Already exists, skipping: ", out_path)
    return(invisible(out_path))
  }

  message("Generating candidates for: ", word)
  permutations <- generate_anagram_candidates(word,
                                              total      = total,
                                              top_n      = top_n,
                                              n_iter     = n_iter,
                                              n_restarts = n_restarts,
                                              sig_index  = sig_index)

  # wrap computation to catch warnings and errors
  df <- tryCatch(
    withCallingHandlers(
      compute_anagram_indices(anagram   = permutations,
                              solution  = tolower(word),
                              sig_index = sig_index),
      warning = function(w) {
        if (grepl("-Inf|max", w$message, ignore.case = TRUE)) {
          message("WARNING in word '", word, "': ", w$message)
        }
      }
    ),
    error = function(e) {
      message("ERROR processing word '", word, "': ", e$message)
      NULL
    }
  )

  if (is.null(df)) return(invisible(NULL))

  df <- df[order(df$SBFrank), ]

  # write to temporary file first (atomic write)
  temp_path <- paste0(out_path, ".tmp")
  write.csv(df, temp_path, row.names = FALSE, quote = TRUE)

  # only rename to final path if write succeeded
  file.rename(temp_path, out_path)

  message("Saved: ", out_path, " (", nrow(df), " rows, ", ncol(df), " columns)")
  invisible(out_path)
}

#' Add a new index column to existing anagram CSV files.
#'
#' Applies a function to the permutation column of each CSV file and saves
#' the result as a new column. Files that already contain the column are
#' skipped by default, making it safe to run incrementally as new indices
#' are developed. Searches recursively through subdirectories.
#'
#' @param col_name Name of the new column to add.
#' @param index_fn A function that takes a single word string and returns a
#'   single value. The return type should match \code{col_type}.
#' @param output_dir Path to the directory containing CSV files. Default is
#'   "anagram_data".
#' @param words Optional character vector of source words. If provided, only
#'   the CSV files for those words are updated. If NULL, all CSV files in
#'   \code{output_dir} are updated.
#' @param overwrite Logical. If TRUE, recomputes and overwrites the column
#'   even if it already exists. Default FALSE.
#' @param col_type The expected return type of \code{index_fn}, specified as
#'   a zero-length vector (e.g. \code{numeric(1)}, \code{character(1)},
#'   \code{logical(1)}). Default \code{numeric(1)}.
#' @param needs_solution Logical. If \code{TRUE}, the index function receives
#'   both the permutation and the source word as arguments. If \code{FALSE},
#'   only the permutation is passed. Default \code{FALSE}.
#'
#' @return Invisibly returns NULL. Progress and summary messages are printed
#'   to the console.
#' @export
add_index_to_csv <- function(col_name,
                             index_fn,
                             output_dir = "anagram_data",
                             words      = NULL,
                             overwrite  = FALSE,
                             col_type   = numeric(1),
                             needs_solution = FALSE)
{
  if (is.null(words)) {
    csv_files <- list.files(output_dir, pattern = "\\.csv$",
                            full.names = TRUE, recursive = TRUE)
  } else {
    csv_files <- file.path(output_dir, paste0(tolower(words), ".csv"))
    missing   <- csv_files[!file.exists(csv_files)]
    if (length(missing) > 0)
      warning("Files not found, skipping: ", paste(missing, collapse = ", "))
    csv_files <- csv_files[file.exists(csv_files)]
  }

  if (length(csv_files) == 0) {
    message("No CSV files found in: ", output_dir)
    return(invisible(NULL))
  }

  updated <- 0L
  skipped <- 0L
  n_total <- length(csv_files)

  for (i in seq_along(csv_files)) {
    path   <- csv_files[i]
    folder <- basename(dirname(path))
    word   <- tools::file_path_sans_ext(basename(path))

    if (i %% 100 == 0 || i == 1 || i == n_total) {
      message(sprintf("[%d / %d] %s / %s", i, n_total, folder, word))
    }

    df <- read.csv(path, stringsAsFactors = FALSE)

    if (col_name %in% names(df) && !overwrite) {
      skipped <- skipped + 1L
      next
    }

    # pass both solution and anagram if needed
    if (needs_solution) {
      df[[col_name]] <- vapply(seq_len(nrow(df)), function(j) {
        index_fn(df$Solution[j], df$Anagram[j])
      }, col_type)
    } else {
      df[[col_name]] <- vapply(df$Anagram, index_fn, col_type)
    }

    write.csv(df, path, row.names = FALSE)
    updated <- updated + 1L
  }

  message("Done. Updated: ", updated, "  Skipped (already had column): ",
          skipped)
  invisible(NULL)
}

#' Rename columns in existing anagram CSV files
#'
#' Renames columns across all CSV files using exact column name matching.
#' Only columns that exactly match the old names are renamed.
#'
#' @param renames A named character vector where names are old column names
#'   and values are new column names (e.g., c("Sonority" = "Articulability")).
#' @param output_dir Path to the directory containing CSV files.
#' @param words Optional character vector of source words to update. If NULL,
#'   updates all CSV files.
#'
#' @return Invisibly returns NULL. Progress printed to console.
#' @export
rename_csv_columns <- function(renames,
                               output_dir = "anagram_data",
                               words = NULL) {
  # get file list
  if (is.null(words)) {
    csv_files <- list.files(output_dir, pattern = "\\.csv$",
                            full.names = TRUE, recursive = TRUE)
  } else {
    csv_files <- file.path(output_dir, paste0(tolower(words), ".csv"))
    csv_files <- csv_files[file.exists(csv_files)]
  }

  if (length(csv_files) == 0) {
    message("No CSV files found in: ", output_dir)
    return(invisible(NULL))
  }

  n_total <- length(csv_files)
  updated <- 0L

  for (i in seq_along(csv_files)) {
    path <- csv_files[i]

    if (i %% 100 == 0 || i == 1 || i == n_total) {
      folder <- basename(dirname(path))
      word   <- tools::file_path_sans_ext(basename(path))
      message(sprintf("[%d / %d] %s / %s", i, n_total, folder, word))
    }

    df <- read.csv(path, stringsAsFactors = FALSE)

    # rename only exact matches
    for (j in seq_along(renames)) {
      old_name <- names(renames)[j]
      new_name <- renames[j]

      if (old_name %in% names(df)) {
        names(df)[names(df) == old_name] <- new_name
      }
    }

    write.csv(df, path, row.names = FALSE)
    updated <- updated + 1L
  }

  message("Done. Renamed columns in ", updated, " files.")
  invisible(NULL)
}

#' Get the local path to the GRAMS database
#'
#' Returns the path where the GRAMS database is stored in the user's app data
#' directory, creating the directory if it does not exist. This is the default
#' location used by \code{\link{download_grams_database}} and
#' \code{\link{load_grams_database}}.
#'
#' @return A character string giving the full path to \code{GRAMS_database.rds}.
#'
#' @seealso \code{\link{download_grams_database}}, \code{\link{load_grams_database}}
#' @export
grams_database_path <- function() {
  path <- rappdirs::user_data_dir("grams")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  file.path(path, "GRAMS_database.rds")
}

#' Download the GRAMS database
#'
#' Downloads the GRAMS database from OSF and saves it to the user's app data
#' directory. The database is required for stimulus selection functions. If the
#' database already exists locally it will not be downloaded again unless
#' \code{overwrite = TRUE}.
#'
#' @param overwrite Logical. If \code{TRUE}, download and overwrite any existing
#'   local copy. Defaults to \code{FALSE}.
#'
#' @return Invisibly returns the path to the downloaded file.
#'
#' @seealso \code{\link{load_grams_database}}, \code{\link{grams_database_path}}
#'
#' @examples
#' \dontrun{
#' download_grams_database()
#' }
#'
#' @export
download_grams_database <- function(overwrite = FALSE) {
  dest <- grams_database_path()
  if (file.exists(dest) && !overwrite) {
    message("GRAMS database already exists at:\n  ", dest,
            "\n  Use overwrite = TRUE to re-download.")
    return(invisible(dest))
  }
  url <- "https://osf.io/s5dnp/download"
  message("Downloading GRAMS database (~82MB) to:\n  ", dest)
  download.file(url, dest, mode = "wb")
  message("Download complete.")
  invisible(dest)
}

#' Load the GRAMS database
#'
#' Loads the GRAMS database from the user's app data directory. If the database
#' has not been downloaded yet, an informative error is raised with instructions.
#'
#' @return A data frame containing the GRAMS database.
#'
#' @seealso \code{\link{download_grams_database}}, \code{\link{grams_database_path}}
#'
#' @examples
#' \dontrun{
#' GRAMS_database <- load_grams_database()
#' }
#'
#' @export
load_grams_database <- function() {
  path <- grams_database_path()
  if (!file.exists(path)) {
    stop(
      "GRAMS database not found. Download it with:\n",
      "  download_grams_database()\n\n",
      "Or load from a custom path:\n",
      "  GRAMS_database <- readRDS('path/to/GRAMS_database.rds')"
    )
  }
  obj <- readRDS(path)
  if (is.list(obj) && "database" %in% names(obj)) {
    message("Loading GRAMS database v", obj$version,
            " (", format(obj$n_words, big.mark = ","), " words)")
    obj$database
  } else {
    message("Loading GRAMS database from:\n  ", path)
    obj
  }
}
