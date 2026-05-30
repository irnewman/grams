
#' Find the top and bottom SBF permutations of a word.
#'
#' For short words (7 letters or fewer), exhaustively scores all permutations.
#' For longer words, uses simulated annealing to approximate the highest and
#' lowest scoring arrangements without exhaustive search. Returns both the
#' extreme scores and all arrangements explored during the search.
#'
#' @param word A word in string format.
#' @param top_n Number of top and bottom permutations to return. Default 5.
#' @param n_iter Number of iterations per simulated annealing restart. Default 5000.
#' @param n_restarts Number of simulated annealing restarts. Default 200.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{top}{Named numeric vector of the highest SBF scores.}
#'     \item{bottom}{Named numeric vector of the lowest SBF scores.}
#'     \item{seen}{Named numeric vector of all explored arrangements and
#'       their scores, sorted ascending.}
#'   }
#' @noRd
find_sbf_extremes <- function(word,
                              top_n      = 5,
                              n_iter     = 5000,
                              n_restarts = 200)
{
  chars <- strsplit(tolower(word), "")[[1]]
  n     <- length(chars)

  # brute force for short words
  if (n <= 7) {
    all_perms <- unique(apply(arrangements::permutations(chars, n), 1,
                              paste, collapse = ""))
    scores    <- sort(setNames(sapply(all_perms, sbf), all_perms))
    return(list(
      bottom = head(scores, top_n),
      top    = tail(scores, top_n),
      seen   = scores
    ))
  }

  # simulated annealing for longer words
  seen <- new.env(hash = TRUE, parent = emptyenv())

  sa_run <- function(maximize) {
    current_chars <- sample(chars)
    current_word  <- paste(current_chars, collapse = "")
    current_score <- sbf(current_word)
    assign(current_word, current_score, envir = seen)
    best_chars    <- current_chars
    best_score    <- current_score

    for (i in seq_len(n_iter)) {
      temp      <- 1 - (i / n_iter)
      candidate <- current_chars
      swap      <- sample(n, 2)
      candidate[swap] <- candidate[rev(swap)]
      cword     <- paste(candidate, collapse = "")

      if (exists(cword, envir = seen, inherits = FALSE)) {
        candidate_score <- get(cword, envir = seen)
      } else {
        candidate_score <- sbf(cword)
        assign(cword, candidate_score, envir = seen)
      }

      delta <- if (maximize) candidate_score - current_score
      else          current_score - candidate_score

      if (delta > 0 || runif(1) < exp(delta / (temp + 1e-6))) {
        current_chars <- candidate
        current_score <- candidate_score
      }

      if ((maximize && current_score > best_score) ||
          (!maximize && current_score < best_score)) {
        best_chars <- current_chars
        best_score <- current_score
      }
    }
  }

  for (i in seq_len(n_restarts)) sa_run(maximize = TRUE)
  for (i in seq_len(n_restarts)) sa_run(maximize = FALSE)

  message("Unique arrangements explored: ", length(ls(seen)))

  all_words  <- ls(seen)
  all_scores <- sort(setNames(
    sapply(all_words, function(w) get(w, envir = seen)),
    all_words
  ))

  list(
    bottom = head(all_scores, top_n),
    top    = tail(all_scores, top_n),
    seen   = all_scores
  )
}

#' Generate anagram candidates for a word.
#'
#' Produces up to \code{total} permutations of a word's letters, guaranteed
#' to include the word itself, any dictionary anagrams, and the highest and
#' lowest scoring arrangements by summed bigram frequency (SBF). The
#' remainder is filled with a diverse, evenly spaced sample from all
#' arrangements explored during the SBF search.
#'
#' For words of 7 letters or fewer, all unique permutations are returned
#' (up to \code{total}). For longer words, simulated annealing is used to
#' approximate the extremes without exhaustive search.
#'
#' @param word A word in string format.
#' @param total Target number of permutations to return. Default 100.
#' @param top_n Number of top and bottom SBF permutations to include. Default 5.
#' @param n_iter Number of iterations per simulated annealing restart. Default 5000.
#' @param n_restarts Number of simulated annealing restarts. Default 200.
#' @param min_dist Minimum positional mismatch required between selected
#'   permutations during diversity filtering. Default 2.
#' @param sig_index Signature index for dictionary lookup. Defaults to the
#'   package internal index.
#'
#' @return A character vector of up to \code{total} permutations.
#' @noRd
generate_anagram_candidates <- function(word,
                                        total      = 100,
                                        top_n      = 5,
                                        n_iter     = 5000,
                                        n_restarts = 200,
                                        min_dist   = 2,
                                        sig_index  = grams::sig_index)
{
  word_lower <- tolower(word)
  n_letters  <- nchar(word_lower)

  # dictionary anagrams
  dict_words <- solve_anagram(word_lower, sig_index, include_self = TRUE)

  # top/bottom SBF + full seen pool
  tb       <- find_sbf_extremes(word_lower,
                                top_n      = top_n,
                                n_iter     = n_iter,
                                n_restarts = n_restarts)
  tb_words <- unique(c(names(tb$top), names(tb$bottom)))

  # seed the selected set
  selected   <- unique(c(dict_words, tb_words))
  remaining  <- total - length(selected)
  all_scores <- tb$seen

  # short words: exhaustive pool, no diversity filter needed
  if (n_letters <= 7) {
    extras <- names(all_scores)[!names(all_scores) %in% selected]
    return(unique(c(selected, head(extras, remaining))))
  }

  # build candidate pool: everything explored, minus already selected
  if (remaining <= 0) return(selected)
  pool <- all_scores[!names(all_scores) %in% selected]
  if (length(pool) == 0) return(selected)

  # oversample evenly across the score range
  n_candidates <- min(remaining * 3L, length(pool))
  indices      <- unique(round(seq(1, length(pool), length.out = n_candidates)))
  candidates   <- names(pool)[indices]

  # greedy diversity filter by positional mismatch
  pos_mismatch <- function(a, b) {
    sum(strsplit(a, "")[[1]] != strsplit(b, "")[[1]])
  }

  diverse <- character(0)
  for (candidate in candidates) {
    if (length(diverse) >= remaining) break
    comparators <- c(selected, diverse)
    min_d <- min(vapply(comparators, pos_mismatch, 0L, b = candidate))
    if (min_d >= min_dist) diverse <- c(diverse, candidate)
  }

  unique(c(selected, diverse))
}
