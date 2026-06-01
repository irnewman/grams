
#' Find shortest number of moves to solution word.
#'
#' @param solution The solution word string format.
#' @param anagram Anagram of the solution word in string format.
#'
#' @return Numeric length of minimum moves to solve.
#' @export
count_moves <- function(solution, anagram)
{
  sol_letters <- strsplit(toupper(solution), "")[[1]]
  ana_letters <- strsplit(toupper(anagram),  "")[[1]]
  n           <- length(ana_letters)

  # compute order of anagram letters relative to solution letters
  ana_order <- integer(n)
  used      <- logical(n)
  for (i in seq_len(n)) {
    positions    <- which(sol_letters == ana_letters[i] & !used)
    ana_order[i] <- positions[1]
    used[positions[1]] <- TRUE
  }

  # generate possible orders of anagram letters if there are repeated letters
  duplicates <- unique(sol_letters[duplicated(sol_letters)]) # duplicate letters
  possible   <- list(ana_order)

  # if any duplicates, add to possible orders
  if (length(duplicates) > 0) {
    for (d in seq_along(duplicates)) {
      sol_indices <- which(sol_letters == duplicates[d])
      ana_indices <- which(ana_order %in% sol_indices)
      perms       <- arrangements::permutations(ana_indices,
                                                length(ana_indices))
      n_existing  <- length(possible)
      for (p in seq_len(n_existing)) {
        base <- possible[[p]]
        for (r in 2:nrow(perms)) {
          new_order               <- base
          new_order[perms[1, ]]   <- base[perms[r, ]]
          possible[[length(possible) + 1L]] <- new_order
        }
      }
    }
  }

  # find minimum moves via longest increasing sub-sequence
  moves <- integer(length(possible))
  for (i in seq_along(possible)) {
    order    <- possible[[i]]
    # sort
    tails <- c()
    for (x in order) {
      pos       <- findInterval(x - 1, tails) + 1L
      tails[pos] <- x
    }
    moves[i] <- length(order) - length(tails)
  }

  return(min(moves))
}
