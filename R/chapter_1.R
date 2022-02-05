to_chars <- function(str) {
  strsplit(str, '')
}

#' Check if a string has all unique characters, vectorized
#' @param str The string to check
#' @export
unique_characters <- function(strs) {
  n <- length(strs)
  chars <- to_chars(strs)
  if (length(strs) == 1L) {
    length(unique(chars[[1L]])) == nchar(strs)
  } else {
    vapply(sapply(chars, unique), length, integer(1L)) == nchar(strs)
  }
}

#' Check if a string is a permutation of the other, vectorized
#' @param str1 The first string to check against.
#' @param str2 The second string to check against.
#' @export
is_permutation <- function(str1, str2) {
  chars1 <- to_chars(str1)
  chars2 <- to_chars(str2)
  sapply(lapply(chars1, sort), paste0, collapse = '') ==
    sapply(lapply(chars2, sort), paste0, collapse = '')
}

#' Replace every space with '%20', vectorized
#'
#' This solution is not in spirit of the exercise, R doesn't give this level of
#' control and I don't believe it should.
#'
#' @param str The string to modify.
#' @return The URLified string.
#' @export
URLify <- function(str) {
  gsub(' ', '%20', str)
}

#' Check that a string is a permutation in linear time, vectorized
#' @param strs The potential permuted palindromes.
#' @return \code{TRUE} for permuted palindromes, \code{FALSE} otherwise.
#' @export
palindrome_permutation <- function(strs) {
  chars <- to_chars(strs)
  counts <- sapply(chars, \(x) tapply(x, x, length))
  counts <- sapply(counts, \(x) x %% 2L)
  sapply(counts, sum) <= 1L
}

#' Check whether the Levenshtein distance is <= 1L
#'
#' Based on https://codereview.stackexchange.com/a/178993/172577
#' @export
one_away <- function(left, right) {
  lengthLeft <- nchar(left)
  lengthRight <- nchar(right)
  difference <- abs(lengthLeft - lengthRight)
  if (difference > 1L) return(FALSE)

  # Ensure that the left string is the shorter one.
  if (lengthLeft > lengthRight) {
    leftChars <- to_chars(right)[[1L]]
    rightChars <- to_chars(left)[[1L]]
  } else {
    leftChars <- to_chars(left)[[1L]]
    rightChars <- to_chars(right)[[1L]]
  }

  leftIndex <- 1L
  rightIndex <- 1L
  # Loop over the strings up to the length of the shortest. If one string is
  # longer any difference between checked chars implies a difference bigger than
  # one. If the length is the equal, one difference in both strings can be
  # ignored.
  while (leftIndex <= length(leftChars)) {
    if (leftChars[leftIndex] != rightChars[rightIndex]) {
      difference <- difference + 1L
      if (difference > 1L) return(FALSE)
      if (lengthLeft == lengthRight) leftIndex <- leftIndex + 1L
    } else {
      leftIndex <- leftIndex + 1L
    }
    rightIndex <- rightIndex + 1L
  }
  TRUE
}
