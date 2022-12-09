# https://adventofcode.com/2022/day/8

parse_tree_matrix <- function(text_lines) {
  text_lines <- trimws(text_lines)
  chars <- strsplit(text_lines, "")
  tmat <- do.call(rbind, chars)
  storage.mode(tmat) <- "integer"
  tmat
}


is_visible <- function(heights) {
  nr <- nrow(heights)
  nc <- ncol(heights)
  visible_left <- matrix(TRUE, nrow = nr, ncol = nc)
  visible_right <- visible_left
  visible_top <- visible_left
  visible_bottom <- visible_left
  for (ii in 2:nc) {
    left_unblocked <- heights[, ii:nc] > heights[, ii - 1]
    visible_left[, ii:nc] <- visible_left[, ii:nc] & left_unblocked
    right_ii <- nc - ii + 1
    right_unblocked <- heights[, 1:right_ii] > heights[, right_ii + 1]
    visible_right[, 1:right_ii] <- visible_right[, 1:right_ii] & right_unblocked
  }
  for (jj in 2:nr) {
    # Need to cast each row across the other rows of the matrix
    rows_compared <- nr - jj + 1
    top_row_cast <- matrix(
      heights[jj - 1, ],
      nrow = rows_compared, ncol = nc, byrow = TRUE
    )
    top_unblocked <- heights[jj:nr, ] > top_row_cast
    visible_top[jj:nr, ] <- visible_top[jj:nr, ] & top_unblocked
    bottom_jj <- nr - jj + 1
    bottom_row_cast <- matrix(
      heights[bottom_jj + 1, ],
      nrow = rows_compared, ncol = nc, byrow = TRUE
    )
    bottom_unblocked <- heights[1:bottom_jj, ] > bottom_row_cast
    visible_bottom[1:bottom_jj, ] <- visible_bottom[1:bottom_jj, ] & bottom_unblocked
  }
  visible_left | visible_right | visible_bottom | visible_top
}


score_scenery <- function(heights) {
  nr <- nrow(heights)
  nc <- ncol(heights)
  distance_left <- matrix(rep(1:nc - 1, each = nr), nrow = nr, ncol = nc)
  distance_right <- distance_left[, nc:1]
  distance_top <- matrix(1:nr - 1, nrow = nr, ncol = nc)
  distance_bottom <- distance_top[nr:1, ]
  for (ii in seq(nc - 1)) {
    left_column <- heights[, ii]
    right_portion <- heights[, (ii + 1):nc, drop = FALSE]
    left_blocked <- left_column >= right_portion
    left_blocked_ind <- which(left_blocked, arr.ind = TRUE)
    if (!is.matrix(left_blocked_ind)) {
      left_blocked_ind <- cbind(left_blocked_ind, 1)
    }
    left_blocked_ind[, 2] <- left_blocked_ind[, 2] + ii
    distance_left[left_blocked_ind] <- col(right_portion)[left_blocked]

    right_column <- heights[, nc - ii + 1]
    left_portion <- heights[, 1:(nc - ii), drop = FALSE]
    right_blocked <- right_column >= left_portion
    right_blocked_ind <- which(right_blocked, arr.ind = TRUE)
    if (!is.matrix(right_blocked_ind)) {
      right_blocked_ind <- cbind(right_blocked_ind, 1)
    }
    right_col_ind <- col(left_portion)
    right_col_ind <- right_col_ind[, ncol(right_col_ind):1]
    distance_right[right_blocked_ind] <- right_col_ind[right_blocked]
  }
  for (jj in seq(nr - 1)) {
    top_column <- matrix(
      heights[jj, ], nrow = nr - jj, ncol = nc, byrow = TRUE
    )
    bottom_portion <- heights[(jj + 1):nr, , drop = FALSE]
    top_blocked <- top_column >= bottom_portion
    top_blocked_ind <- which(top_blocked, arr.ind = TRUE)
    if (!is.matrix(top_blocked_ind)) {
      top_blocked_ind <- cbind(1, top_blocked_ind)
    }
    top_blocked_ind[, 1] <- top_blocked_ind[, 1] + jj
    distance_top[top_blocked_ind] <- row(bottom_portion)[top_blocked]

    bottom_column <- matrix(
      heights[nr - jj + 1, ], nrow = nr - jj, ncol = nc, byrow = TRUE
    )
    top_portion <- heights[1:(nr - jj), , drop = FALSE]
    bottom_blocked <- bottom_column >= top_portion
    bottom_blocked_ind <- which(bottom_blocked, arr.ind = TRUE)
    if (!is.matrix(bottom_blocked_ind)) {
      bottom_blocked_ind <- cbind(1, bottom_blocked_ind)
    }
    bottom_row_ind <- row(top_portion)
    bottom_row_ind <- bottom_row_ind[nrow(bottom_row_ind):1, ]
    distance_bottom[bottom_blocked_ind] <- bottom_row_ind[bottom_blocked]
  }
  distance_left * distance_right * distance_top * distance_bottom
}


# Testing with example data
example_input <- c(
  "30373",
  "25512",
  "65332",
  "33549",
  "35390"
)
example_heights <- parse_tree_matrix(example_input)
example_visible <- is_visible(example_heights)
stopifnot(sum(example_visible) == 21)
example_scenery <- score_scenery(example_heights)
stopifnot(all(example_scenery[1, ] == 0))
stopifnot(all(example_scenery[5, ] == 0))
stopifnot(all(example_scenery[, 1] == 0))
stopifnot(all(example_scenery[, 5] == 0))
stopifnot(example_scenery[2, 3] == 4)
stopifnot(example_scenery[4, 3] == 8)

# Part 1
input <- readLines("day08.txt")
tree_heights <- parse_tree_matrix(input)
visibility <- is_visible(tree_heights)
print(sum(visibility))

# Part 2
scenic_scores <- score_scenery(tree_heights)
print(max(scenic_scores))
