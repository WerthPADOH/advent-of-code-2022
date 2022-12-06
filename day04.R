# https://adventofcode.com/2022/day/4
library(stringi)


parse_bounds <- function(text_lines) {
  assignments <- stri_split_fixed(text_lines, ",", simplify = TRUE)
  bounds <- cbind(
    stri_split_fixed(assignments[, 1], "-", simplify = TRUE),
    stri_split_fixed(assignments[, 2], "-", simplify = TRUE)
  )
  storage.mode(bounds) <- "integer"
  colnames(bounds) <- c("a_lower", "a_upper", "b_lower", "b_upper")
  # Easier to assume the "A" range has the lowest bound
  # If tied, "A" is the range with the lowest upper bound
  b_first <- (
    bounds[, "b_lower"] <= bounds[, "a_lower"] |
    (
      bounds[, "a_lower"] == bounds[, "b_lower"] &
        bounds[, "b_upper"] < bounds[, "a_lower"]
    )
  )
  bounds[b_first, ] <- bounds[b_first, c("b_lower", "b_upper", "a_lower", "a_upper")]
  bounds
}


has_subset <- function(bounds) {
  bounds[, "a_lower"] == bounds[, "b_lower"] |
    bounds[, "a_upper"] >= bounds[, "b_upper"]
}


any_overlap <- function(bounds) {
  bounds[, "a_lower"] == bounds[, "b_lower"] |
    bounds[, "a_upper"] >= bounds[, "b_lower"]
}


# Test with the example
example_lines <- c(
  "2-4,6-8",
  "2-3,4-5",
  "5-7,7-9",
  "2-8,3-7",
  "6-6,4-6",
  "2-6,4-8"
)
example_bounds <- parse_bounds(example_lines)

# Part 1
example_is_subset <- has_subset(example_bounds)
stopifnot(sum(example_is_subset) == 2)
input <- readLines("day04.txt")
elf_bounds <- parse_bounds(input)
print(sum(has_subset(elf_bounds)))

# Part 2
example_overlaps <- any_overlap(example_bounds)
stopifnot(sum(example_overlaps) == 4)
print(sum(any_overlap(elf_bounds)))
