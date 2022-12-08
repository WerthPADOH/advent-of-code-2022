# https://adventofcode.com/2022/day/6

characters_to_start_regex <- function(stream) {
  # R isn't known for handling streams, so let's just regex it
  # Look for a string of four characters were the first one doesn't show up in
  # the next three, the second in the next two, and the third in the fourth
  frankensteins_regex <- paste0(
    "([a-z])(?!\\1[a-z]{2}|[a-z]\\1[a-z]|[a-z]{2}\\1)",
    "([a-z])(?!\\2[a-z]|[a-z]\\2)",
    "([a-z])(?!\\3)",
    "[a-z]"
  )
  loc <- regexpr(frankensteins_regex, stream, perl = TRUE)
  loc <- as.integer(loc)
  # Finds the first character's position, but we need the last one
  loc + 3
}


characters_to_start_connection <- function(stream, width) {
  # Let's do a stream in R for kicks and giggles
  con <- textConnection(strsplit(stream, "")[[1]])
  on.exit(close(con), add = TRUE)
  quartet <- c("", scan(con, "", width - 1, quiet = TRUE))
  ii <- 0
  index <- as.integer(width - 1)
  while (TRUE) {
    next_char <- scan(con, "", 1, quiet = TRUE)
    if (length(next_char) == 0) {
      stop("No start-of-packet marker found")
    }
    index <- index + 1L
    quartet[[ii + 1]] <- next_char
    if (!anyDuplicated(quartet)) {
      return(index)
    }
    ii <- (ii + 1L) %% width
  }
}


# Testing on example data
examples <- data.frame(
  stream = c(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb", "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg", "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ),
  packet_marker = c(7, 5, 6, 10, 11),
  message_marker = c(19, 23, 23, 29, 26),
  stringsAsFactors = FALSE
)
example_res_regex <- characters_to_start_regex(examples[["stream"]])
example_res_connection <- vapply(
  examples[["stream"]],
  FUN = characters_to_start_connection,
  FUN.VALUE = integer(1),
  width = 4
)
stopifnot(all(example_res_regex == examples[["packet_marker"]]))
stopifnot(all(example_res_connection == examples[["packet_marker"]]))

# Part 1
input <- readLines("day06.txt")
print(characters_to_start_regex(input))

# Part 2
# I don't feel like writing the regex abomination needed for 14 characters
print(characters_to_start_connection(input, 14))
