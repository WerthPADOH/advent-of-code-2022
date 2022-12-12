# https://adventofcode.com/2022/day/10
library(data.table)


cpu_states <- function(input) {
  parts <- do.call(rbind, strsplit(input, " "))
  progress <- data.table(
    command = parts[, 1],
    value = ifelse(parts[, 1] == "addx", parts[, 2], "0"),
    cost = ifelse(parts[, 1] == "addx", 2, 1)
  )
  progress[
    , value := as.integer(value)
  ][
    , ":="(X = cumsum(c(1, value[-.N])), end_cycle = cumsum(cost))
  ][
    , start_cycle := 1 + c(0, end_cycle[-.N])
  ]
  progress
}


signal_strengths <- function(cpu) {
  key_cycles <- data.table(
    start_cycle = seq(20, max(cpu[["start_cycle"]]), by = 40)
  )
  out <- cpu[key_cycles, on = "start_cycle", roll = TRUE]
  out[, strength := start_cycle * X]
  out
}


render_screen <- function(cpu) {
  draw_cycles <- cpu[
    list(start_cycle = 1:240), on = "start_cycle", roll = TRUE
  ][
    , ":="(pixel_col = (start_cycle - 1) %% 40, pixel_row = rep(0:5, each = 40))
  ][
    , char := ifelse(abs(X - pixel_col) < 2, "#", ".")
  ]
  draw_cycles[
    order(pixel_col),
    list(drawn_row = paste0(char, collapse = "")),
    by = list(pixel_row)
  ][
    order(pixel_row),
    drawn_row
  ]
}


# Example test
example_input <- readLines("day10-example.txt")
example_cpu <- cpu_states(example_input)
example_strengths <- signal_strengths(example_cpu)
example_res <- sum(example_strengths[["strength"]])
stopifnot(example_res == 13140)
example_screen <- render_screen(example_cpu)
example_expected <- c(
  "##..##..##..##..##..##..##..##..##..##..",
  "###...###...###...###...###...###...###.",
  "####....####....####....####....####....",
  "#####.....#####.....#####.....#####.....",
  "######......######......######......####",
  "#######.......#######.......#######....."
)
stopifnot(identical(example_screen, example_expected))

# Part 1
input <- readLines("day10.txt")
comm_cpu <- cpu_states(input)
strengths <- signal_strengths(comm_cpu)
print(sum(strengths[["strength"]]))

# Part 2
cat(render_screen(comm_cpu), sep = "\n")
