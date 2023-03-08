# https://adventofcode.com/2022/day/22
library(stringi)
library(ggplot2)
library(data.table)


parse_input <- function(text_lines) {
  out <- list(map = NULL, commands = NULL)
  blank <- which(text_lines == "")
  map_lines <- text_lines[seq(blank - 1)]
  map_width <- max(stri_length(map_lines))
  map_lines <- stri_pad_right(map_lines, map_width, " ")
  out[["map"]] <- do.call(rbind, strsplit(map_lines, ""))
  command_line <- text_lines[blank + 1]
  commands <- strsplit(command_line, "(?<=[RL])|(?=[RL])", perl = TRUE)[[1]]
  is_paces <- stri_detect_regex(commands, "^\\d+$")
  commands <- as.list(commands)
  commands[is_paces] <- lapply(commands[is_paces], as.integer)
  out[["commands"]] <- commands
  out
}


plot_map <- function(map, path = NULL) {
  map_dt <- data.table(
    x = as.vector(col(map)),
    y = as.vector(nrow(map) - row(map) + 1),
    char = as.vector(map)
  )
  map_dt <- map_dt[char != " "]
  plotted <- ggplot(map_dt, aes(x = x, y = y, fill = char)) +
    geom_tile() +
    scale_fill_manual(values = c("." = "gray", "#" = "darkred")) +
    guides(fill = "none") +
    coord_equal() +
    theme_void()
  if (!is.null(path)) {
    path <- copy(path)
    path[, y := nrow(map) - y + 1]
    plotted <- plotted +
      geom_tile(data = path, fill = "steelblue") +
      geom_text(data = path, aes(label = char), color = "darkblue")
  }
  plotted
}


COMPASS <- c(E = 1, S = 2, W = 3, N = 4)
WALK_CHARS <- c(E = ">", S = "v", W = "<", N = "^")

# R has 1-based indices, so modulo math is weird
mod_base_1 <- function(x, modulus) {
  ((x - 1) %% modulus) + 1
}


walk_direction <- function(map_dt, position, distance) {
  direction <- names(COMPASS)[[position[["facing"]]]]
  pos_x <- position[["x"]]
  pos_y <- position[["y"]]
  tiles <- switch(direction,
    "E" = map_dt[y == pos_y],
    "S" = map_dt[x == pos_x],
    "W" = map_dt[y == pos_y][order(-x)],
    "N" = map_dt[x == pos_x][order(-y)]
  )
  pos_index <- tiles[position, on = c("x", "y"), which = TRUE]
  path_index <- pos_index + seq_len(distance)
  path_index <- mod_base_1(path_index, nrow(tiles))
  path <- tiles[path_index]
  wall_loc <- which(path[["char"]] == "#")
  if (length(wall_loc) > 0) {
    path <- path[seq_len(min(wall_loc) - 1)]
  }
  path_char <- WALK_CHARS[direction]
  path[, list(x, y, char = rep(path_char, .N))]
}


blaze_trail <- function(map, commands) {
  map_dt <- data.table(
    x = as.vector(col(map)),
    y = as.vector(row(map)),
    char = as.vector(map)
  )
  map_dt <- map_dt[char != " "]
  map_len <- length(map)
  walked <- data.table(
    x = rep(NA_integer_, map_len),
    y = rep(NA_integer_, map_len),
    char = factor(rep(NA, map_len), WALK_CHARS),
    facing = rep(NA_integer_, map_len)
  )
  start_pos <- map_dt[y == 1 & char != "#", list(x = min(x), y = 1L)]
  walked[
    1,
    ":="(
      x = start_pos[["x"]], y = start_pos[["y"]],
      char = ">", facing = COMPASS[["E"]]
    )
  ]
  position_ii <- 1L
  for (cmd in commands) {
    if (identical(cmd, "R")) {
      walked[position_ii, facing := mod_base_1(facing + 1, 4)]
    } else if (identical(cmd, "L")) {
      walked[position_ii, facing := mod_base_1(facing - 1, 4)]
    } else {
      path <- walk_direction(
        map_dt = map_dt,
        position = walked[position_ii],
        distance = cmd
      )
      steps_taken <- nrow(path)
      if (steps_taken > 0) {
        if (position_ii + steps_taken > nrow(walked)) {
          n_rows <- nrow(walked)
          more_blanks <- data.table(
            x = rep(NA_integer_, n_rows),
            y = rep(NA_integer_, n_rows),
            char = factor(rep(NA, n_rows), WALK_CHARS),
            facing = rep(NA_integer_, n_rows)
          )
          walked <- rbind(walked, more_blanks)
          rm(more_blanks)
        }
        added_rows <- position_ii + seq_len(steps_taken)
        set(walked, i = added_rows, j = c("x", "y", "char"), value = path)
        current_facing <- walked[position_ii, facing]
        set(walked, i = added_rows, j = "facing", value = current_facing)
      }
      position_ii <- position_ii + steps_taken
    }
  }
  walked[seq_len(position_ii)]
}


password <- function(position) {
  1000 * position[["y"]] + 4 * position[["x"]] + position[["facing"]] - 1
}



# Testing with the example
example_inlines <- (
"        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"
)
example_input <- parse_input(strsplit(example_inlines, "\n")[[1]])
example_path <- blaze_trail(
  map = example_input[["map"]],
  commands = example_input[["commands"]]
)
example_stop <- example_path[.N]
stopifnot(identical(password(example_stop), 6032))

# Part 1
inlines <- readLines("day22.txt")
puzzle_input <- parse_input(inlines)
path1 <- blaze_trail(
  map = puzzle_input[["map"]],
  commands = puzzle_input[["commands"]]
)
print(password(path1[.N]))


# Part 2
cube_map <- puzzle_input[["map"]]
connections <- rbind(
  data.table(
    x = 1, y = 51:100, facing = COMPASS[["N"]],
    next_x = 151:200, next_y = 1, next_facing = COMPASS[["E"]]
  ),
  data.table()
)
