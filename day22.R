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


COMPASS <- c(E = 1, S = 2, W = 3, N = 4)
WALK_CHARS <- c(E = ">", S = "v", W = "<", N = "^")


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
    path[, ":="(y = nrow(map) - y + 1, char = WALK_CHARS[facing])]
    plotted <- plotted +
      geom_tile(data = path, fill = "steelblue") +
      geom_text(data = path, aes(label = char), color = "darkblue")
  }
  plotted
}


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
cube_sides <- list(
  CJ(y =   1: 50, x =  51:100),
  CJ(y =   1: 50, x = 101:150),
  CJ(y =  51:100, x =  51:100),
  CJ(y = 101:150, x =   1: 50),
  CJ(y = 101:150, x =  51:100),
  CJ(y = 151:200, x =   1: 50)
)
for (ii in seq_along(cube_sides)) {
  cube_sides[[ii]][, char := puzzle_input[["map"]][cbind(y, x)]]
}


rotate_matrix <- function(mat, degrees = 0L) {
  degrees <- as.integer(degrees)
  if (!(length(degrees) == 1 & degrees %in% c(0L, 90L, 180L, 270L))) {
    stop("degrees must be one of 0, 90, 180, or 270")
  }
  nr <- nrow(mat)
  nc <- ncol(mat)
  if (degrees == 0L) {
    return(mat)
  }
  if (degrees == 90L) {
    indices <- cbind(
      rep(seq(nr, 1), each = nc),
      rep(seq_len(nc), nr)
    )
    return(matrix(mat[indices], nrow = nc, ncol = nr))
  } else if (degrees == 180L) {
    return(mat[seq(nr, 1), seq(nc, 1)])
  } else if (degrees == 270L) {
    indices <- cbind(
      rep(seq_len(nr), each = nc),
      rep(seq(nc, 1), nr)
    )
    return(matrix(mat[indices], nrow = nc, ncol = nr))
  }
}


make_rings <- function(
  side1, dir1,
  side2, dir2,
  side3, dir3,
  side4, dir4
) {
  sides <- rbindlist(list(side1, side2, side3, side4), idcol = "side")
  sides[side == 1, facing := dir1]
  sides[side == 2, facing := dir2]
  sides[side == 3, facing := dir3]
  sides[side == 4, facing := dir4]
  setindexv(sides, c("side", "x", "y"))
  setorderv(sides, c("side", "x", "y"))
  side_length <- max(side1[["x"]]) - min(side1[["x"]]) + 1
  index_ring <- sides[, matrix(seq_len(.N), nrow = side_length)]
  dirs <- c(dir1, dir2, dir3, dir4)
  compass_degrees <- c(E = 0, S = 90, W = 180, N = 270)
  for (ii in seq_along(dirs)) {
    side_cols <- seq_len(side_length) + side_length * (ii - 1)
    index_ring[, side_cols] <- rotate_matrix(
      index_ring[, side_cols],
      compass_degrees[dirs[[ii]]]
    )
  }
  ring_dt <- sides[
    as.vector(index_ring)
  ][
    ,
    ":="(
      ring = rep(seq_len(side_length), 4 * side_length),
      ordering = rep(seq_len(4 * side_length), each = side_length)
    )
  ]
  reversed <- copy(ring_dt)
  reversed[, ring := ring + max(ring)]
  reversed[, ordering := order(-ordering), by = ring]
  reversed[, facing := mod_base_1(facing + 2, 4)]
  out <- rbind(ring_dt, reversed)
  setorderv(out, c("ring", "ordering"))
  out
}


x_rings <- make_rings(
  side1 = cube_sides[[1]], dir1 = COMPASS[["E"]],
  side2 = cube_sides[[2]], dir2 = COMPASS[["E"]],
  side3 = cube_sides[[5]], dir3 = COMPASS[["W"]],
  side4 = cube_sides[[4]], dir4 = COMPASS[["W"]]
)
y_rings <- make_rings(
  side1 = cube_sides[[1]], dir1 = COMPASS[["N"]],
  side2 = cube_sides[[6]], dir2 = COMPASS[["E"]],
  side3 = cube_sides[[5]], dir3 = COMPASS[["N"]],
  side4 = cube_sides[[3]], dir4 = COMPASS[["N"]]
)
z_rings <- make_rings(
  side1 = cube_sides[[2]], dir1 = COMPASS[["N"]],
  side2 = cube_sides[[6]], dir2 = COMPASS[["N"]],
  side3 = cube_sides[[4]], dir3 = COMPASS[["N"]],
  side4 = cube_sides[[3]], dir4 = COMPASS[["E"]]
)
y_rings[, ring := ring + max(x_rings[["ring"]])]
z_rings[, ring := ring + max(y_rings[["ring"]])]
cube_rings <- rbind(x_rings, y_rings, z_rings)


blaze_cube <- function(rings, commands) {
  n <- nrow(rings)
  ring_length <- max(rings[["ordering"]])
  walked <- data.table(
    x = rep(NA_integer_, n),
    y = rep(NA_integer_, n),
    facing = rep(NA_integer_, n)
  )
  start_pos <- rings[y == 1 & char != "#", list(x = min(x), y = 1L)]
  walked[
    1,
    ":="(x = start_pos[["x"]], y = start_pos[["y"]], facing = COMPASS[["E"]])
  ]
  position_ii <- 1L
  for (cmd in commands) {
    if (identical(cmd, "R")) {
      walked[position_ii, facing := mod_base_1(facing + 1, 4)]
    } else if (identical(cmd, "L")) {
      walked[position_ii, facing := mod_base_1(facing - 1, 4)]
    } else {
      ring_info <- rings[walked[position_ii], on = c("x", "y", "facing")]
      path_key <- ring_info[, list(
        ring,
        ordering = mod_base_1(ordering + seq_len(cmd), ring_length)
      )]
      path <- rings[
        path_key, on = c("ring", "ordering"),
        list(x, y, facing)
      ]
      wall_loc <- which(path[["char"]] == "#")
      if (length(wall_loc) > 0) {
        first_wall <- min(wall_loc)
        path <- path[seq_len(first_wall - 1)]
      }
      steps_taken <- nrow(path)
      if (steps_taken > 0) {
        if (position_ii + steps_taken > nrow(walked)) {
          n_rows <- nrow(walked)
          more_blanks <- data.table(
            x = rep(NA_integer_, n_rows),
            y = rep(NA_integer_, n_rows),
            facing = rep(NA_integer_, n_rows)
          )
          walked <- rbind(walked, more_blanks)
          rm(more_blanks)
        }
        added_rows <- position_ii + seq_len(steps_taken)
        set(walked, i = added_rows, j = c("x", "y", "facing"), value = path)
      }
      position_ii <- position_ii + steps_taken
    }
  }
  walked[seq_len(position_ii)]
}


example_sides <- list(
  CJ(x = 9:12, y = 1:4),
  CJ(x = 1:4, y = 5:8),
  CJ(x = 5:8, y = 5:8),
  CJ(x = 9:12, y = 5:8),
  CJ(x = 9:12, y = 9:12),
  CJ(x = 13:16, y = 9:12)
)
for (ii in seq_along(example_sides)) {
  example_sides[[ii]][, char := example_input[["map"]][cbind(y, x)]]
}

ex_x_rings <- make_rings(
  side1 = example_sides[[1]], dir1 = COMPASS[["N"]],
  side2 = example_sides[[2]], dir2 = COMPASS[["S"]],
  side3 = example_sides[[5]], dir3 = COMPASS[["N"]],
  side4 = example_sides[[4]], dir4 = COMPASS[["N"]]
)
ex_y_rings <- make_rings(
  side1 = example_sides[[1]], dir1 = COMPASS[["E"]],
  side2 = example_sides[[6]], dir2 = COMPASS[["W"]],
  side3 = example_sides[[5]], dir3 = COMPASS[["W"]],
  side4 = example_sides[[3]], dir4 = COMPASS[["N"]]
)
ex_z_rings <- make_rings(
  side1 = example_sides[[2]], dir1 = COMPASS[["E"]],
  side2 = example_sides[[3]], dir2 = COMPASS[["E"]],
  side3 = example_sides[[4]], dir3 = COMPASS[["E"]],
  side4 = example_sides[[6]], dir4 = COMPASS[["S"]]
)
ex_y_rings[, ring := ring + max(ex_x_rings[["ring"]])]
ex_z_rings[, ring := ring + max(ex_y_rings[["ring"]])]
ex_rings <- rbind(ex_x_rings, ex_y_rings, ex_z_rings)

path_ex_cube <- blaze_cube(
  rings = ex_rings, commands = example_input[["commands"]]
)
stopifnot(password(path_ex_cube[.N]) == 5031)


path2 <- blaze_cube(
  rings = cube_rings,
  commands = puzzle_input[["commands"]]
)
print(password(path2[.N]))
