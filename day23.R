# https://adventofcode.com/2022/day/23

parse_elves <- function(in_lines) {
  chars <- unlist(strsplit(in_lines, ""))
  mat <- matrix(
    chars, nrow = length(in_lines), ncol = nchar(in_lines[1]),
    byrow = TRUE
  )
  coords <- which(mat == "#", arr.ind = TRUE)
  coords <- t(coords)
  rownames(coords) <- c("y", "x")
  coords
}


shares_point <- function(coord1, coord2) {
  same_x <- outer(coord1["x", ], coord2["x", ], FUN = `==`)
  same_y <- outer(coord1["y", ], coord2["y", ], FUN = `==`)
  rowSums(same_x & same_y, na.rm = TRUE) > 0
}


duplicated_point <- function(coords) {
  same_x <- outer(coords["x", ], coords["x", ], FUN = `==`)
  same_y <- outer(coords["y", ], coords["y", ], FUN = `==`)
  same_point <- same_x & same_y
  diag(same_point) <- FALSE
  rowSums(same_point) > 0
}


print_coords <- function(coords) {
  coords["x", ] <- coords["x", ] - min(coords["x", ]) + 1
  coords["y", ] <- coords["y", ] - min(coords["y", ]) + 1
  mat <- matrix(".", nrow = max(coords["y", ]), ncol = max(coords["x", ]))
  mat[t(coords)] <- "#"
  text_lines <- apply(mat, 1, paste0, collapse = "")
  cat(text_lines, sep = "\n")
}


scatter_elves <- function(elf_coords, rounds = NULL) {
  shifts <- list(n = c(-1, 0),  e = c(0, 1))
  shifts[["s"]] <- -shifts[["n"]]
  shifts[["w"]] <- -shifts[["e"]]
  shifts[["ne"]] <- shifts[["n"]] + shifts[["e"]]
  shifts[["se"]] <- shifts[["s"]] + shifts[["e"]]
  shifts[["sw"]] <- shifts[["s"]] + shifts[["w"]]
  shifts[["nw"]] <- shifts[["n"]] + shifts[["w"]]
  decision_info <- list(
    list(move = "n", look = c("nw", "n", "ne")),
    list(move = "s", look = c("sw", "s", "se")),
    list(move = "w", look = c("nw", "w", "sw")),
    list(move = "e", look = c("ne", "e", "se"))
  )
  elf_count <- ncol(elf_coords)
  rd <- 0
  show_rounds <- is.null(rounds)
  if (show_rounds) {
    rounds <- Inf
  }
  while (rd < rounds) {
    rd <- rd + 1
    proposed <- matrix(
      NA_integer_, nrow = 2, ncol = elf_count,
      dimnames = list(c("y", "x"), NULL)
    )
    shifted <- lapply(shifts, `+`, elf_coords)
    neighbored <- lapply(shifted, shares_point, elf_coords)
    no_neighbors <- !Reduce(`|`, neighbored)
    proposed[, no_neighbors] <- elf_coords[, no_neighbors]
    decision_order <- ((seq(rd, length.out = 4) - 1) %% 4) + 1
    for (decision in decision_info[decision_order]) {
      move <- decision[["move"]]
      look <- decision[["look"]]
      blocked_arc <- Reduce(`|`, neighbored[look])
      undecided <- is.na(proposed["x", ])
      moving <- undecided & !blocked_arc
      proposed[, moving] <- shifted[[move]][, moving]
    }
    undecided <- is.na(proposed["x", ])
    proposed[, undecided] <- elf_coords[, undecided]
    same_prop <- duplicated_point(proposed)
    proposed[, same_prop] <- elf_coords[, same_prop]
    if (all(proposed == elf_coords)) {
      break
    }
    elf_coords <- proposed
  }
  if (show_rounds) {
    print(rd)
  }
  elf_coords
}


empty_ground <- function(coords) {
  x_range <- range(coords["x", ])
  y_range <- range(coords["y", ])
  x_width <- x_range[2] - x_range[1] + 1
  y_width <- y_range[2] - y_range[1] + 1
  x_width * y_width - ncol(coords)
}


# Testing on example ----
ex_small <- c(
  ".....",
  "..##.",
  "..#..",
  ".....",
  "..##.",
  "....."
)
ex_small_elves <- parse_elves(ex_small)
ex_small_res <- scatter_elves(ex_small_elves, 10)
stopifnot(identical(empty_ground(ex_small_res), 25))

ex_medium <- c(
  "....#..",
  "..###.#",
  "#...#.#",
  ".#...##",
  "#.###..",
  "##.#.##",
  ".#..#.."
)
ex_med_elves <- parse_elves(ex_medium)
ex_med_res <- scatter_elves(ex_med_elves, 10)
stopifnot(identical(empty_ground(ex_med_res), 110))

scatter_elves(ex_med_elves)

# Part 1
input_lines <- readLines("day23.txt")
part1_elves <- parse_elves(input_lines)
part1_res <- scatter_elves(part1_elves, 10)
print(empty_ground(part1_res))

# Part 2
part2_res <- scatter_elves(part1_elves)
