# https://adventofcode.com/2022/day/12
library(data.table)


parse_map <- function(text_lines) {
  chars <- strsplit(text_lines, "")
  do.call(rbind, chars)
}


connected_squares <- function(map) {
  map_int <- matrix(match(map, letters), nrow = nrow(map), ncol = ncol(map))
  map_int[map == "S"] <- 1
  map_int[map == "E"] <- 26
  map_long <- data.table(
    ii = seq_along(map_int),
    x = as.vector(col(map_int)),
    y = as.vector(row(map_int)),
    value = as.vector(map_int)
  )
  north <- map_long[y > 1][, y := y - 1]
  south <- map_long[y < nrow(map_int)][, y := y + 1]
  east <- map_long[x > 1][, x := x - 1]
  west <- map_long[x > ncol(map_int)][, x := x + 1]
  adjacencies <- map_long[
    ,
    list(
      x = x + c(-1, 0, 0, 1),
      y = y + c(0, -1, 1, 0),
      value2 = value
    ),
    by = list(ii)
  ][
    x > 0 & x <= ncol(map_int) & y > 0 & y <= nrow(map_int)
  ]
  setnames(adjacencies, old = "ii", new = "ii2")
  cons <- map_long[
    adjacencies, on = c("x", "y")
  ][
    abs(value - value2) <= 1,
    list(ii, ii2)
  ]
  setkeyv(cons, c("ii", "ii2"))
  cons
}


shortest_path <- function(map) {
  connections <- connected_squares(map)
  start_ii <- which(map == "S")
  end_ii <- which(map == "E")
  paths <- data.table(
    history_str = as.character(start_ii),
    current = start_ii,
    previous = start_ii,
    n = 1L,
    path_id = "1"
  )
  history <- paths[, list(path_id, visited = current)]
  while (TRUE) {
    paths <- paths[
      connections,
      on = c(current = "ii"),
      allow.cartesian = TRUE,
      nomatch = 0
    ][
      , parent_path := path_id
    ][
      duplicated(path_id),
      path_id := NA
    ]
    na_pid <- which(is.na(paths[["path_id"]]))
    new_pids <- seq(
      max(as.integer(paths[["path_id"]]), na.rm = TRUE) + 1,
      length.out = length(na_pid)
    )
    new_pids <- formatC(new_pids, format = "d")
    set(paths, i = na_pid, j = "path_id", value = new_pids)
    paths[
      history,
      on = c(parent_path = "path_id", ii2 = "visited"),
      been_here := TRUE
    ]
    paths <- paths[is.na(been_here)]
    paths[
      ,
      ":="(
        history_str = paste(history_str, ii2, sep = ","),
        previous = current,
        current = ii2,
        n = n + 1L
      )
    ]
    if (!anyNA(paths[["been_here"]])) {
      stop("All paths will double back?!")
    }
    history <- unique(rbind(
      history,
      history[
        paths, on = c(path_id = "parent_path"), allow.cartesian = TRUE
      ][
        ,
        list(visited = c(visited, current[1])),
        by = list(path_id = i.path_id)
      ]
    ))
    set(paths, j = c("ii2", "parent_path", "been_here"), value = NULL)
  }
  paths[current == end_ii]
}


# Example
example_input <- "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"
example_inlines <- strsplit(example_input, "\n")[[1]]
example_map <- parse_map(example_inlines)
example_path <- shortest_path(example_map)
print(example_path[["n"]][1])

# Part 1
letter_map <- parse_map(readLines("day12.txt"))
path1 <- shortest_path(letter_map)
print(path1[["n"]][1])
