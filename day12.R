# https://adventofcode.com/2022/day/12
library(data.table)


parse_map <- function(text_lines) {
  chars <- strsplit(text_lines, "")
  do.call(rbind, chars)
}


connected_squares <- function(map, direction = c("forward", "backward")) {
  direction <- match.arg(direction)
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
  cons <- map_long[adjacencies, on = c("x", "y")]
  cons <- switch(direction,
    "forward" = cons[value + 1 >= value2, list(ii, ii2)],
    "backward" = cons[value2 + 1 >= value, list(ii, ii2)]
  )
  setkeyv(cons, c("ii", "ii2"))
  cons
}


begin_paths <- function(map, origin = c("S", "E")) {
  origin <- match.arg(origin)
  direction <- switch(origin, "S" = "forward", "E" = "backward")
  origin_index <- which(map == origin)
  paths <- data.table(
    history_str = as.character(origin_index),
    current = origin_index,
    previous = origin_index,
    n = 0L,
    path_id = "1"
  )
  setattr(paths, "history", paths[["current"]])
  setattr(paths, "connections", connected_squares(map, direction))
  setattr(paths, "pruned", paths[0])
  paths
}


extend_paths <- function(paths) {
  history <- attr(paths, "history")
  connections <- attr(paths, "connections")
  pruned <- attr(paths, "pruned")
  paths <- connections[
    paths,
    on = c(ii = "current"),
    allow.cartesian = TRUE,
  ][
    duplicated(path_id),
    path_id := NA
  ]
  setnames(paths, "ii", "current")
  na_pid <- which(is.na(paths[["path_id"]]))
  new_pids <- seq(
    max(as.integer(paths[["path_id"]]), na.rm = TRUE) + 1,
    length.out = length(na_pid)
  )
  new_pids <- formatC(new_pids, format = "d")
  set(paths, i = na_pid, j = "path_id", value = new_pids)
  paths[
    ,
    ":="(
      history_str = paste(history_str, ii2, sep = ","),
      previous = current,
      current = ii2,
      n = n + 1L,
      ii2 = NULL
    )
  ]
  # Remove paths that double back or rejoin a shorter path
  been_here <- paths[["current"]] %in% history
  # Keep only one path for each current location
  redundant <- duplicated(paths[["current"]])
  needs_pruned <- been_here | redundant
  new_pruned <- paths[needs_pruned, names(pruned), with = FALSE]
  pruned <- funion(pruned, new_pruned)
  paths <- paths[!needs_pruned]
  history <- union(history, paths[["current"]])
  setattr(paths, "history", history)
  setattr(paths, "connections", connections)
  setattr(paths, "pruned", pruned)
  paths
}


shortest_path <- function(map, max_steps = length(map)) {
  start_paths <- begin_paths(map, "S")
  end_paths <- begin_paths(map, "E")
  no_path_result <- start_paths[0, list(n, path_id, history_str)]
  while (TRUE) {
    if (any(start_paths[["current"]] %in% end_paths[["current"]])) break
    start_paths <- extend_paths(start_paths)
    if (nrow(start_paths) == 0) return(no_path_result)
    if (any(start_paths[["current"]] %in% end_paths[["current"]])) break
    end_paths <- extend_paths(end_paths)
    if (nrow(end_paths) == 0) return(no_path_result)
    # Escape when reality breaks down
    path_length <- min(start_paths[["n"]]) + min(end_paths[["n"]])
    if (path_length > length(map)) {
      stop(sprintf("Could not find a path shorter than traversing the whole map"))
    }
    if (path_length > max_steps) return(no_path_result)
  }
  final_paths <- start_paths[
    end_paths,
    on = "current",
    nomatch = 0,
    list(
      n = n + i.n,
      path_id = paste0(path_id, ".", i.path_id),
      start_history = history_str,
      end_history = i.history_str
    )
  ][
    ,
    end_history_rev := vapply(
      X = strsplit(end_history, ","),
      FUN = function(x) paste0(rev(x), collapse = ","),
      FUN.VALUE = character(1)
    )
  ][
    ,
    ":="(
      history_str = paste0(start_history, ",", end_history_rev),
      start_history = NULL,
      end_history = NULL,
      end_history_rev = NULL
    )
  ]
  final_paths
}


shortest_to_nadir <- function(map) {
  a_spots <- which(map %in% c("a", "S"))
  end_coords <- which(map == "E", arr.ind = TRUE)
  x_dist <- abs(end_coords[2] - col(map)[a_spots])
  y_dist <- abs(end_coords[1] - row(map)[a_spots])
  total_dist <- x_dist + y_dist
  a_spots <- a_spots[order(total_dist)]
  best <- data.table(n = length(map) + 1)
  for (spot in a_spots) {
    new_map <- map
    new_map[map == "S"] <- "a"
    new_map[spot] <- "S"
    path_to_spot <- shortest_path(new_map, max_steps = best[["n"]][1] - 1)
    if (nrow(path_to_spot) > 0 && path_to_spot[["n"]][1] < best[["n"]][1]) {
      best <- path_to_spot
    }
  }
  return(best)
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
stopifnot(example_path[["n"]][1] == 31)

example_nadir <- shortest_to_nadir(example_map)
print(example_nadir[["n"]][1])
stopifnot(example_nadir[["n"]][1] == 29)

# Part 1
letter_map <- parse_map(readLines("day12.txt"))
path1 <- shortest_path(letter_map)
print(path1[["n"]][1])

# Part 2
path2 <- shortest_to_nadir(letter_map)
print(path2[["n"]][1])
