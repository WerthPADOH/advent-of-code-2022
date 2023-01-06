# Day 14
library(data.table)


seq_path <- function(path_coords) {
  segments <- lapply(
    seq_len(nrow(path_coords) - 1),
    function(ii) {
      cbind(
        x = seq(path_coords[ii, "x"], path_coords[ii + 1, "x"]),
        y = seq(path_coords[ii, "y"], path_coords[ii + 1, "y"])
      )
    }
  )
  do.call(rbind, segments)
}


locate_rock <- function(path_text) {
  pieces <- strsplit(path_text, "\\D+")
  vertices <- lapply(
    pieces,
    function(pp) {
      pp <- as.integer(pp)
      odds <- seq(1, length(pp), by = 2)
      cbind(x = pp[odds], y = pp[odds + 1])
    }
  )
  path_coords <- lapply(vertices, seq_path)
  do.call(rbind, path_coords)
}


fill_with_sand <- function(rock_coords, sand_origin = c(x = 500, y = 0)) {
  x_min <- min(rock_coords[, "x"]) - 1
  x_max <- max(rock_coords[, "x"]) + 1
  y_max <- max(rock_coords[, "y"]) + 1
  sand_origin["x"] <- sand_origin["x"] - x_min + 1
  rock_coords[, "x"] <- rock_coords[, "x"] - x_min + 1
  map <- matrix("air", nrow = y_max, ncol = x_max - x_min + 1)
  map[rock_coords[, c("y", "x")]] <- "rock"
  map[sand_origin["y"] + 1, sand_origin["x"]] <- "sand"
  # Fill with sand
  for (height in seq(2, y_max - 1)) {
    row_above <- map[height - 1, ]
    sand_above <- which(row_above == "sand")
    reachable <- unique(c(sand_above, sand_above - 1, sand_above + 1))
    reachable <- reachable[reachable > 0 & reachable <= ncol(map)]
    is_rock <- map[height, ] == "rock"
    reachable <- setdiff(reachable, which(is_rock))
    map[height, reachable] <- "sand"
  }
  # Remove what flows into the void
  for (height in seq(y_max - 1, 1)) {
    air_below <- which(map[height + 1, ] == "air")
    falls_down <- unique(c(air_below, air_below - 1, air_below + 1))
    falls_down <- falls_down[falls_down > 0 & falls_down <= ncol(map)]
    is_sand <- map[height, ] == "sand"
    falls_down <- intersect(which(is_sand), falls_down)
    map[height, falls_down] <- "air"
  }
  # Remove the sand that wouldn't have flowed to the right
  one_to_left <- cbind("air", map[, seq(ncol(map) - 1)])
  solid_to_left <- one_to_left %in% c("rock", "sand")
  solid_to_left <- matrix(solid_to_left, nrow = nrow(map))
  two_to_left <- cbind("air", "air", map[, seq(ncol(map) - 2)])
  air_to_far_left <- two_to_left == "air"
  air_to_far_left <- matrix(air_to_far_left, nrow = nrow(map))
  one_to_right <- cbind(map[, seq(2, ncol(map))], "air")
  for (height in seq(2, y_max - 1)) {
    no_source <- map[height - 1, ] != "sand" &
      one_to_left[height - 1, ] != "sand" &
      one_to_right[height - 1, ] != "sand"
    air_above <- map[height - 1, ] == "air"
    wrong_right_flow <- map[height, ] == "sand" &
      air_above &
      solid_to_left[height, ] &
      air_to_far_left[height, ]
    map[height, no_source | wrong_right_flow] <- "air"
  }
  map
}


find_left_slope <- function(rock_coords, sand_origin) {
  # Highest point of sand is always along 45 degrees from the rock space closest
  # to the top-left corner, Manhattan-distance style. From that point, it will
  # fill a diagonal down-and-left path (except where there's rock) to that close
  # rock point
  rock_below <- rock_coords[, "y"] > sand_origin["y"]
  rock_left <- rock_coords[, "x"] <= sand_origin["x"]
  rock_coords <- rock_coords[rock_below & rock_left, , drop = FALSE]
  if (nrow(rock_coords) == 0) {
    return(rock_coords)
  }
  dist_to_corner <- apply(rock_coords, 1, max)
  diag_dist <- min(dist_to_corner)
  is_close <- rock_coords[, "x"] == diag_dist | rock_coords[, "y"] == diag_dist
  is_close <- which(is_close)
  close_side <- apply(rock_coords, 1, min)
  closest <- is_close[which.min(close_side[is_close])]
  closest_rock <- rock_coords[closest, ]
  dist_to_origin <- sand_origin["x"] - closest_rock["x"]
  top_sand <- c(x = sand_origin["x"], y = closest_rock["y"] - dist_to_origin)
  names(top_sand) <- c("x", "y")
  cbind(
    x = seq(top_sand["x"], closest_rock["x"], by = -1),
    y = seq(top_sand["y"], closest_rock["y"], by = 1)
  )
}


fill_with_sand2 <- function(rock_coords, sand_origin = c(x = 500, y = 0)) {
  x_min <- min(rock_coords[, "x"]) - 1
  x_max <- max(rock_coords[, "x"]) + 1
  y_max <- max(rock_coords[, "y"]) + 1
  sand_origin["x"] <- sand_origin["x"] - x_min + 1
  rock_coords[, "x"] <- rock_coords[, "x"] - x_min + 1
  map <- matrix("air", nrow = y_max, ncol = x_max - x_min + 1)
  map[rock_coords[, c("y", "x")]] <- "rock"
  iter_origin <- sand_origin
  while (TRUE) {
    left_slope <- find_left_slope(rock_coords, iter_origin)
    if (nrow(left_slope) == 0) {
      break
    }
    slope_contents <- map[left_slope[, c("y", "x")]]
    fill_slope <- slope_contents == "air"
    fill_key <- left_slope[fill_slope, c("y", "x"), drop = FALSE]
    map[fill_key] <- "sand"
    iter_origin <- left_slope[nrow(left_slope), ]
    iter_origin["x"] <- iter_origin["x"] - 1
  }
  # Flow from the slopes
  one_to_left <- cbind("air", map[, seq(ncol(map) - 1)])
  one_to_right <- cbind(map[, seq(2, ncol(map))], "air")
  for (height in seq(2, y_max - 2)) {
    sand_above <- which(map[height - 1, ] == "sand")
    flowable <- unique(c(sand_above, sand_above - 1, sand_above + 1))
    flowable <- flowable[flowable > 0 & flowable <= ncol(map)]
    is_open <- map[height, ] == "air"
    need_fill <- intersect(flowable, which(is_open))
    map[height, need_fill] <- "sand"
  }
  # Remove what flows into the void
  for (height in seq(y_max - 1, 1)) {
    air_below <- which(map[height + 1, ] == "air")
    falls_down <- unique(c(air_below, air_below - 1, air_below + 1))
    falls_down <- falls_down[falls_down > 0 & falls_down <= ncol(map)]
    is_sand <- map[height, ] == "sand"
    falls_down <- intersect(which(is_sand), falls_down)
    map[height, falls_down] <- "air"
  }
  # Remove the sand that wouldn't have flowed to the right
  solid_to_left <- one_to_left %in% c("rock", "sand")
  solid_to_left <- matrix(solid_to_left, nrow = nrow(map))
  two_to_left <- cbind("air", "air", map[, seq(ncol(map) - 2)])
  air_to_far_left <- two_to_left == "air"
  air_to_far_left <- matrix(air_to_far_left, nrow = nrow(map))
  for (height in seq(2, y_max - 1)) {
    no_source <- map[height, ] == "sand" &
      map[height - 1, ] != "sand" &
      one_to_left[height - 1, ] != "sand" &
      one_to_right[height - 1, ] != "sand"
    air_above <- map[height - 1, ] == "air"
    wrong_right_flow <- map[height, ] == "sand" &
      air_above &
      solid_to_left[height, ] &
      air_to_far_left[height, ]
    map[height, no_source | wrong_right_flow] <- "air"
  }
  map
}


plot_grid <- function(grid_map, ..., xaxt = "n", yaxt = "n", bty = "n") {
  num_grid <- matrix(NA_integer_, nrow = nrow(grid_map), ncol = ncol(grid_map))
  num_grid[] <- match(grid_map, c("air", "rock", "sand"))
  rotated <- t(num_grid)
  flipped <- rotated[, seq(ncol(rotated), 1, by = -1)]
  image(flipped, ..., xaxt = xaxt, yaxt = yaxt, bty = bty)
}


# Example
example_input <- c("498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9")
example_rocks <- locate_rock(example_input)
example_grid <- fill_with_sand2(example_rocks)
example_sand_volume <- sum(example_grid == "sand")
print(example_sand_volume)
stopifnot(example_sand_volume == 24)

# Part 1
inlines <- readLines("day14.txt")
rocks <- locate_rock(inlines)
sandiness <- fill_with_sand2(rocks)
print(sum(sandiness == "sand"))
