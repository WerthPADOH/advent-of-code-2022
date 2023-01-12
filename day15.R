# https://adventofcode.com/2022/day/15
library(stringi)
library(data.table)


parse_sensors <- function(reports) {
  sensor_info <- stri_match_first_regex(reports, c("x=(-?\\d+), y=(-?\\d+)"))
  beacon_info <- stri_match_last_regex(reports, c("x=(-?\\d+), y=(-?\\d+)"))
  data.table(
    sensor_x = as.integer(sensor_info[, 2]),
    sensor_y = as.integer(sensor_info[, 3]),
    beacon_x = as.integer(beacon_info[, 2]),
    beacon_y = as.integer(beacon_info[, 3])
  )[
    , reach := abs(beacon_x - sensor_x) + abs(beacon_y - sensor_y)
  ]
}


sure_no_beacon <- function(sensor_data, y) {
  sensor_data <- copy(sensor_data)
  sensor_data[, distance_to_y := abs(sensor_y - y)]
  sensor_data <- sensor_data[distance_to_y <= reach]
  sensor_data[, ":="(
    row_x_min = sensor_x - (reach - distance_to_y),
    row_x_max = sensor_x + (reach - distance_to_y)
  )]
  cover_min <- min(sensor_data[["row_x_min"]])
  cover_max <- max(sensor_data[["row_x_max"]])
  examined_x <- seq(cover_min, cover_max)
  row_beacons <- sensor_data[beacon_y == y, beacon_x]
  examined_x <- setdiff(examined_x, row_beacons)
  above_min <- outer(examined_x, sensor_data[["row_x_min"]], FUN = ">=")
  below_max <- outer(examined_x, sensor_data[["row_x_max"]], FUN = "<=")
  out_of_range <- above_min & below_max
  included <- rowSums(out_of_range) > 0
  examined_x[included]
}


bordering_squares <- function(x, y, reach) {
  top_left <- data.table(
    x = seq(x - reach - 1, x),
    y = seq(y, y - reach - 1, by = -1)
  )
  bottom_right <- data.table(
    x = seq(x + reach + 1, x, by = -1),
    y = seq(y, y + reach + 1, by = 1)
  )
  bottom_left <- data.table(
    x = seq(x - reach, x - 1),
    y = seq(y + 1, y + reach)
  )
  top_right <- data.table(
    x = seq(x + 1, x + reach),
    y = seq(y - reach, y - 1)
  )
  unique(rbind(top_left, bottom_right, bottom_left, top_right))
}


# If only a single square isn't in range of a sensor, that means each adjacent
# square is at the furthest reach of a sensor. So we just need to look at all
# the "border adjacent" squares and filter out the ones within reach of any
# sensor.
find_unsensed <- function(sensor_data, min_coord, max_coord) {
  candidates <- data.table(x = integer(0), y = integer(0))
  indexes <- seq_len(nrow(sensor_data))
  for (ii in indexes) {
    border <- sensor_data[
      ii,
      bordering_squares(sensor_x, sensor_y, reach)
    ][
      between(x, min_coord, max_coord) &
        between(y, min_coord, max_coord)
    ]
    for (jj in setdiff(indexes, ii)) {
      border[, ":="(
        x_dist = abs(x - sensor_data[["sensor_x"]][jj]),
        y_dist = abs(y - sensor_data[["sensor_y"]][jj])
      )]
      border <- border[x_dist + y_dist > sensor_data[["reach"]][jj]]
    }
    candidates <- unique(rbind(candidates, border[, list(x, y)]))
  }
  candidates
}


# Example
example_reports <- c(
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
  "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
  "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
  "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
  "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
  "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
  "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
  "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
  "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
  "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
  "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
  "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
  "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
  "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
)
example_sensors <- parse_sensors(example_reports)
example_not_beacon <- sure_no_beacon(example_sensors, 10)
print(length(example_not_beacon))
stopifnot(length(example_not_beacon) == 26)

example_unsensed <- find_unsensed(example_sensors, 0, 20)
print(example_unsensed)
stopifnot(example_unsensed == cbind(x = 14, y = 11))

# Part 1
reports <- parse_sensors(readLines("day15.txt"))
not_beacon_2m <- sure_no_beacon(reports, 2000000)
print(length(not_beacon_2m))

# Part 2
# Break into pieces
true_beacon <- find_unsensed(reports, 0, 4000000)
print(formatC(4000000 * true_beacon[["x"]] + true_beacon[["y"]], format = "f"))
