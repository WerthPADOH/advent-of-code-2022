# https://adventofcode.com/2022/day/19
resources <- c(
  ore = 0, clay = 0, obsidian = 0, geode = 0,
  ore_bot = 1, clay_bot = 0, obs_bot = 0, geo_bot = 0
)

# Transformations
no_build <- matrix(0, nrow = length(resources), ncol = length(resources))
diag(no_build) <- 1
no_build[cbind(5:8, 1:4)] <- 1
