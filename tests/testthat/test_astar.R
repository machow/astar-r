context("astar")

run_simple <- function(start, end, nodes) {
  neighbors <- function(node) names(nodes[[node]])
  cost_estimate <- function(node, goal) 1
  edge_distance <- function(src, dst) nodes[[src]][dst]
  is_goal_reached <- function(node, goal) identical(node, goal)

  astar::astar(start, end, cost_estimate, edge_distance, neighbors, is_goal_reached)
}

test_that("it takes a path with more nodes", {
  nodes <- list(
    A = c(B = 100, C = 20),
    C = c(D = 20),
    D = c(B = 20)
  )

  res <- run_simple("A", "B", nodes)
  expect_equal(res, list("A", "C", "D", "B"))
})
