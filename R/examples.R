#' AStar subclass demonstrating 2D maze navigation.
#'
#' Has methods for all function arguments of astar.
#' Takes a 2d matrix with 0 for walkable tiles, and any other number for walls
#'
#' @examples
#' M <- matrix(ncol = 4, byrow = TRUE, c(
#'   0, 1, 0, 0,
#'   0, 1, 0, 1,
#'   0, 1, 0, 1,
#'   0, 0, 0, 0
#'   ))
#'
#' sm <- SearchMaze2D$new(M)
#' sm$run(c(1, 1), c(1, 4))
#'
#' @rdname SearchMaze2D_class
#' @export
SearchMaze2D <- R6::R6Class(
  "SearchMaze2D",
  inherit = AStar,
  public = list(
    # attributes
    M = NULL,
    M_dim = NULL,
    adjacent = list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1)),
    # methods
    initialize = function (M) {
      self$M <- M
      self$M_dim <- dim(M)
    },
    neighbors = function(node) {
      candidates <- lapply(self$adjacent, function(x) x + node)
      Filter(function(ind) all(ind > 0) & all(ind < self$M_dim + 1), candidates)
    },
    hash_func = function(x) {
      paste(x, collapse = '-')
    },
    is_goal_reached = function(src, dst) {
      identical(src, dst)
    },
    edge_distance = function(src, dst) {
      if (self$M[dst[1], dst[2]] != 0) Inf else sum(abs(src - dst))
    },
    cost_estimate = function(node, goal) {
      sum(abs(node - goal))
    }
  )
)

#' Class to help visualize the inner workings of SearchMaze.
#'
#' MazeGazer adds a history entry that collects each paze the algorithm visits.
#' @examples
#'
#' M <- matrix(ncol = 4, byrow = TRUE, c(
#'   0, 1, 0, 0,
#'   0, 1, 0, 1,
#'   0, 1, 0, 1,
#'   0, 0, 0, 0
#'   ))
#'
#' mg <- MazeGazer$new(M)
#' mg$run(c(1, 1), c(1, 4))
#'
#' mg$history
#'
#'
#' @rdname MazeGazer_class
#' @export
MazeGazer <- R6::R6Class(
  "MazeGazer",
  inherit = SearchMaze2D,
  public = list(
    history = list(),
    round = 1,
    initialize = function(...) {
      super$initialize(...)
      self$search_node_env = new.env()
    },
    neighbors = function(node) {
      indx <- self$hash_func(node)
      node_data <- self$search_node_env[[indx]]

      path <- reconstruct_path(node_data)

      entry <- self$history_entry(self$round, path)
      self$history[[self$round]] <- entry
      self$round <- self$round + 1

      super$neighbors(node)
    },
    run = function(start, goal) {
      path <- super$run(start, goal)
      entry <- self$history_entry(self$round, path)

      return(path)
    },
    history_entry = function(round, path) {
      crnt_path <- do.call(rbind, path)

      df <- as.data.frame(cbind(round, crnt_path))
      setNames(df, c("round", "y", "x"))
    }
  )
)
