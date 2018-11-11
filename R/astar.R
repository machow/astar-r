make_search_node <- function(data, gscore, fscore) {
  env <- new.env()
  env$data = data
  env$gscore = gscore
  env$fscore = fscore
  env$closed = FALSE
  env$out_openset = TRUE
  env$came_from = NULL
  env
}

# Store a user-defined node as data, so we can score and prioritize their search
SearchNode <- R6::R6Class("SearchNode", list(
  data = NULL,
  gscore = Inf,
  fscore = Inf,
  closed = FALSE,
  out_openset = TRUE,
  came_from = NULL,
  initialize = function(data, gscore, fscore) {
    self$data <- data
    self$gscore <- gscore
    self$fscore <- fscore
  }
  )
)

# Take a goal node, return a list of the nodes leading to it
reconstruct_path <- function(goal) {
  path <- list(goal$data)
  crnt <- goal
  while (!is.null(crnt$came_from)) {
    crnt <- crnt$came_from
    path <- c(list(crnt$data), path)
  }
  path
}


#' A* algorithm for shortest path finding.
#'
#' @param start beginning node
#' @param goal ending node
#' @param cost_estimate binary function of node, goal. Should return Lower bound guess of distance between them.
#' @param edge_distance binary function of node, neighbor. Should return distance between them.
#' @param neighbors function that takes a node and returns its neighbors as a list.
#' @param is_goal_reached binary function of a node and the goal. Returns whether that node reached the goal.
#' @param hash_func function that takes a node and returns something that can be used as the key in a list (e.g. a number, a string...).
#' @examples
#'  nodes <- list(
#'    A = c(B = 100, C = 20),
#'    C = c(D = 20),
#'    D = c(B = 20)
#'  )
#'  neighbors <- function(node) names(nodes[[node]])
#'  cost_estimate <- function(node, goal) 1
#'  edge_distance <- function(src, dst) nodes[[src]][dst]
#'  is_goal_reached <- function(node, goal) identical(node, goal)
#'
#'  path <- astar('A', 'B', cost_estimate, edge_distance, neighbors, is_goal_reached)
#'
#' @export
astar <- function(start, goal,
                  cost_estimate, edge_distance, neighbors, is_goal_reached, hash_func = identity) {
  if (is_goal_reached(start, goal))
    return(list(start))

  search_nodes <- list()

  start_node <- make_search_node(start, gscore = 0, fscore = cost_estimate(start, goal))
  start_hash <- hash_func(start)
  search_nodes[[start_hash]] <- start_node

  open_set <- datastructures::binomial_heap("numeric")
  # insert does not like inserting the SearchNode class
  datastructures::insert(open_set, start_node$fscore, start_hash)

  while (!is.null(datastructures::peek(open_set))) {
    crnt <- search_nodes[[datastructures::pop(open_set)[[1]]]]

    if (is_goal_reached(crnt$data, goal))
      return(reconstruct_path(crnt))

    crnt$out_openset <- TRUE
    crnt$closed <- TRUE

    # nodes need to be hashable
    for (neighbor in neighbors(crnt$data)) {
      indx <- hash_func(neighbor)
      neigh_node <- search_nodes[[indx]]
      if (is.null(neigh_node)) {
        neigh_node <- search_nodes[[indx]] <- make_search_node(neighbor, Inf, Inf)
      }

      if (neigh_node$closed) next

      # skip if this new path through neighbor has higher cost (to neighbor)
      tentative_gscore <- crnt$gscore + edge_distance(crnt$data, neigh_node$data)
      if (tentative_gscore >= neigh_node$gscore) next

      # update with new path, and estimated cost
      neigh_node$came_from <- crnt
      neigh_node$gscore <- tentative_gscore
      neigh_node$fscore <- tentative_gscore + cost_estimate(neigh_node$data, goal)

      # put back into open set, using updated score
      if (neigh_node$out_openset) {
        neigh_node$out_openset <- FALSE
        datastructures::insert(open_set, neigh_node$fscore, indx)
      }

    }
  }
}

.not_implemented <- function() {
  stop("This method needs to be overriden in child class.")
}

#' Convenience class for running A* algorithm.
#'
#' Has methods for all function arguments of astar.
#'
#' @rdname AStar_class
#' @examples
#'  nodes <- list(
#'    A = c(B = 100, C = 20),
#'    C = c(D = 20),
#'    D = c(B = 20)
#'  )
#'  Searcher <- R6::R6Class(
#'    "Searcher",
#'    inherit = AStar,
#'    public = list(
#'      neighbors = function(node) names(nodes[[node]]),
#'      cost_estimate = function(node, goal) 1,
#'      edge_distance = function(src, dst) nodes[[src]][dst],
#'      is_goal_reached = function(node, goal) identical(node, goal)
#'    )
#'  )
#'
#'  searcher <- Searcher$new()
#'  path <- searcher$run('A', 'B')
#'
#' @export
AStar <- R6::R6Class("AStar", list(
  cost_estimate = .not_implemented,
  edge_distance = .not_implemented,
  neighbors = .not_implemented,
  is_goal_reached = .not_implemented,
  hash_func = identity,
  run = function(start, goal) {
    astar(start, goal,
          self$cost_estimate, self$edge_distance, self$neighbors,
          self$is_goal_reached, self$hash_func)
  }
))
