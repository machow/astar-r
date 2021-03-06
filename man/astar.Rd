% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/astar.R
\name{astar}
\alias{astar}
\title{A* algorithm for shortest path finding.}
\usage{
astar(start, goal, cost_estimate, edge_distance, neighbors,
  is_goal_reached, hash_func = identity, search_node_env = NULL)
}
\arguments{
\item{start}{beginning node}

\item{goal}{ending node}

\item{cost_estimate}{binary function of node, goal. Should return Lower bound guess of distance between them.}

\item{edge_distance}{binary function of node, neighbor. Should return distance between them.}

\item{neighbors}{function that takes a node and returns its neighbors as a list.}

\item{is_goal_reached}{binary function of a node and the goal. Returns whether that node reached the goal.}

\item{hash_func}{function that takes a node and returns something that can be used as the key in a list (e.g. a number, a string...).}

\item{search_node_env}{custom environment to put node information in (may be useful for visualization).}
}
\description{
A* algorithm for shortest path finding.
}
\examples{
 nodes <- list(
   A = c(B = 100, C = 20),
   C = c(D = 20),
   D = c(B = 20)
 )
 neighbors <- function(node) names(nodes[[node]])
 cost_estimate <- function(node, goal) 1
 edge_distance <- function(src, dst) nodes[[src]][dst]
 is_goal_reached <- function(node, goal) identical(node, goal)

 path <- astar('A', 'B', cost_estimate, edge_distance, neighbors, is_goal_reached)

}
