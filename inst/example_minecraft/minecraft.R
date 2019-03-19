library(astar)
library(miner)    # remotes::install_github("ropenscilabs/miner")
library(R6)

# 3d pathfinding class, without any of the minecraft specific parts ---------------------
# All adjacent blocks (not including diagonals)
ADJACENT = c(
  sapply(
    list(c(0, 1), c(1, 0), c(-1, 0), c(0, -1)),
    function(xy) list(c(xy, -1), c(xy, 0), c(xy, 1))
  ))

PathFinder <- R6Class("PathFinder", inherit = AStar, lock_class = FALSE, lock_objects = FALSE, list(
  M = NULL,
  edge_callback = NULL,
  initialize = function(M, edge_callback = identity) {
    self$M = M
    self$edge_callback = edge_callback
  },
  # Abstract methods for A* ----------------------
  hash_func = function(x) paste(x, collapse = '-'),

  neighbors = function(node) {
    candidates <- lapply(ADJACENT, function(xyz) node + xyz)
    Filter(function(ind) all(ind > 0 & ind <= dim(self$M)), candidates)
  },

  is_goal_reached = function(src, dst) identical(src, dst),

  edge_distance = function(src, dst) {
    ind <- t(dst)
    block <- self$M[ind]
    above <- self$M[rbind(ind + c(0,0,1), ind + c(0,0,2))]

    # can't fit on block
    if (length(block) == 0 || block == 0 || any(above != 0))
      return(Inf)

    self$edge_callback(src, dst, score)
    1
  },

  cost_estimate = function(node, goal) sum(abs(node - goal))
))

# Functions for building in minecraft ---------------------------------------------------

# R uses (north, east, up), minecraft uses (east, up, north)
r_to_mc = c(2, 3, 1)
mc_to_r <- order(r_to_mc)
mc_origin <- c(200, 64, 264)   # setPlayerPos(0,0,0) goes to here

block_setter <- function(offset = c(0,0,0)) {
  # Return a function that sets block at (north/south, east/west, height) in minecraft
  function(r_xyz, ...){
    ijk <- (r_xyz + offset)[r_to_mc]
    miner::setBlock(ijk[1], ijk[2], ijk[3], ...)
  }
}

build_map <- function(height, insert, delete) {
  # Build 3d array of blocks, filled in up to height
  M <- array(dim = c(dim(height), max(height) + 3))

  for (ii in 1:max(height))
    M[cbind(which(height >= ii, TRUE), ii)] <- 1

  M[is.na(M)] = 0
  M <- array(M, c(dim(height), max(height) + 3))

  M[insert] <- 1
  M[delete] <- 0
  M
}

create_map <- function(M, offset) {
  # Set the blocks from M in minecraft
  # make empty space
  block_type = 0
  args <- c(c(0,0,0, dim(M)[r_to_mc]) + offset[r_to_mc], list(block_type))
  print(args)
  do.call(setBlocks, args)
  # fill in
  set_block <- block_setter(offset)
  apply(which((M != 0 & M != 31), arr.ind = TRUE), 1, function(xyz) {
    set_block(xyz, M[t(xyz)])
  })
  apply(which(M == 0, arr.ind = TRUE), 1, function(xyz) {
    set_block(xyz, M[t(xyz)])
  })
}

# Function for running PathFinder, while marking edges it checks in minecraft -----------
mc_find_path <- function(start, end, M, offset = c(0,0,0), create = TRUE) {
  if (create) create_map(M, offset)

  set_block <- block_setter(offset)

  marked <- array(dim = dim(M))
  edge_marker <- function(src, dst, score) {
    cat(src, ' : ', dst, ' : ', marked[t(dst)], '\n')
    if (is.na(marked[t(dst)])) {
      marked[t(dst)] <<- 1
      set_block(dst, 100)
      Sys.sleep(.05)
    }
  }

  pf <- PathFinder$new(M, edge_marker)

  res <- pf$run(start, end)

  lapply(rev(res), function(ind) {
    Sys.sleep(.1)
    set_block(ind, 35, 14)
  })

  res
}

# Example 1: building and navigating some terrain ---------------------------------------

mc_connect()     # start minecraft connection
height <- matrix(nrow = 6, byrow = TRUE, c(
  1, 1, 1, 1, 1, 1, 2, 6,
  4, 4, 3, 3, 5, 6, 3, 4,
  2, 2, 3, 4, 5, 4, 2, 4,
  1, 3, 3, 2, 4, 5, 1, 6,   # bridge (6)
  1, 5, 5, 4, 1, 1, 2, 5,
  1, 2, 3, 4, 5, 6, 3, 8    # bridge, tunnel
))

insert <- rbind(c(4, 7, 6), c(6, 7, 7), c(1, 6, 6), c(1, 7, 6))
delete <- rbind(c(6, 8, 5), c(6, 8, 6), c(6, 8, 7))

# build block map, then make it twice as big
M <- build_map(height, insert, delete)
newM <- M %x% matrix(1, nrow = 3, ncol = 3)

Sys.sleep(10)
mc_find_path(c(1,1,1), c(c(6,8)*2, 8), newM)

# Example 2: navigate terrain retrieved from the game -----------------------------------
# NOTE: currently all points in start must be less than than end
# To do yourself...
#   1. move to start tile, run mc_start <- getPlayerPos(<PLAYERID>)
#   2. move to end tile, run mc_end <- getPlayerPos(<PLAYERID>)
#   3. try using setBlock(x,y,z, 1) to see that mc_start, and mc_end
#      are blocks that you could stand on.

mc_connect()     # start minecraft connection
mc_start <- floor(getPlayerPos(210))
mc_end <- floor(getPlayerPos(210))

# how many blocks beyond start and end to grab
mc_pad <- c(10, 5, 10)
M_mc <- do.call(getBlocks, as.list(c(mc_start - mc_pad, mc_end + mc_pad)))

M_r <- aperm(M_mc, mc_to_r)

# Note: switch create to TRUE, to rebuild the landscape
# TODO: fix off-by-one error...
Sys.sleep(10)
mc_find_path(
  (c(1,1,1) + mc_pad)[mc_to_r],
  (mc_end - mc_start + mc_pad + c(1,0,1))[mc_to_r],
  M_r,
  offset = (mc_start - mc_pad - c(1,1,1))[mc_to_r],
  create = FALSE
  )
