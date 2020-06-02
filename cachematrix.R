############################################################################################
##
## R Programming - Week 3 - Assignment 1
## J Costa
##
############################################################################################

############################################################################################
# makeCacheMatrix - Function to invert matrix and cache in global memory
############################################################################################

makeCacheMatrix <- function(x = matrix()) {
  # clear global vars
  mtrx <<- x 
  imtrx <<- NULL
  
  #define function "set" and sets a global vairable x, and resets global variable m
  setMatrix <- function(y) { 
    mtrx <<- y
    imtrx <<- NULL
  }
  
  # defines function "getMatrix" as a returning global variable mtrx
  getMatrix <- function() mtrx
  
  # defines function "invMatrix" inverts mtrx and returns global variable imtrx
  invMatrix <- function() imtrx <<- solve(mtrx)
  
  # defines function "getMatrix" as a returning global variable mtrx
  getiMatrix <- function() imtrx
  
  # defines function "clrMatrix" - clear contents of imtrx
  clriMatrix <- function() imtrx <<- NULL
  
  #creates and returns(?) a list of functions
  list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        invMatrix = invMatrix,
        getiMatrix = getiMatrix,
        clriMatrix = clriMatrix
      )
}

######################################################################
# cacheMatrix - Function to call makeCacheMatrix invert matrix or use cached matrix
######################################################################

cacheMatrix <- function(x, ...) {
  
  # Calls list with function get inverted matrix
  im <- x$getiMatrix()
  
  # Checks if global variable is null
  if(!is.null(im)) {
    message("Cache is availble... using cache")
    # The inverse of the matrix already exists - no need to calculate; return the inverted matrix
    return(im)
  }

  # Otherwise, calculate the Inverted matrix  
  # Data is now avaialble (either from above or b/c it was there already), and return the data
  message("\nCache not available... generating inverted matrix\n")
  x$invMatrix()
  im <- x$getiMatrix()
  
  return(im)
}
######################################################################