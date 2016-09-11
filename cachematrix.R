## Coursera "R Programming" course
## Week 3 programming assignment

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  slv <- NULL
  
  set <- function(y) {
    x <<- y
    slv <<- NULL
  }
  get <- function() x
  
  setSolve <- function(s) slv <<- s
  getSolve <- function() slv
  
  list(set = set,
       get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)
}


## checks if solve is cached for matrix x and if so, returns it
## otherwise calculates solve, store result and returns it
cacheSolve <- function(x, ...) {
  slv <- x$getSolve()
  
  if (!is.null(slv)) {
    message('getting cached solve')
    return(slv)
  }

  x$setSolve(solve(x$get(), ...))
  x$getSolve()
}
