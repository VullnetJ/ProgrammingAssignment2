
## The first step is to create an input as a matrix for the vaiable makeCasheMatrix
## I changed the "m" to "a"
## The "mean' i changed to "inv"
makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setinv <- function(inv) a <<- inv
  getinv <- function() a
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The same thing is done in the casheinv as well, by replacing "m" to "a" and then "mean" to "inv"
cacheinv <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a <- x$getinv()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  dat <- x$get()
  a <- solve(dat, ...)
  x$setinv(a)
  a
}
