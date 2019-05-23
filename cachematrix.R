## Matrix inverse: If a matrix is denoted A, we can assume its inverse is B <- A^(-1)
## If we take A*B we get the Identity Matrix
## A*B = B*A = I (This should be true) essentially its matrice division

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL                                #following the given example mean of a vector
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setINV <- function(inverse) I <<- inverse
  getINV <- function() I
  list(set = set, get = get,
       setINV = setINV,
       getINV = getINV)
  
}


## cache solve is to obtain the inverse of the result of makecachematirx

cacheSolve <- function(x, ...) {
  I <- x$getINV()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setINV(I)
  I
}
