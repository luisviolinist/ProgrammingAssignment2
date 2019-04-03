# makeCacheMatrix
# This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(A = matrix()) {
  i <- NULL
  set <- function(y) {
    A <<- y
    i <<- NULL
  }
  get <- function() A
  setInv <- function(Inv) i <<- Inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# cacheSolve
# This function computes the inverse of makeCacheMatrix
cacheSolve <- function(A, ...) {
  i <- A$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- A$get()
  i <- solve(data, ...)
  A$setInv(i)
  i
}

# mat1 <- matrix(c(2,-3,4,-7),2,2, byrow = TRUE)
# mat1
# cacheSolve(makeCacheMatrix(mat1))
# solve(mat1)
