
# This function creates a special "matrix" object that can cache its inverse.
# Computing the inverse of a square matrix can be done with the solve 
# function in R. For example, if X is a square invertible matrix, then solve(X) 
# returns its inverse. We assume a matrix is square, and if not then a message is 
# returned.
makeCacheMatrix <- function(x = matrix()){
  if (nrow(x) != ncol(x)){
    print("This is not a square matrix")
  }
  else {
    original <- x # local variable containing an empty original matrix
    inverse <- NULL # local variable containing the inverse matrix
    set <- function(y) {
      if (!is.matrix(y)){
        print("Parameter object is not a matrix")
      }
      else{
        if (nrow(y) != ncol(y)){
          print("This is not a square matrix")
        }
        else{
          # R takes care of changing the dimensions if the sizes of the matrices are different
          original <<- y
          inverse <<- NULL # Set the inverse to NULL
        }
      }      
    }
    get <- function() original
    setinverse <- function(x) inverse <<- x
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then 
# the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  invMatrix <- x$getinverse()
  if(!is.null(invMatrix)) {
    message("getting the cached inverse matrix...")
    return(invMatrix)
  }
  originalMatrix <- x$get()
  inverseMatrix <- solve(originalMatrix, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}