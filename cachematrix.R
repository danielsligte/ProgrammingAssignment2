## In this script I will make a matrix object that can cache its inverse
## Subsequently, I will write a function that actually returns the inverse of a matrix



## make a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix())  {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

### test of the makematrix function
amatrix = makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2) )
amatrix$get()
amatrix$getinverse()

## make the function that caches the inverse of a matrix

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

## and now the test if caching the inverse works:
amatrix$getinverse()
cacheSolve(amatrix)
amatrix$get()
amatrix$getinverse()
cacheSolve(amatrix)
