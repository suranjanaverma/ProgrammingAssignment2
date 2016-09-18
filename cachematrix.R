
## Write a short comment describing this function

## makeCacheMatrix creates a list having functions which
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  
  ## Get function will return the value of the matrix
    get <- function() x
    
    ## Assign Inverse value of matrix
    setmatInv <- function(inverse) matInv <<- inverse
    
    ## Gets the value of matInv 
    getmatInv <- function() matInv
    list(set = set,
         get = get,
         setmatInv = setmatInv,
         getmatInv = getmatInv)
    
}


## Write a short comment describing this function
## cacheSolve function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  i <- x$getmatInv()
  if(!is.null(i)) {
    message("retrieving cached data")
    return(i)
  }
  newMatrix <- x$get()
  i <- solve(newMatrix)
  x$setmatInv(i)
  i
}
