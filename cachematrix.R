## These R functions are designed to take advantage of the Lexical Scoping rules to 
## make sure that the process of inverting a matrix is not repeated. The first time
## the inverse function is calculated, the inverse matrix is stored in the cache and
## it can be restored instead of repeating the computation. 
## The functions are based on the examples given on the assignment 2 of the R programming
## class from JHU on coursera.


## This function creates a special list containing a function to:
## 1. Set the values of the matrix (function set)
## 2. Get the value of the matrix  (function get)
## 3. Set the value of the inverse matrix (function setinverse)
## 4. Get the value of the inverse matrix (function getinverse)

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## This function computes the inverse matrix, but it first checks if the inverse matrix has 
## already been calculated, in which case the inverse matrix is obtained from the cache. If 
## the inverse needs to be calculated, the function uses the solve() function to invert the 
## matrix and the inverse matrix is additionaly stored on the cache. 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}