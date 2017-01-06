## There are benefits of caching values instead of computing them repeatedly. (Even harder to compute majot operation again and again , time consuming and resources consuming)
## follwoing two functions use to cache the inverse of the a matrix 

## makeCacheMatrix- Creates list of functions-
## 1. set value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve -Returns - inverse of the matrix .
## if inverse has already computed it returns the same else computes again and returns .

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
