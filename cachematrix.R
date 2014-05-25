## The two functions below are meant to calculate the inversion of a given matrix
## Given that matrix inverse computation is a costly and time consuming operation
## it is benificial to avoid recomputation for a matrix whose inverse has previously
## been calculated. The caching feature is utilized to achieve this.

# makeCacheMatrix function creates a special "vector", 
# which is really a list containing a function to

#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the matrix inverse
#4. get the value of the maxtrix inverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) m <<- solve
    
    getinverse <- function() m
    
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the matrix 
## created with the above function but it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation, else it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinverse function

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

