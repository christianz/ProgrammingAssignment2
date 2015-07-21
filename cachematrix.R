## The makeCacheMatrix function creates a matrix object
## that has the ability to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## When this function is called, clear the cached
  ## version of the matrix.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Return environment x
  get <- function() x
  
  ## Assigns the inverse of the matrix to environment var m
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  ## Return a list containing the methods relevant to
  ## the modified matrix object we've created
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## The cacheSolve function runs a check on the
## passed object to see if it's inverse has been cached.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  ## If the inverse has already been solved and cached,
  ## return the cached version.
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If it hasn't, we solve() the matrix, then cache
  ## and return the result.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Create the matrix
## [ 1 0 5 ]
## [ 2 1 6 ]
## [ 3 4 0 ]
##
## Inverse is
## [ -24 20 -5 ]
## [ 18 -15  4 ]
## [ 5   -4  1 ]

a <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), ncol = 3, nrow = 3)

m1 <- makeCacheMatrix(a)

# Print the inverse of m1
print(cacheSolve(m1))

# Get cached version of m1 and print it
print(cacheSolve(m1))

## Now let's alter the matrix cached in m1
## First make a copy of a
b <- a

## Then change the [1, 1] value to 2 instead of 1
b[1, 1] <- 2

## Now we change the matrix in m1
m1$set(b)

## m1 has changed, so the inverse is no longer cached.
## It will be recalculated...
print(cacheSolve(m1))

## ..and cached
print(cacheSolve(m1))
