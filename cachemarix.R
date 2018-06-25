## This function makes a cache of a matrix
makeCacheMatrix <- function(x = matrix()) {
#this function returns
    setval <- function(y) { 
    x <<- y    
     }
  #This Function gets the value of x
  getval <- function() x
  setInv <- function(inverse) 
     {inv <<- solve(x)}
   getInv <- function() inv
  list( setval = setval, getval =getval,
        setinv = setinv, getinv = getinv)
   }

cacheSolve <- function(x) {
  inv <- x$getinv() 
  if(!is.null(inv)) { # If the cache was not empty, we can just return it
    message("getting cached data")
    return(inv)
  }
  # The cache was empty. We need to calculate it, cache it, and then return it.
  data <- x$get()  # Get value of matrix
  m <- solve(data) # Calculate inverse
  x$setInverse(m)  # Cache the result
  m                # Return the inverse
}

