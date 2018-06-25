## This function makes a cache of a matrix
makeCacheMatrix <- function(x = matrix()) {
   #function to set value of matrix
    setval <- function(y) { 
    x <<- y    
     }
  #This Function gets the value of x
  getval <- function() x
  setinv <- function(inverse) 
     {inv <<- solve(x)}
   getinv <- function() inv
   # I copied these over from the example and don't really know why
  list( setval = setval, getval =getval,
        setinv = setinv, getinv = getinv)
   }

cacheSolve <- function(x) {
  inv <- x$getinv() 
  # Check to see if cache containes an inverse
  if(!is.null(inv)) {
        return(inv)
  } else{
  # if cache does not have inverse this will calculate it.
  temp <- x$getval()  
  inv <- solve(temp) 
  x$setinv(inv)  
  inv  
}
}
