## Creates an object containing a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv_ <- NULL  # Initially store NULL in the inverse cache
    # This function sets the matrix values and resets the inverse cache to null
    set <- function(y) {
      x <<- y
      inv_ <<- NULL
    }
    # This function returns the matrix object
    get <- function() x
    # This function stores the calculated inverse in the cache
    setInv <- function(inv) inv_ <<- inv
    # This function gets the inverse from the cache
    getInv <- function() inv_
    # This list object is returned
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Inverts the cacheable matrix, using the cached value if available to save time
cacheSolve <- function(x, ...) {
  # Get the value of the inverse cache
  inv <- x$getInv()
  # If the cache is not empty, return the cache value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # The cache was empty. Get the data and perform the inverse
  data <- x$get()
  inv <- solve(data, ...)
  # Store the inverse in the cache for future use
  x$setInv(inv)
  # Return the inverse
  inv
}
