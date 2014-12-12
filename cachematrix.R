## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Variable to store the inverse of the matrix
    i <- NULL
    
    # Function to store matrix and reset reset the inverse of the matrix
    set <- function(y) {
        # Stores matrix in x
        x <<- y
        # Resets the inverse of the matrix that it's cached since it's not valid due to
        # original matrix have changed
        i <<- NULL
    }
    
    # Function to return matrix
    get <- function() x
    
    # Function to cache the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    # Function to retrieve the inverse of the matrix cached
    getinverse <- function() i
    
    # Returns a list with the 4 functions declared above
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# This function computes the inverse of the matrix 'x'. If the inverse has already been calculated
# then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Get inverse of the matrix from cache
    i <- x$getinverse()
    
    # Checks if there is actually an inverse of the matrix cached
    if (!is.null(i)) {
        # Since it's not NULL, there is a inverse of the matrix cached
        message("getting cached data")
        # Returning the inverse of the matrix cached
        return(i)
    }
    # If reached to this point, it means there is no inverse of the matrix cached,
    # so it gets the matrix to create the inverse of it
    data <- x$get()
    # Create the inverse of the matrix
    i <- solve(data)
    # It caches the inverse of the matrix
    x$setinverse(i)
    # Returns the inverse of the matrix
    i
}
