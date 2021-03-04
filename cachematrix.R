## The first function, makeVector creates a special "vector", which is really a list containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	# Cache value
	cached_matrix <- NULL
	
        # set function
        set <- function(y) {
                x <<- y
                cached_matrix <<- NULL
        }

        # get function
        get <- function() x

        # set inverse matrix to cache
        setInverse <- function(inverse) cache <<- inverse
        
	# get the inverted matrix from cache
        getInverse <- function() cache

        # return the created functions to the working environment
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
}

## The following function calculates the inverse of the matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
	
	# Try to check in cache first
	cached_matrix <- x$getInverse()
        if(!is.null(cached_matrix)) {
                message("getting cached data")
                return(cached_matrix)
        }
	
        
	# Calculate inverse of matrix
        matrix <- x$get()
	cached_matrix <- solve(matrix, ...)
	
	# Store to cache
        x$setInverse(cached_matrix)

        return(cached_matrix)
}
