
# See README.md for instructions on how to run the code and derive output based on its implementation.

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 						# matrix not set yet, hence null here. Next step sets the value of matrix
        set <- function(y) { 
                x <<- y						#assigns a value to the matrix in an environment that is different from the current environment.
                m <<- NULL	
        }
        get <- function() x 					# return matrix as it is
        setmatrix <- function(solve) m <<- solve	# store(cache) matrix for future use
        getmatrix <- function() m				# return stored(cached) matrix
        list(set = set, get = get,				# list of instructions/methods with their values
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()					# check whether matrix is stored (cached)
        if(!is.null(m)) {					# if matrix in cache, return value and stop execution
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()					# this steps and following steps are implemented if m is null
        m <- solve(matrix, ...)				# calculates inverse
        x$setmatrix(m)						# write/store matrix to cache
        m
}
