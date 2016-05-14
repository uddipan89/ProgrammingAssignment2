## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	
	# the following are the functions used:
	# set_matrix: sets the value of the matrix
	# get_matrix: gets the value of the matrix
	# set_inverse: sets the value of the inverse of the matrix
	# get_matrix: gets the value of the inverse of the matrix
	
	inverse <- NULL
        set_matrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(inverse) inverse <<- inverse
        get_inverse <- function() inverse
        
        # the following creates a list of the functions
        list(set_matrix = set_matrix,
		get_matrix = get_matrix, 
		set_inverse = set_inverse,
		get_inverse = get_inverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$get_inverse()
	
	# the following block executes only if the inverse og the matrix is pre-calculated and cached
        if(!is.null(inverse)) {
                message("getting cached matrix")
                return(inverse)
        }
        
        matrx <- x$get_matrix()
        
        # slove(matrx) fetches the inverse of the matrix
        inverse <- solve(matrx)
        
        # setting the inverse of the matrix
        x$set_inverse(inverse)
        inverse
}
