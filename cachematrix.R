# The following two functions were written to complete HW Assignment 2 which caches the inverse of an invertible matrix.

# The function makeCacheMatrix() creates a special 'matrix' object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {                     # input x will be a matrix
        m <- NULL                                               # m will be the inverse matrix and set to NULL every time makeCacheMatrix is called
        set <- function(y) {                                    # takes an input matrix y
                x <<- y                                         # saves the input matrix using superassignment
                m <<- NULL                                      # resets the inverse matrix to NULL
        }
        get <- function() x                                     # returns the value of the original matrix
        setinverse <- function(solve) m <<- solve               # called by cacheSolve() during first cacheSolve()
        getinverse <- function() m                              # returns cached matrix to cacheSolve() on subsequent accesses
        list(set = set, get = get, 
             setinverse =setinverse,
             getinverse = getinverse)                           # accessed each time makeCacheMatrix called so a calling function knows how to access those methods
}


# The function cacheSolve() computes the inverse of the special 'matrix' returned by makeCacheMatrix().
# This function checks to see if the matrix has changed and if the inverse has already been calculated. 
# If the inverse has already been calculated, the function returns the inverse matrix that was cached.

cacheSolve <- function(x, ...) {                        # the input x is an object created by makeCacheMatrix
        m <- x$getinverse()                             # accesses the object 'x' and gets the inverse matrix
        if(!is.null(m)) {                               # checks to see if m has something in it, if it does, it returns that data
                message("getting cached data")
                return(m)
        }
        data <- x$get()                                 # this code returns if x$getinverse() returned NULL                              
        m <- solve(data, ...)                           # the inverse matrix is calculated
        x$setinverse(m)                                 # stores the calculated inverse matrix in x
        m                                               # returns the inverse matrix
}
