## Programming Assignment 2: Lexical Scoping
## This file contains two functions that are created to be able to cache the invers of a matrix, to save computing 
## time.
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. It is really a list.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## Declaring the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
       #Initialising the inverse variable as NULL (since it is not computed yet)
        inv <- NULL
        #Creating the set function for saving the input matrix as x, and making sure saving a new/changed 
        #matrix also resets the inverse variable to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #Creating the get function for retrieving the input matrix.
        get <- function() x
        #Creating the set inverse function for storing the inverse matrix to the inv variable
        setInv <- function(inv) inv <<- inv
        #Creating the get inverse function for getting the stored inverse matrix (or empty object, if not 
        # yet calculated) 
        getInv <- function() inv
        #Creating the list of functions that makes up this special "matrix". 
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Declaring the cacheSolve function for calculating/retrieving and returning the inverse matrix

cacheSolve <- function(x, ...) {
        #Getting the cached inverse matrix object
        inv <- x$getInv()
        #Checking if the stored inverse matrix object is NULL or already calculated.
        if(!is.null(inv)) {
                message("Getting inverse matrix from cache")
                #Returning inverse matrix if it is cached.
                return(inv)
        }
        #If the inverse matrix is not calculated (NULL), get the input matrix.
        data <- x$get()
        #Calculate the inverse matrix
        inv <- solve(data, ...)
        #Store the inverse matrix in the list created by the makeCacheMatrix function.
        x$setInv(inv)
        #Return inverse matrix. 
        inv
}
