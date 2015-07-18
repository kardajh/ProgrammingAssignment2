## A pair of functions, one to make a "matrix object' which can store 
## itself plus its inverse. The second function will calcualte the inverse
## but will first check the stored inverse and avoid re-calculating the same 
## inverse again.

## A structure to store a numeric matrix and its inverse. Initialised 
## with just the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    # x - reference to original matrix 
    # minv - reference to matrix inverse
    minv <- NULL
    
    # Resets the original matrix, uses <<- to update the m, minv var in parent function
    set <- function (moriginal) {
        x <<- moriginal
        minv <<- NULL
    }
    
    # Gets the original matrix
    get <- function() x
    
    # Sets the inverse matrix here for storing, use <<- to update the minv variable in parent function
    setInverse <- function(theInverse) {
        minv <<- theInverse
    }
    
    # Gets the matrix inverse
    getInverse <- function() minv 
    
    # Returns all these functions in a handy list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of the matrix x, where x is of the form "makeCacheMatrix"
## Will return a cached value if possible. 

cacheSolve <- function(x, ...) {
    # x - an object of form "makeCacheMatrix", containing at least a numeric matrix. 
    # inv - the inverse of the matrix inside x
    inv <- x$getInverse()
    
    # Returns inverse immediately if its cached 
    if (!is.null(inv)) {
        message ("Returning cached inverse")
        return (inv)
    }
    
    # Otherwise computes the inverse, store it for next time and return it.
    mat_data <- x$get()
    message("Computing inverse...")
    inv <- solve(mat_data, ...) 
    x$setInverse(inv)
    inv
}
