## Intro To R - Programming Assignment 2: Lexical Scoping
## This set of functions returns the inverse of a square invertable matrix
## If the matrix has already been set and the inverse calculated, it will 
## return the cache the inverse matrix and return the cached value 
## rather than recalculating the matrix inverse every time.

## Written by Nathan L. Post
## Saturday APril 23, 2016

## Function to store matrix x and inverse matrix invX, and return
## these values when called accordingly.

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y){
        x <<-y
        invX<<-NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) invX <<- inverseMatrix
    getInverse <- function () invX
    list (set = set, get = get, setInverse = setInverse, 
          getInverse = getInverse)
}


## function calculates the inverse of a matrix x.  
## if the inverse of that matrix has already been calculated
## then it returns the previously calculated value.

cacheSolve <- function(x, ...) {
    ## check if inverse has already been calculated
    invX <- x$getInverse()
    if(!is.null(invX)){
        message("getting cached data")
        return(invX)
    }
    ## if not, calculate inverse and store
    matrixX <- x$get()
    message("calculating inverse matrix")
    invX <- solve(matrixX)
    x$setInverse(invX)
    invX    ## Return a matrix that is the inverse of 'x'
}
