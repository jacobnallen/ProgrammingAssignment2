## Put comments here that give an overall description of what your
## functions do

## this matrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## defining the argument
        inv <- NULL ## setting inverse of matrix to NULL
        set <- function(y){ ## setting value of matrix with another function
                x <<- y 
                inv <<- NULL ## if there is a new matrix, reset inv to NULL
        } 
        get <- function() {x} ## defining the get function
        setInverse <- function(inverse) {inv <<- inverse} # assigns the value of inv in parent environment
        getInverse <- function() {inv} ## gets the value of inv where called
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## listing so you can refer to functions with $ operator
}

## Write a short comment describing this function
## this function finds the inverse of the matrix returned by the makeCacheMatrix function above
## if the inverse has already been calculated and the matrix is unchanged, CacheSolve will grab the inverse from the cache

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("geting cached data ")
                return(inv)
        }
        mat <- x$get ()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
