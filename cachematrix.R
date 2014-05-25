## makeCacheMatrix: makes a list of functions which 
## use interact with this function's scope's variables
## 
## use set() to store the matrix to be used
##
## example: a <- makeCacheMatrix()
##          a$set(matrix(c(2,3,3,2),2,2))

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #variable for the inverse of the matrix
    
    #Set the matrix using a given matrix y
    set <- function(y) {
        x <<- y
        i <<- NULL #reset the cache for a new setting
    }
    
    #get the matrix
    get <- function() x
    
    #sets the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    #gets the inverse of the matrix
    getinverse <- function () i
    
    #return this list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 

}



## This solves a CacheMatrix's inverse, attempting to find
## a cached solution first
##
## example: cacheSolve(a)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #check cache
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i) #return cache, don't need to solve again
    }
    
    #solve
    data <- x$get()
    i <- solve(data, ...)
    
    #save into cache
    x$setinverse(i)
    
    #Return a matrix that is the inverse of 'x'
    i
}
