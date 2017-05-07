## The first function takes a matrix vector as input and then creates a vector that saves the 
## 1. inverse of that matrix
## 2. returns inverse of the matrix
## 3. set the original matrix in the list
## 4. returns the matrix


## This function takes input of a matrix vector and creates a list that
##1. stores that matrix
##2. has a function to retrieve that matrix
##3. stores the inverse of that matrix
##4. retrieves the inverse of that matrix
## It also has a vector "m", depending upon its value, cacheSolve function finds out whether 
##the inverse is there in cache or not

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
                x <<- y
                i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function calculates the inverse, if it is not already calculated (if the matrix is not changed)
## If the inverse is calculated for first time, it also saves the invers in the list usfin the setinverse
## using the makeCacheMatrix setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
