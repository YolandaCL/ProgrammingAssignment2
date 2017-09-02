## makeCacheMatrix: 
## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}

## cacheSolve: 
## This function computes the inverse
## of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Sample run :
r1 = c(0,1,1)
r2 = c(1,0,0)
r3 = c(0,0,1)
x = rbind(r1,r2,r3)
x
##    [,1] [,2] [,3]
## r1   0    1    1
## r2   1    0    0
## r3   0    0    1
m = makeCacheMatrix(x)
m$get()
##    [,1] [,2] [,3]
## r1   0    1    1
## r2   1    0    0
## r3   0    0    1

## No cache in the first run
cacheSolve(m)
##       r1  r2  r3
## [1,]  0   1   0
## [2,]  1   0  -1
## [3,]  0   0   1

## Retrieving from the cache in the second run
cacheSolve(m)
## getting cached data
##       r1  r2  r3
## [1,]  0   1   0
## [2,]  1   0  -1
## [3,]  0   0   1

