## Developer Dilfiruz Emiral
## Week 3 Assignment
## This function creates a special "matrix" object that can cache its inverse
# [functions] #
# [makeCacheMatrix] creates a matrix whose inverse form can be cached
# [getinverse] --> returns the inverse of matrix
# [setinverse] --> sets the inverse of matrix
# [get] --> returns the original matrix
# [set] --> sets the original matrix
# [functions] #

makeCacheMatrix <- function(w = matrix()) { ## define the argument with default mode of "matrix"
    matinv <- NULL                             ## initialize matinv as NULL; will hold value of matrix matinverse 
    set <- function(y) {                    ## define the set function to assign new 
        w <<- y                             ## value of matrix in parent environment
        matinv <<- NULL                        ## if there is a new matrix, reset matinv to NULL
    }
    get <- function() w                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) matinv <<- inverse  ## assigns value of matinv in parent environment
    getinverse <- function() matinv                     ## gets the value of matinv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(w, ...) {
        ## Return a matrix that is the inverse of 'x'
    matinv <- w$getinverse()
    if(!is.null(matinv)) {
        message("Second call - getting cached data")
        return(matinv)
    }
    data <- w$get()
    matinv <- solve(data, ...)
    w$setinverse(matinv)
    matinv
}



# Run Time Example
# 
#wMatrix <- matrix( c(2, 5, 8, 10, 9, 7, 6, 1, 5,12,11,1,1,2,3,4), nrow=4,  ncol=4) 
#> wMatrix
#      [,1] [,2] [,3] [,4]
#[1,]    2    9    5    1
#[2,]    5    7   12    2
#[3,]    8    6   11    3
#[4,]   10    1    1    4
#> mycache <- makeCacheMatrix(wMatrix)
#> cacheSolve(mycache)
#           [,1]       [,2]       [,3]         [,4]
#[1,]  0.1968504 -2.0393701  2.1968504 -0.677165354
#[2,]  0.1811024 -0.2362205  0.1811024 -0.062992126
#[3,] -0.1023622  0.2204724 -0.1023622 -0.007874016
#[4,] -0.5118110  5.1023622 -5.5118110  1.960629921

# When we call the function again, it brings data from cach and it prints "second call"

#> cacheSolve(mycache)
#Second call - getting cached data
#           [,1]       [,2]       [,3]         [,4]
#[1,]  0.1968504 -2.0393701  2.1968504 -0.677165354
#[2,]  0.1811024 -0.2362205  0.1811024 -0.062992126
#[3,] -0.1023622  0.2204724 -0.1023622 -0.007874016
#[4,] -0.5118110  5.1023622 -5.5118110  1.960629921
