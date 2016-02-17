## When you multiply a Matrix by its Inverse you get the Identity Matrix 
## (which is like "1" for Matrices)It has 1s on the diagonal and 0s everywhere else.  
## assume your matrix is mat1 <- matrix(1:4, 2, 2)
## > mat1
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## So you can invert it by swap the positions of 1 and 4, put negatives in front of 3 and 2, 
## and divide everything by the determinant (1*4-3*2) 
## R can do that using ?solve function
## the result "manully calculated or using R code equal to 
## invertedMatrix <- solve(mat1)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## I <- So invertedMatrix %*% mat1
## I (Identity)
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## the following function create a Special Matrix  
## As a helper to encapsulate matrix opoerations
## we assuming this is a Square Matrix
makeCacheMatrix <- function(x = matrix()) {
        invertedMatrix <- NULL
        set <- function(value) {
                x <<- value
                invertedMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse){ invertedMatrix <<- inverse}
        getInverse <- function(){ invertedMatrix}
        list(set = set,get = get,setInverse = setInverse, getInverse = getInverse)
}


## used with the makeCacheMatrix to invert created MAtrix 
## and maintian chached value and  it
cacheSolve <- function(x, ...) {
        invertdMatrix <- x$getInverse()
        if (!is.null(invertdMatrix)) {
                message("getting cached inverted Matrix")
                ## already cached no need to invert it agian  !
                return(invertdMatrix)
        }
        ## if not cached, so cach it
        squareMatrix <- x$get()
        invertdMatrix <- solve(squareMatrix, ...)
        x$setInverse(invertdMatrix)
        invertdMatrix
}

## Test
## > mat1 <-  matrix(1:4, 2, 2)
## > helper <- makeCacheMatrix(mat1)
## > invertedMatrix <- cacheSolve(helper)
## > mat1 %*% invertedMatrix ## mulitibly the orginal Matrix by its invertion should result I
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## 
## cacheSolve(helper)
## getting cached inverted Matrix
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
