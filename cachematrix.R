## R script contains functions to cache the inverse of a requested matrix

## Function to creates matrix object to cache inverse of matrix

makeCacheMatrix <- function( mtx = matrix() ) {

## initialize inverse Matrix
invMtx <- NULL

## setter for  matrix
set <- function( matrix ) {
mtx <<- matrix
invMtx <<- NULL
}

## getter for matrix
get <- function() {
mtx
}

## setter for inverse  matrix
setInverse <- function(inverse) {
invMtx <<- inverse
}

## getter for inverse Matrix
getInverse <- function() {
## return Inverse matrix
invMtx
}

## Methods list to return
list(set = set, get = get,
setInverse = setInverse,
getInverse = getInverse)
}

## Function cacheSolve to compute and return inverse of a matrix returned by function "makeCacheMatrix"
## If inverse matrix already exist then return cached using getinvere from above function
## Else compute and set and return .

cacheSolve <- function(x, ...) {
## Return inverse matrix of x
mtx <- x$getInverse()
## returm cached inverse matrix data if already exist
if( !is.null(mtx) ) {
message("getting cached data")
return(mtx)
}
## Get the matrix from our object
data <- x$get()
##  matrix multiplication to calculate inverse matrix
mtx <- solve(data) %*% data
## Set inverse matrix
x$setInverse(mtx)
## Return the matrix
mtx
}
