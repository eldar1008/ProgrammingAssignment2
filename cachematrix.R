## These two functions are used to cache the inverse of a given matrix so that the operation does not need to be performed again.
## This is because the inverse of a matrix; especially a large matrix is a very expensive operation. It is important to note that a new inverse is calculated if the current matrix is changed

## makeCacheMatrix takes the argument of a matrix and also contains the following functions get, set, getinverse and setinverse 
## 	The get function displays the current matrix   
## 	The set function allows for the matrix passed into it to become the current matrix; this function is practically equivalent to makeCacheMatrix(currentmatrix)   
## 	The getInverse function displays the current inverse matrix     
## 	The setInverse function stores the inverse matrix. This is the true storing of the inverse matrix into the cache, it is called by cacheSolve although it could be called by the user   

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## cacheSolve works in conjunction with makeCacheMatrix; particularly by using the getInverse and setInverse functions.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("the inverse matrix is stored in the cache; retrieve it now")
		return(inv)
	}	
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}

##test data

## mdat <- matrix(c(1,8, 1,7), nrow = 2, ncol = 2, byrow = TRUE,
##               dimnames = list(c("row1", "row2"),
##                               c("C.1", "C.2")))
## mdat

## 
## cm <- makeCacheMatrix(mdat)
## cacheSolve(cm)
## cm$get()



