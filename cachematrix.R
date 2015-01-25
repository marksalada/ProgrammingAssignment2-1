## The first function creates a chache-able matrix inverse function
## and the second function is the matrix inverse function itself

## Returns a list of functions to enable caching of a matrix inverse
##
## 1) set the matrix
## 2) get the matrix
## 3) set the value of the matrix inverse
## 4) get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set<-function(y){
		x <<- y
		invx <<- NULL
	}
	get<-function() x
	setinv<-function(inv) invx <<- inv
	getinv <- function() invx
	list(set = set, get = get,
		setinv = setinv, getinv = getinv)
}


## Calculates the inverse of a matrix if it is not already cached

cacheSolve <- function(x, ...) {

	## check for bad entry
	if(!is.list(x)){		
		message("entry bad or not cacheable: use makeCacheMatrix() first")
		return(NULL)
	}
	if(is.null(x[["getinv"]])){
		message("missing method: use makeCacheMatrix() first")
		return(NULL)
	}

	## grab and use if the inv is cached already
	invx <- x$getinv()
	if(!is.null(invx)){
		message("getting cached data")
		return(invx)
	}

	## inv wasn't cached, so grab matrix
	data <- x$get()

	## calculate inverse; this would be the place to 
	## check if the matrix is invertable
	## (e.g., abs(det(x)) > threshold)
	invx <- solve(data)
	
	## cache the result
	x$setinv(invx)

     	## Return a matrix that is the inverse of 'x'
	invx
}
