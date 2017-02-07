## Since matrix inversion is a costly computation, this code caches the inverse of a matrix instead of computing it repeatedly.
## This pair of functions cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) i<<-solve
	getinverse<-function() i
	list(set=set, get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by the above function. If the inverse has already been calculated, then the function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	i<-x$getinverse()
	if(!is.null(i)){
		message("Getting cached data.")
		return(i)
	}
	data<-x$get()
	i<-solve(data,...)
	x$setinverse(i)
	i
}
