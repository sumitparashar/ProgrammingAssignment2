makeCacheMatrix <- function(x = matrix()) {
	invr <- NULL
	set <- function(y){
		x<<-y
		invr<<-NULL
	}
	get<-function() x
	setinv<-function(inverse) invr<<-inverse
	getinv<-function() invr
	list(set=set,get=get,setinv=setinv,getinv=getinv)	
}


## Function written above is returning the list of function which are required in function given below. It will take the value NULL if its not.

cacheSolve <- function(x, ...) {
      invr<-x$getinv()
	if(!is.null(invr)){
		message("getting cached data")
		return(invr)
	}
	data<-x$get()
	invr<-solve(data,...)
	x$setinv(invr)
	invr
}

## This function is checking whether inverse of matrix is already computed or not. 
## If not, it will compute the inverse and if it is already computed, then is will display a message and return the inverse matrix.
