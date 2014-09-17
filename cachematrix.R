## creates a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
       
        setinverse <- function(solve){
                m <<- solve(solve) #%*% inverse
        } 
        getinverse <- function() m
        
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        

}


## computes the inverse of the matrix

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("Geting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...) #%*% data
        x$setinverse(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}






