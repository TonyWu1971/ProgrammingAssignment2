## The makeCacheMatrix function is to 
## 1. set the value of the matrix, 
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get <-function() x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set=set, get =get, setinverse=setinverse, getinverse = getinverse)
        
}


## The cacheSolve function is to calculate the inverse matrix of the special "matrix" with the above function.
## It first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation
## Otherwise, it calculates the inverse matrix and sets teh value of the inverse matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

        ## Return a matrix that is the inverse of 'x'
}
