## makeCacheMatrix: This function creates
## a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.    

## makeCacheMatrix: This function creates
## a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL ## assigns NULL value to inv, where the inverted matrix will be stored in future
        set<-function(y){ ## this function replaces x by y and assigns NULL to inv
                x <<- y
                inv <<- NULL
        }
        get<-function()x ## this function returns X
        setinverse<-function(inverse) inv<<-inverse ## this function assigns inverse value to inv
        getinverse<-function() inv ## this function returns inv
        list(set = set, get = get, ## the entire function returns a list of all 4 above funtions
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) { ## receives a matrix creadted by the previous function
        inv <- x$getinverse() ## assigns to x the output of getinverse funtion out of the list x
        if(!is.null(inv)) { ## checks if the matrix was inverted and saved before
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ## if not assigns its value to data
        inv <- solve(data, ...) ## performs the inverse and assigns the result to inv
        x$setinverse(inv) ## calls the setinverse funtion
        inv ## returns inv
        
}
