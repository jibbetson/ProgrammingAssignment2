## Pair of interactive functions to calculate and cache the inverse of a matrix
# using solve() while exploring the scoping rules of R

# Function derives a special "matrix" object from an input matrix 'x'
# returns a list of 4 functions that enable its inverse to be cached and retrieved 
makeCacheMatrix <- function(x = matrix()){
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function(){
        x
    }
    setinverse <-function(Minverse){
        inverse <<- Minverse 
    }
    getinverse <- function(){
        inverse
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Function computes the inverse of the special "matrix" object returned by makeCacheMatrix
# If the inverse has already been calculated and the data has not changed then 
# cacheSolve retrieves the inverse directly from the cache. Otherwise the new 
# inverse is calculated and returned
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()   #check if the inverse has already been calculated
    if(!is.null(inverse)) {     #if it has then its value will not be 'null' 
        message("getting cached data")
        return(inverse)         #in which case return the cached inverse and exit
    }
    data <- x$get()             #get the matrix contents
    inverse <- solve(data, ...) #calculate its inverse
    x$setinverse(inverse)       #cache the inverse
    return(inverse)             #return the inverse
}