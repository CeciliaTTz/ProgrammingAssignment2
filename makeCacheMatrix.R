makeCacheMatrix <- function(x = matrix()) { #create a matrix
  +     aaa <- NULL
  +     set <- function(y) {
    +         x <<- y
    +         aaa <<- NULL
    +     }
  +     get <- function() x  #get the matrix
  +     setinverse <- function(inverse) aaa <<- inverse #set the cached inverse
  +     getinverse <- function() aaa
  +     list(set = set, get = get,    #return the list
             +          setinverse = setinverse,
             +          getinverse = getinverse) #return the function list
  + }

#CacheSolve function
CacheSolve <- function(x, ...) {
  +     aaa <- x$getinverse()
  +     if (!is.null(aaa)) {
    +         message("getting cached data")
    +         return(aaa)
    +     }
  + cached_data <- x$get()
  + i <- solve(cached_data, ...)
  + x$setinverse(i) #set the inverse and return it
  + }
