## Put comments here that give an overall description of 
## what your functions do

## makeCacheMatrix: function creates a matrix x that can 
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL      #inverse as NULL
      set <- function(y){    #set value
            x <<- y
            inv <<- NULL
      }
      get <- function() x    #get value matrix x
      setmean <- function(mean) m <<- mean   #set mean
      getmean <- function() m                #get mean
      list(set = set, get = get,
           setmean = seatmean,
           getmean = getmean)
}

## cacheSolve: function computes inverse of matrix x 
## (returned by makeCacheMatrix)
## if: inverse has been calculated, then cache solve gets 
## inverse form cache.
cacheSolve <- function(x, ...) {     #get cache data
      inv <- x$getInverse()
      if(!is.null(inv)){      #if inverse is NULL
            message("getting cached data")
            return(inv)       #returns inverse value
      }
      mat <- x$get()
      inv <- solve(mat, ...)   #calculates inverse value 
      x$setInverse(inv)
      inv     #retun a matrix inverse of x
}
