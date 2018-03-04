## Put comments here that give an overall description of what your
## functions 


#This is hard to grasp without additonal reading. This is basically a function that 
#returns a set of functions, with the nested function(s) being supplied information 
#based on the parent enironment in which they were created. I hope this isn't a typical 
#process one would need to grapple with in most R applications.... 

c<- matrix(c(1,2,3,4), nrow = 2, ncol = 2)


makeCacheMatrix <- function(x = matrix()) {
  c_inv <- NULL
  set <- function(y) {
    x <<- y
    c_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) c_inv <<- inverse
  getinverse <- function() c_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

p <- makeCacheMatrix(c)
p$get()

## Write a short comment describing this function
#So, this guy takes functions from MakeCacheMatric, grabs the solved inverse that's
#already created, and if there isn't one calculates it from scratch. Right? 

cacheSolve <- function(x, ...) {
  c_inv <- x$getinverse()
  if (!is.null(c_inv)) {
    message("getting cached data")
    return(c_inv)
  }
  data <- x$get()
  c_inv <- solve(data, ...)
  x$setinverse(c_inv)
  c_inv
}

cacheSolve(p)


#Playing with examples


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

p<-(1:10)

t <- function () p

globalenv()
search()
ls(.GlobalEnv, all.names =TRUE)

ls.str(.GlobalEnv)
exists("get", envir = .GlobalEnv)


