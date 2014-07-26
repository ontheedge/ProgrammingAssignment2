## The file contains a pair of functions that allows for speeding up (repeated)
## matrix inverse calculations.
##
## After sourcing the functions makeCacheMatrix() and cacheSolve() can be used
## to create a container for a matrix and lazily compute and store the inverse
## of the underlying matrix in such containers.
## ---------------------------------------------------------------------------

## Create a special container (a list) which manages a matrix and its inverse. 
## The container defines getters and setters for both the matrix and the 
## inverse.
## 
## It's advised (but not enforced) that clients only manipulate the matrix value
## through the container's `set()` element, and rely on cacheSolve() below for 
## inverse calculation / storage.
## 
## It's also possible to specify the starting matrix value as a parameter.
## 
## A less error-prone design would be to not expose the setter functions at all,
## and put the lazy-computation + caching logic into this function instead of a 
## separate one. But this would be incosistent with the design blueprint of the 
## original assignment. Arguably this immutability would force clients into 
## using more memory (resultung from more matrix copying and/or container 
## creation because inability to resuse them). Nevertheless it's an interesting 
## trade-off to consider (most of the time memory and cpu is cheaper than 
## exposing programmer errors).
## 
## Note that no error-handling is implemented here.
makeCacheMatrix <- function(data = matrix()) {
  inverse <- NULL

  set <- function(value) {
    data <<- value
    inverse <<- NULL
  }
  get <- function() {
    data
  }

  setinverse <- function(value) {
    inverse <<- value
  }
  getinverse <- function() {
    inverse
  }

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Lazily compute and store the inverse of a matrix inside the container created
## by makeCacheMatrix.
##
## No error handling implemented is implemented here.
## It's expected that the first argument is an object returned by
## makeCacheMatrix and the underlying value inside it is an object accepted by
## base.solve()
##
## The second and further arguments (if any) are passed unmodified to solve().
cacheSolve <- function(cachematrix, ...) {
  inverse <- cachematrix$getinverse()
  if(!is.null(inverse)) {
    return(inverse)
  }
  data <- cachematrix$get()
  inverse <- solve(data, ...)
  cachematrix$setinverse(inverse)
  inverse
}


## A rudimentary test-harness, both for functionality and speed-up of the caching mechanism.
## To use during development, uncomment the call to this function after its definition.
test <- function() {
  # matrix generator helper function (from the help page of solve())
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

  # testcase: CacheMatrix constructed with initial value
  cm <- makeCacheMatrix(hilbert(8))
  expected <- solve(hilbert(8))
  actual <- cacheSolve(cm)
  if(!identical(expected, actual)) {
    stop()
  }

  # testcase: CacheMatrix constructed with default value
  cm <- makeCacheMatrix()
  expected <- solve(matrix())
  actual <- cacheSolve(cm)
  if(!identical(expected, actual)) {
    stop()
  }

  # testcase: CacheMatrix constructed with initial value then changed
  cm <- makeCacheMatrix(hilbert(4))
  expected <- solve(hilbert(4))
  actual <- cacheSolve(cm)
  if(!identical(expected, actual)) {
    stop()
  }
  cm$set(hilbert(6))
  expected <- solve(hilbert(6))
  actual <- cacheSolve(cm)
  if(!identical(expected, actual)) {
    stop()
  }

  print("all functional tests ran OK")

  print("run timing for original solver")
  m <- hilbert(9)
  t <- c()
  for(j in 1:3) {
    t <- c(t, system.time(for(i in 1:10000) x <- solve(m))["elapsed"])
  }

  print("run timing for caching solver")
  cm <- makeCacheMatrix(hilbert(9))
  ct <- c()
  for(j in 1:3) {
    ct <- c(ct, system.time(for(i in 1:10000) x <- cacheSolve(cm))["elapsed"])
  }

  print("original times")
  print(t)

  print("cached times")
  print(ct)

  print(paste("original mean =", mean(t)))
  print(paste("cached mean = ", mean(ct)))
}

# test()
