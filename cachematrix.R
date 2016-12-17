# Assignment :- Caching the inverse of a matrix

## makeCacheMatrix - creates a special "matrix" object that caches the inverse of the matrix supplied
## cacheSolve - checks to see if inverse of matrix supplied exists and if so returns it otherwise calculates the inverse


### Steps for makeCacheMatrix are:
#### 1. initialise inverse matrix as NA values of same dimensions as x
#### 2. create function "set" to cache vales of initial matrix x and initital inverse matrix inv of NAs in the parent environment
####    when x is passed to makeCacheMatrix
#### 3. create function "get" to return  x
#### 4. create function "setinverse" to calculate inv using the solve function 
#### 5. create funtion "getinverse" to recall inverse matrix by name of inv 
#### 6. return a named list of the 4 functions  set, get, set inverse and get inverse 
#### 7. this list is then in the form that it can be passed to cacheSolve

makeCacheMatrix <- function(x = matrix()) {  
        inv <- matrix(NA,nrow(x),ncol(x))       
        
        set <- function(y) {                     
                 x <<- y
                 inv <<- matrix(NA,nrow(x),ncol(x))
        }
        
        get <- function() x                                
        setinverse <- function(solve) inv <<- solve        
        getinverse <- function() inv                       
        list(set = set, get = get,                         
             setinverse = setinverse,
             getinverse = getinverse)
        
        
        }
 

### Steps for cacheSolve are:
#### 1. takes list returned from make makeCacheMatrix called x 
#### 2  checks if inverse matrix exists in cache (if first time called by cacheSolve then inv is NA as it hasn't been calculated) 
####    if it does return it otherwise:
#### 3. create variable "data" which is using the globally defined function get() determined when makeCacheMatrix was run 
#### 4. calculate inverse of data using solve called "inv"
#### 5. store as inv in cache
#### 6. return "inv" which is the inverse 


cacheSolve <- function(x, ...) {
     
                inv <- x$getinverse()
                if(!is.na(inv[1,1])) {                                # check on first element only to stop warning message
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)  
                inv
        }
        
        


a <- matrix(c(4,2,7,6),2,2)
z <- matrix(c(1,2,7,6),2,2)

b <- makeCacheMatrix(a)
c<- makeCacheMatrix(z)
cacheSolve(b)
cacheSolve(c)
cacheSolve(b)
