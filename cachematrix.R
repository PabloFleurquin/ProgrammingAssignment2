## Put comments here that give an overall description of what your
## functions do

# DESCRIPTION:
# These two functions combined save time for hard computational tasks.
# In this case calculating the inverse matrix could happen to last
# more time than desirable, specially when this operation is done
# recursively. To avoid unnecesary operations when the object (in this
# case a matrix) does not change, it makes sense to store away for 
# future use (cache) the object and retrieve it from memory when needed.

## Write a short comment describing this function

"makeCacheMatrix() takes as a single argument an empty matrix. In this 
case it sets m (the cache object) to NULL. It is important to understand 
that the operator <<- refers to variables or objects defined  in the 
'parent' context (environment). if we were to define setinv with <- it 
will only change m's value inside setinv and not in its parent context. 
The function returns a list object that contains an empty matrix, has an 
internal variable m which is set to NULL, and has four subfunctions that 
can be called to work on it (set(), get(), setinv(), and getinv()).
"

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

"
The function cacheSolve takes the input argument (that eventually 
could be an object of any type but in our case is a list of functions) 
and assigns the getinv() value to m. Next step, checks if m is a null 
object (empty). If this is not the case it returns the data stored in 
m (remember - 'DESCRIPTION:..to avoid unnecessary calculations...').
If m is null, it goes to x$get() line, assign the value to data and then 
calculates the inverse matrix with the solve function. Also it passes m 
to the function setinv(), that because it is defined using the '<<-' 
operator it can modify the input argument used globally. Finally it 
returns the inverse matrix as a result.
"

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m   
}
