## This function creates a special "matrix"
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setInversa <- function(inversa) inversa<<- inversa
        getInversa <- function() inversa
        list(set = set,
             get = get,
             setInversa = setInversa,
             getInversa = getInversa)
}


## This function returns the inverse of the special "matrix" created by
## makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        inversa <- x$getInversa()
        if (!is.null(inversa)) {
                message("obteniendo datos en caché")
                return(inversa)
        }
        matriz <- x$get()
        inversa <- solve(matriz, ...)
        x$setInversa(inversa)
        inversa
}


