## Put comments here that give an overall description of what your
## functions do
	##              1. This set the matrix
        ##              2. This get the matrix
        ##              3. This set the inverse
        ##              4. This get the inverse
        ##         	Finally this list is used as the input to cacheSolve()


makeCacheMatrix <- function(x = matrix()) {

        dtInv <- NULL

        set <- function(y) {

                x <<- y

                dtInv <<- NULL

        }

        get <- function() x

        setInverse <- function(inverse) dtInv <<- inverse

        getInverse <- function() dtInv

        list(set = set,

             get = get,

             setInverse = setInverse,

             getInverse = getInverse)

}

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        dtInv <- x$getInverse()

        if (!is.null(dtInv)) {

                message("getting cached data")

                return(dtInv)

        }

        dtMatrix <- x$get()

        dtInv <- solve(dtMatrix, ...)

        x$setInverse(dtInv)

        dtInv

}
