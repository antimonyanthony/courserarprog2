## Because it can often be a waste of time and processing power to
## calculate values that have already been calculated, these functions
## make use of caching to avoid re-calculating known values - in this case,
## such values are the inverses of matrices.


## This function creates a list of sub-functions that relate to an argument
## "x", which is a matrix.  You can call these subfunctions to set a known
## value for the inverse of a given matrix, but if you don't known the
## inverse, the cacheSolve function will calculate it for you.
makeCacheMatrix <- function(x = matrix()) { ## You can give makeCacheMatrix
                                            ## an argument if you wish,
                                            ## but if not, the default
                                            ## argument is an empty matrix.
    inv <- NULL                             ## If inv, that is, the
                                            ## inverse of x, does not yet
                                            ## exist, this line gives inv 
                                            ## a value of NULL.
    set <- function(y) {                    ## set is a function that
                                            ## changes the value of x to
                                            ## a given input y, in case you
                                            ## left the argument of
                                            ## makeCacheMatrix blank.
                                            ## If you store makeCacheMatrix
                                            ## in a variable, this also
                                            ## allows you to modify x even
                                            ## if makeCacheMatrix was called
                                            ## with an argument in that
                                            ## variable assignment.
        x <<- y
        inv <<- NULL                        ## This line ensures that an
                                            ## inverse value for a previous
                                            ## value of x is not assigned to
                                            ## the new value of x.
    }
    get <- function() x
    setinve <- function(inve) inv <<- inve  ## If you know the inverse of
                                            ## a matrix without needing the
                                            ## computer to calculate it for
                                            ## you, you can call setinve with
                                            ## the known inverse as an
                                            ## argument, to set inv equal to
                                            ## that known inverse.
        getinve <- function() inv
        list(set = set, get = get,          ## This ensures that
                                            ## makeCacheMatrix returns a
                                            ## list of functions, so that if
                                            ## you store makeCacheMatrix in
                                            ## a variable, say, z, then
                                            ## you can call the subfunctions
                                            ## by calling z$set(yourmatrix),
                                            ## z$get(), etc.
             setinve = setinve,
             getinve = getinve)
}
## This function will retrieve known inverse values if they exist, and if
## they don't, it will calculate that inverse.

cacheSolve <- function(z, ...) {
        # cacheSolve takes as an argument a
                                        # variable in which makeCacheMatrix
                                        # has been stored, with a given
                                        # input matrix (which may have been
                                        # set after the fact by the set
                                        # function).
    inv <- z$getinve()                  # The value of the inverse of the
                                        # input matrix is retrieved and
                                        # stored as inv; this
                                        # value may be an actual matrix
                                        # or it may be NULL.
    if(!is.null(inv)) {                 # This part of the function checks
                                        # if inv is NULL (that is, if the
                                        # value of the inverse has not
                                        # already been set).  If not, the
                                        # braced code is executed.
        message("getting cached data")
        return(inv)                     # Since this inverse is known, the
                                        # function just returns it without
                                        # wasting any time on calculation.
    }
    dat <- z$get()                      # Since "return" ends the function,
                                        # this part is executed if inv is
                                        # NULL.  The value of the input
                                        # matrix is retrieved from z through
                                        # the get function, and stored as
                                        # dat.
    inv <- solve(dat, ...)              # Calculates the previously unknown
                                        # inverse of the input matrix.
    z$setinve(inv)                      # This line is necessary so that
                                        # z "knows" what the newly discovered
                                        # inverse of the input matrix is;
                                        # setinve is used to set the known
                                        # inverse value.  Thus, if cacheSolve
                                        # is called on the same z, it won't
                                        # need to recalculate.
    return(inv)                         # Finally, returns the inverse.
}