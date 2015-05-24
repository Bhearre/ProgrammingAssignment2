cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.nul(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}