### is a an ancestor of b in graph g?
#fff <- memoize(is.ancestor)
is.ancestor <- function(a, b, g) {
    if (a == b)
        return(TRUE) ## fix 7/26... if x=y, x is an ancestor of y
    foundpath <- c()
    ind1 <- which(g == 3, arr.ind = TRUE, useNames = FALSE)
    ind1 <-
        subset(ind1, ind1[, 2] == a) # array of indices for tails out of A
    if (nrow(ind1) == 0)
        return(FALSE) # addition: if there are no tails at A
    for (x in 1:nrow(ind1)) {
        # loop through tails out of A
        if (g[ind1[x, 2], ind1[x, 1]] == 2) {
            # if there is an arrowhead at the other end of the x-th tail (call this C)
            if (ind1[x, 1] == b) {
                foundpath <- append(foundpath, TRUE)
                break
            }
            if (any(g[, ind1[x, 1]] == 3)) {
                # if there are any tails out of C, i.e., A-->C--*
                a_old <- a
                a2 <- ind1[x, 1]
                if (a2 == a_old)
                    next
                foundpath <- append(foundpath, is.ancestor(a2, b, g))
                if (any(foundpath) == TRUE)
                    break
            }
        } # if there isn't an arrowhead at C - !(A-->C) - don't return anything
    } # for x in 1:nrow(ind1)
    if (any(foundpath) == TRUE)
        return(TRUE)
    else
        return(FALSE)
} # end function

### is a an ancestor of b in graph g?
#fff.2 <- memoize(is.poss.ancestor)
is.poss.ancestor <- function(a, b, g, visited = NULL) {
    if (a == b)
        return(TRUE)
    foundpath <- c()
    ind1 <- which(g == 3, arr.ind = TRUE, useNames = FALSE) #tails
    ind11 <- which(g == 1, arr.ind = TRUE, useNames = FALSE) #circles
    ind1 <- rbind(ind1, ind11) ## tails and circles
    ind1 <-
        subset(ind1, ind1[, 2] == a) # array of indices for tails and circles out of A
    if (nrow(ind1) == 0)
        return(FALSE) # addition: if there are no tails or circles at A
    for (x in 1:nrow(ind1)) {
        # loop through tails and circles out of A
        if (ind1[x, 1] %in% visited)
            next
        if (g[ind1[x, 2], ind1[x, 1]] == 2 ||
            g[ind1[x, 2], ind1[x, 1]] == 1) {
            # if there is an arrowhead or circle at the other end of the x-th tail (call this C)
            if (ind1[x, 1] == b) {
                foundpath <- append(foundpath, TRUE)
                break
            }
            if (any(g[, ind1[x, 1]] == 3 |
                    g[, ind1[x, 1]] == 1)) {
                # if there are any tails or circles out of C
                a_old <- a
                a2 <- ind1[x, 1]
                if (a2 == a_old)
                    next
                foundpath <-
                    append(foundpath,
                           is.poss.ancestor(a2, b, g, visited = c(visited, a_old)))
                if (any(foundpath) == TRUE)
                    break
            }
        } # if there isn't an arrowhead at C - !(A-->C) - don't return anything
    } # for x in 1:nrow(ind1)
    if (any(foundpath) == TRUE)
        return(TRUE)
    else
        return(FALSE)
} # end function
