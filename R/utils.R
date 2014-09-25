
#' @title Is Matrix Element in Sequence by Row
#' @param from vector of starting positions
#' @param to vector of finishing positions
#' @param dim length 2 numeric vector defining number of rows and columns of comparison matrix
#' @return logical matrix with dimensions dim
#' @examples is.seq.mat(from = 1:3, to = 4:6, dim = c(3, 6))

is.seq.mat <- function(from = 1, to = 1, dim = c(1, 1)) {
    
    if (length(from) != dim[1]) {
        if (length(from) != 1) { stop("from must be length one or length nrow") }
        from <- rep(from, dim[1])
    }
    
    if (any(c(from, to) < 1 | c(from, to) > dim[2])) { stop("from and to should be between 1 and ", dim[2]) } 
    
    if (length(to) != dim[1]) {
        if (length(to) != 1) { stop("to must be length one or length nrow") }
        to <- rep(to, dim[1])
    }
    
    out <- matrix(FALSE, nrow = dim[1], ncol = dim[2])
    
    for (i in seq_len(dim[1])) {
        
        out[i, ] <- seq_len(dim[2]) %in% seq.int(from = from[i], to = to[i])
    }
    
    return(out)
}



#' @title Select Rows from Matrix by Sequences
#' @param mat matrix from which to select
#' @param from vector of starting positions
#' @param to vector of finishing positions
#' @param pad value with which to pad short rows (default NA)
#' @examples 
#'     ma <- matrix(letters[1:18], nrow = 3, ncol = 6, byrow = TRUE)
#'     selectSequences(mat = ma, from = 1:3, to = 4:6)
#'     selectSequences(mat = ma, from = 1:3, to = 4)

selectSequences <- function(mat, from = 1, to = 1, pad = NA) {
    
    issm <- is.seq.mat(from = from, to = to, dim = dim(mat))
    
    nvals <- apply(issm, 1, sum)
    
    out <- matrix(pad[1], nrow = nrow(mat), ncol = max(nvals))
    
    for (i in seq_len(nrow(mat))) { out[i, seq_len(nvals[i])] <- mat[i, issm[i, ]] }
    
    return(out)
}
