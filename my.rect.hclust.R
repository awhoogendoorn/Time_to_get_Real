my.rect.hclust <- function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2, 
          cluster = NULL) 
{
  if (length(h) > 1L | length(k) > 1L) 
    stop("'k' and 'h' must be a scalar")
  if (!is.null(h)) {
    if (!is.null(k)) 
      stop("specify exactly one of 'k' and 'h'")
    k <- min(which(rev(tree$height) < h))
    k <- max(k, 2)
  }
  else if (is.null(k)) 
    stop("specify exactly one of 'k' and 'h'")
  if (k < 2 | k > length(tree$height)) 
    stop(gettextf("k must be between 2 and %d", length(tree$height)), 
         domain = NA)
  if (is.null(cluster)) 
    cluster <- cutree(tree, k = k)
  clustab <- table(cluster)[unique(cluster[tree$order])]
  m <- c(0, cumsum(clustab))
  if (!is.null(x)) {
    if (!is.null(which)) 
      stop("specify exactly one of 'which' and 'x'")
    which <- x
    for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
  }
  else if (is.null(which)) 
    which <- 1L:k
  if (any(which > k)) 
    stop(gettextf("all elements of 'which' must be between 1 and %d", 
                  k), domain = NA)
  border <- rep_len(border, length(which))
  retval <- list()
  for (n in seq_along(which)) {
    rect(m[which[n]] + 0.66, par("usr")[3L], 
         m[which[n] + 1] + 0.33, mean(rev(tree$height)[(k - 1):k]), 
         border = border[n], lwd = 3, col = "#3e525b", density = 0, angle = 0)
    retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
  }
  invisible(retval)
}




rect.hclust.labels <-
  function (tree,
            k = NULL,
            which = NULL,
            x = NULL,
            h = NULL,
            border = 2,
            cluster = NULL,
            labels = 1:k) {
    # begin code from rect.hclust
    if (length(h) > 1 | length(k) > 1)
      stop("'k' and 'h' must be a scalar")
    if (!is.null(h)) {
      if (!is.null(k))
        stop("specify exactly one of 'k' and 'h'")
      k <- min(which(rev(tree$height) < h))
      k <- max(k, 2)
    }
    else if (is.null(k))
      stop("specify exactly one of 'k' and 'h'")
    if (k < 2 | k > length(tree$height))
      stop(gettextf("k must be between 2 and %d", length(tree$height)),
           domain = NA)
    if (is.null(cluster))
      cluster <- cutree(tree, k = k)
    clustab <- table(cluster)[unique(cluster[tree$order])]
    m <- c(0, cumsum(clustab))
    if (!is.null(x)) {
      if (!is.null(which))
        stop("specify exactly one of 'which' and 'x'")
      which <- x
      for (n in 1L:length(x))
        which[n] <- max(which(m < x[n]))
    }
    else if (is.null(which))
      which <- 1L:k
    
    if (any(which > k))
      stop(gettextf("all elements of 'which' must be between 1 and %d", k),
           domain = NA)
    # end code from rect.hclust
    
    n <- length(which)
    xpos <-
      rowMeans(cbind(m[which[1:n]] + 0.66,    m[which[1:n] + 1] + 0.33))
    ypos <- rep(par("usr")[3L], n)
    text(
      xpos,
      ypos,
      labels[n:1],
      adj = c(0, 0),
      pos = 3,
      col = "red",
      cex = 0.75
    )
  }
