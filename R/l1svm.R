# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

l1svm<-function(data, target, cost=1, epsilon=0.1, bias=1, verbose=FALSE, ...) {
  input_p <- preprocess_input(data, target)
  xs = cbind(input_p$data, bias)
  y = input_p$yc
  l = input_p$n
  p = input_p$p
  alpha <- rep(0,l)
  w <- c(rep(0,p),0)
  U = cost
  Qii <- y * y * sapply(1:l, function(i) xs[i,]%*%xs[i,])
  for (k in 1:1000) {
    prev_alpha <- alpha
    for(i in 1:l) {
      G <- y[i] * w %*% xs[i,] - 1
      PG <- G

      if (alpha[i] == 0) {
        PG <- min(G,0)
      }

      if (alpha[i] == U) {
        PG <- max(G,0)
      }

      if (PG != 0) {
        old_alpha <- alpha[i]
        alpha[i] <- min(max(alpha[i]-G/Qii[i],0),U)
        w <- w + (alpha[i] - old_alpha)*y[i]*xs[i,]
      }
    }
    diff <- prev_alpha - alpha
    if(sqrt(diff %*% diff) < epsilon) break

  }
  w
}

preprocess_input <- function(data, target) {
  if (!inherits(data, "matrix") && !inherits(data, "matrix.csr")) stop("Argument data is not a matrix!")

  if (sparse <- inherits(data, "matrix.csr")){
    if(requireNamespace("SparseM",quietly=TRUE)){
      # trying to handle the sparse martix case
      data = SparseM::t(SparseM::t(data)) # make sure column index are sorted
      n = data@dimension[1]
      p = data@dimension[2]
    } else {
      stop("newx inherits from 'matrix.csr', but 'SparseM' package is not available. Cannot proceed further. Use non-sparse matrix or install SparseM.")
    }
  } else {
    # Nb samples
    n=dim(data)[1]
    # Nb features
    p=dim(data)[2]
  }

  if (n != length(target)) stop("Argument data has different row nr than length of target argument!")

  if(is.character(target))
    target = factor(target)

  # targetlabels are sorted by first occurrence in target ; if target is a factor, targetlabels too, with the same levels.
  targetlabels = unique(target)
  nbclass = length(targetlabels)

  yc = as.integer(ifelse(target == target[1], 1, -1))

  if (nbclass!=2)
    stop("wrong number of classes ( < 2 ).")

  list(data=data, n=n, p=p, yc=yc)
}
