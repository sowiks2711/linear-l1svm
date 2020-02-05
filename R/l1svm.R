# l1svm
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# data
# a nxp data matrix. Each row stands for an example (sample, point) and each column stands for a dimension (feature, variable).
#
# target
# a response vector for prediction tasks with one value for each of the n rows of data. For classification, the values correspond to class labels and can be a 1xn matrix, a simple vector or a factor. For regression, the values correspond to the values to predict, and can be a 1xn matrix or a simple vector.
#
#
# cost
# cost of constraints violation (default: 1). Rules the trade-off between regularization and correct classification on data. It can be seen as the inverse of a regularization constant.
#
#
# epsilon
# set tolerance of termination criterion for optimization. Default set to 0.1.
#
# bias
# if bias > 0, instance data becomes [data; bias]; if <= 0, no bias term added (default 1)
#
#
#
#

l1svm<-function(data, target, cost=1, epsilon=0.1, bias=1, ...) {
  input_p <- preprocess_input(data, target)
  xs <- cbind(input_p$data, bias)
  y <- input_p$yc
  l <- input_p$n
  p <- input_p$p
  alpha <- rep(0,l)
  w <- c(rep(0,p),0)
  U <- cost
  Qii <- y * y * sapply(1:l, function(i) xs[i,]%*%xs[i,])
  PGmax_old <- Inf
  PGmin_old <- -Inf
  active_size <- l

  for (k in 1:1000) {
    PGmax_new <- -Inf
    PGmin_new <- Inf

    prev_w <- w
    s <- 0
    index <- sample(1:active_size)
    while(s < active_size) {
      s <- s + 1
      i <- index[s]
      G <- y[i] * w %*% xs[i,] - 1
      PG <- 0

      if (alpha[i] == 0) {
        if (G > PGmax_old)
        {
          temp <- index[s]
          index[s] <- index[active_size]
          active_size <- active_size - 1
          s <- s - 1
          next
        } else if (G < 0) {
          PG <- G
        }
      } else if (alpha[i] == U) {
        if (G < PGmin_old)
        {
          temp <- index[s]
          index[s] <- index[active_size]
          active_size <- active_size - 1
          s <- s - 1
          next
        } else if (G > 0) {
          PG <- G
        }
      } else {
        PG <- G
      }

      PGmax_new <- max(PGmax_new, PG)
      PGmin_new <- min(PGmin_new, PG)

      if (abs(PG) > 1.0e-12) {
        old_alpha <- alpha[i]
        alpha[i] <- min(max(alpha[i]-G/Qii[i],0),U)
        w <- w + (alpha[i] - old_alpha)*y[i]*xs[i,]
      }
    }

    if(PGmax_new - PGmin_new <= epsilon) {
      if (active_size == l) {
        break
      } else {
        active_size <- l
        PGmax_old <- Inf
        PGmin_old <- -Inf
        next
      }
    }

    PGmax_old <- PGmax_new
    PGmin_old <- PGmin_new
    if (PGmax_old <= 0) {
      PGmax_old <- Inf
    }
    if (PGmin_old >= 0) {
      PGmin_old <- -Inf
    }
  }
  w
}

# preprocess_input
# Function used for ensuring that data and target arguments for l1svm functions have appriopriate format

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
