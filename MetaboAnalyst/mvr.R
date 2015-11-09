function (formula, ncomp, Y.add, data, subset, na.action, method = pls.options()$mvralg, 
          scale = FALSE, validation = c("none", "CV", "LOO"), model = TRUE, 
          x = FALSE, y = FALSE, ...) 
{
  ret.x <- x # a logical. If TRUE, the model matrix is returned.
  ret.y <- y # a logical. If TRUE, the response is returned.
  mf <- match.call(expand.dots = FALSE) # 匹配所有参数
  if (!missing(Y.add)) {
    #a vector or matrix of additional responses containing relevant information about the observations. Only used for cppls.
    Y.addname <- as.character(substitute(Y.add))
    mf$formula <- update(formula, paste("~ . +", Y.addname))
  }
  m <- match(c("formula", "data", "subset", "na.action"), 
             names(mf), 0)
  mf <- mf[c(1, m)]
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  method <- match.arg(method, c("kernelpls", "widekernelpls", 
                                "simpls", "oscorespls", "cppls", "svdpc", "model.frame"))
  if (method == "model.frame") 
    return(mf)
  mt <- attr(mf, "terms")
  Y <- model.response(mf, "numeric")
  if (is.matrix(Y)) {
    if (is.null(colnames(Y))) 
      colnames(Y) <- paste("Y", 1:dim(Y)[2], sep = "")
  }
  else {
    Y <- as.matrix(Y)
    colnames(Y) <- deparse(formula[[2]])
  }
  if (missing(Y.add)) {
    Y.add <- NULL
  }
  else {
    Y.add <- mf[, Y.addname]
    mt <- drop.terms(mt, which(attr(mt, "term.labels") == 
                                 Y.addname), keep.response = TRUE)
  }
  X <- delete.intercept(model.matrix(mt, mf))
  nobj <- dim(X)[1]
  npred <- dim(X)[2]
  if (length(attr(mt, "term.labels")) == 1 && !is.null(colnames(mf[[attr(mt, 
                                                                         "term.labels")]]))) 
    colnames(X) <- sub(attr(mt, "term.labels"), "", colnames(X))
  if (missing(ncomp)) {
    ncomp <- min(nobj - 1, npred)
    ncompWarn <- FALSE
  }
  else {
    if (ncomp < 1 || ncomp > min(nobj - 1, npred)) 
      stop("Invalid number of components, ncomp")
    ncompWarn <- TRUE
  }
  sdscale <- identical(TRUE, scale)
  if (is.numeric(scale)) 
    if (length(scale) == npred) 
      X <- X/rep(scale, each = nobj)
  else stop("length of 'scale' must equal the number of x variables")
  switch(match.arg(validation), CV = {
    val <- mvrCv(X, Y, ncomp, Y.add = Y.add, method = method, 
                 scale = sdscale, ...)
  }, LOO = {
    segments <- as.list(1:nobj)
    attr(segments, "type") <- "leave-one-out"
    val <- mvrCv(X, Y, ncomp, Y.add = Y.add, method = method, 
                 scale = sdscale, segments = segments, ...)
  }, none = {
    val <- NULL
  })
  if (identical(TRUE, ncomp > val$ncomp)) {
    ncomp <- val$ncomp
    if (ncompWarn) 
      warning("`ncomp' reduced to ", ncomp, " due to cross-validation")
  }
  fitFunc <- switch(method, kernelpls = kernelpls.fit, widekernelpls = widekernelpls.fit, 
                    simpls = simpls.fit, oscorespls = oscorespls.fit, cppls = cppls.fit, 
                    svdpc = svdpc.fit)
  if (sdscale) {
    scale <- sqrt(colSums((X - rep(colMeans(X), each = nobj))^2)/(nobj - 
                                                                    1))
    if (any(abs(scale) < .Machine$double.eps^0.5)) 
      warning("Scaling with (near) zero standard deviation")
    X <- X/rep(scale, each = nobj)
  }
  start.time <- proc.time()[3]
  z <- fitFunc(X, Y, ncomp, Y.add = Y.add, ...)
  z$fit.time <- proc.time()[3] - start.time
  class(z) <- "mvr"
  z$na.action <- attr(mf, "na.action")
  z$ncomp <- ncomp
  z$method <- method
  if (is.numeric(scale)) 
    z$scale <- scale
  z$validation <- val
  z$call <- match.call()
  z$terms <- mt
  if (model) 
    z$model <- mf
  if (ret.x) 
    z$x <- X
  if (ret.y) 
    z$y <- Y
  z
}
