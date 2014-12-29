indt <-rawMs
splitCols <- 'cellLines'
cSplit <- function(indt, splitCols, sep = ",", direction = "wide", 
                   makeEqual = NULL, fixed = TRUE, drop = TRUE, 
                   stripWhite = FALSE) {
  message("`cSplit` is now part of the 'splitstackshape' package (V1.4.0)")
  ## requires data.table >= 1.8.11
  require(data.table)  
  if (!is.data.table(indt)) setDT(indt)
  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  if (any(!vapply(indt[, splitCols, with = FALSE],  # 判断splitCols列是否为character
                  is.character, logical(1L)))) {    # 是则跳过，否则转换
    indt[, eval(splitCols) := lapply(.SD, as.character),
         .SDcols = splitCols]
  }
  
  if (length(sep) == 1) 
    sep <- rep(sep, length(splitCols))  # 定义分割符号
  if (length(sep) != length(splitCols)) { #可以多列根据不同的分割符进行分割
    stop("Verify you have entered the correct number of sep")
  }
  
  if (isTRUE(stripWhite)) { #判断是否存在空格
    indt[, eval(splitCols) := mapply(function(x, y) 
      gsub(sprintf("\\s+%s\\s+|\\s+%s|%s\\s+",  # 正则表达式判断分隔符前后是否有空格
                   x, x, x), x, y),             # 将分隔符前后空格 替换为仅有分隔符
      sep, indt[, splitCols, with = FALSE], 
      SIMPLIFY = FALSE)]
  }  
  
  X <- lapply(seq_along(splitCols), function(x) {
    #分割字符,保存在list X中
    strsplit(indt[[splitCols[x]]], split = sep[x], fixed = fixed)  
  })
  
  if (direction == "long") {
    if (is.null(makeEqual)) {
      IV <- function(x,y) if (identical(x,y)) TRUE else FALSE
      makeEqual <- ifelse(Reduce(IV, rapply(X, length, how = "list")),
                          FALSE, TRUE)
    }
  } else if (direction == "wide") {
    if (!is.null(makeEqual)) {
      if (!isTRUE(makeEqual)) {
        message("makeEqual specified as FALSE but set to TRUE")
        makeEqual <- TRUE
      }
      makeEqual <- TRUE
    } else {
      makeEqual <- TRUE
    }
  }
  if (isTRUE(makeEqual)) {
    SetUp <- lapply(seq_along(X), function(y) {
      A <- vapply(X[[y]], length, 1L)
      list(Mat = cbind(rep(seq_along(A), A), sequence(A)),
           Val = unlist(X[[y]]))
    })    
    Ncol <- max(unlist(lapply(SetUp, function(y) y[["Mat"]][, 2]), 
                       use.names = FALSE))
    X <- lapply(seq_along(SetUp), function(y) {
      M <- matrix(NA_character_, nrow = nrow(indt), ncol = Ncol)
      M[SetUp[[y]][["Mat"]]] <- SetUp[[y]][["Val"]]
      M
    })
    if (direction == "wide") {
      X <- lapply(seq_along(X), function(x) {
        colnames(X[[x]]) <- paste(splitCols[x], 
                                  sequence(ncol(X[[x]])), 
                                  sep = "_")
        X[[x]]
      })
      if (isTRUE(drop)) {
        cbind(indt, do.call(cbind, X))[, eval(splitCols) := NULL][]
      } else {
        cbind(indt, do.call(cbind, X))
      }
    } else {
      indt <- indt[rep(sequence(nrow(indt)), each = Ncol)]
      X <- lapply(X, function(y) as.vector(t(y)))
      indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
    }  
  } else {
    Rep <- vapply(X[[1]], length, integer(1L))
    indt <- indt[rep(sequence(nrow(indt)), Rep)]
    indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
  }
}