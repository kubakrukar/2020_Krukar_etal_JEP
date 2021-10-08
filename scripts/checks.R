# my CHECKs ####

normcheck <- function(x){
  print(shapiro.test(x))
  qqnorm(x)
  qqline(x)
  plot(density(x, na.rm=TRUE))
  kasia <- shapiro.test(x)
  ifelse(kasia$p.value < 0.05,
         
         ifelse(kasia$p.value < 0.001,
                print(paste("(Shapiro-Wilk's *W*~=~",round(kasia$statistic, 2),", *p*~<~.001)",sep="")),
                print(paste("(Shapiro-Wilk's *W*~=~",round(kasia$statistic, 2),", *p*~=~",round(kasia$p.value, 3),")",sep=""))),
         
         print(paste("(Shapiro-Wilk's *W*~=~",round(kasia$statistic, 2),", *p*~=~",round(kasia$p.value, 2),")",sep="")))
  print("If comparing many groups, don't forget to check homogenity of variance with bartlett.test()")
}

lmercheck <- function(x){
  require(MuMIn)
  require(effects)
  print(summary(x))
  cat("\n")
  kasia1 <- kappa.mer(x)
  kasia2 <- r.squaredGLMM(x)
  print(paste0("*Kappa* score was ", round(kasia1, 1), ", Marginal R^2 was ", 
               round(kasia2[1], 2), " and Conditional R^2 was ", round(kasia2[2], 2),"."))
  
  line <- readline("Press Enter to see normcheck(x) with residual plots and DFBETAS or Esc to terminate ")
  
  normcheck(residuals(x))
  plot(fitted(x), residuals(x))
  
  cat("\n")
  print(paste0("Make sure to run          plot(allEffects(",deparse(substitute(x)),"))          "))
  # deparse(substitute(x)) is a trick to get the name of an object from the object itself
  
  line <- readline("Press Enter to see DFBETAS or Esc to terminate ")
  
  require(influence.ME)
  cosik <- influence(x, obs=TRUE)
  xx <- as.data.frame(dfbetas(cosik))
  xx$rowname.in.modeled.df <- rownames(xx)
  for (i in 1:ncol(xx[, -ncol(xx)])) {
    z <- ncol(xx)
    print(head(xx[order(abs(xx[i]), decreasing=TRUE), c(i,z)]))
  }
  # see arXiv preprint arXiv:1308.5499 for intrpretation but in general
  # DFbeta suggests an influencial data point if it's half the value for the slope
}

lmcheck <- function(x){
  require(effects)
  print(summary(x))
  line <- readline("Press Enter to see normcheck(x) with residual plots and effect plots or Esc to terminate ")
  
  normcheck(residuals(x))
  plot(fitted(x), residuals(x))
  
  line <- readline("Press Enter to see effect plots or Esc to terminate ")
  
  plot(allEffects(x))
  
  # deparse(substitute(x)) is a trick to get the name of an object from the object itself
  
}


# Tools for diagnosing collinearity ####
# from http://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/
# Takes lmer object (i.e. my model) as input. Important to use the final predictors and not raw dataset
# prior to transformations.
# vif is more typically used for 'multicollinearity' (i.e. when one factor correlates with a combination of other
# factors).

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.

# so in my case (where this is less critical) I'll use kappa
## kappa, aka condition number.
## kappa < 6 is no collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity        [#baayen2008analyzing]

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

kappa.mer <- function (fit,
                       scale = TRUE, center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
  X <- fit@pp$X
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  nrp <- sum(1 * (nam == "(Intercept)"))
  if (nrp > 0) {
    X <- X[, -(1:nrp), drop = FALSE]
    nam <- nam[-(1:nrp)]
  }
  
  if (add.intercept) {
    X <- cbind(rep(1), scale(X, scale = scale, center = center))
    kappa(X, exact = exact)
  } else {
    kappa(scale(X, scale = scale, center = scale), exact = exact)
  }
}

colldiag.mer <- function (fit,
                          scale = TRUE, center = FALSE,
                          add.intercept = TRUE) {
  ## adapted from perturb::colldiag, method in Belsley, Kuh, and
  ## Welsch (1980).  look for a high condition index (> 30) with
  ## more than one high variance propotion.  see ?colldiag for more
  ## tips.
  result <- NULL
  if (center) 
    add.intercept <- FALSE
  if (is.matrix(fit) || is.data.frame(fit)) {
    X <- as.matrix(fit)
    nms <- colnames(fit)
  }
  else if (class(fit) == "mer") {
    nms <- names(fixef(fit))
    X <- fit@X
    if (any(grepl("(Intercept)", nms))) {
      add.intercept <- FALSE
    }
  }
  X <- X[!is.na(apply(X, 1, all)), ]
  
  if (add.intercept) {
    X <- cbind(1, X)
    colnames(X)[1] <- "(Intercept)"
  }
  X <- scale(X, scale = scale, center = center)
  
  svdX <- svd(X)
  svdX$d
  condindx <- max(svdX$d)/svdX$d
  dim(condindx) <- c(length(condindx), 1)
  
  Phi = svdX$v %*% diag(1/svdX$d)
  Phi <- t(Phi^2)
  pi <- prop.table(Phi, 2)
  colnames(condindx) <- "cond.index"
  if (!is.null(nms)) {
    rownames(condindx) <- nms
    colnames(pi) <- nms
    rownames(pi) <- nms
  } else {
    rownames(condindx) <- 1:length(condindx)
    colnames(pi) <- 1:ncol(pi)
    rownames(pi) <- 1:nrow(pi)
  }         
  
  result <- data.frame(cbind(condindx, pi))
  zapsmall(result)
}

maxcorr.mer <- function (fit,
                         exclude.intercept = TRUE) {
  so <- summary(fit)
  corF <- so@vcov@factors$correlation
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0 & exclude.intercept) {
    corF <- corF[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  corF[!lower.tri(corF)] <- 0
  maxCor <- max(corF)
  minCor <- min(corF)
  if (abs(maxCor) > abs(minCor)) {
    zapsmall(maxCor)
  } else {
    zapsmall(minCor)
  }
}
