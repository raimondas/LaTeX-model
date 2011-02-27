.help.ESS <- help

x <- rnorm(100, mean = 2, sd = 0.1)
z <- runif(100)
y <- -0.65 * x + 0.75*z + rnorm(100, sd = 0.01)
m <- lm(y ~ x + z)
sm <- summary(m)

latex.model <- function(x) {
  UseMethod("latex.model")
}

latex.model.lm <- function(model, file, yname = NULL, xnames = NULL,
                        digits = 4, signif = c("se", "tval", "pval"),
                        remins = TRUE, alpha = 0.05) {
  sm <- summary(model)
  if(remins) {
    nrrows <- which(sm$coefficients[, 4] <= alpha)
    smc <- sm$coefficients[nrrows, , drop = FALSE]
  } else {
    smc <- sm$coefficients
    nrrows <- 1:dim(smc)[1]
  }
  coeft <- formatC(smc, digits = digits, format = "f")
  coeft[, 2] <- paste("(", coeft[, 2], ")", sep = "")
  coeft[, 3] <- paste("[", coeft[, 3], "]", sep = "")
  coeft[, 4] <- paste("<", coeft[, 4], ">", sep = "")
  coeft <- t(apply(coeft, 1, formatC, flag = " "))
  coeft[, -1] <- gsub(" ", "\\\\,", coeft[, -1])
  colnames(coeft) <- c("est", "se", "tval", "pval")
  coeft <- coeft[, c("est", signif), drop = FALSE]
  if(is.null(yname)) yname <- as.character(sm$terms)[2]
  if(is.null(xnames)) {
    xnames <- unlist(strsplit(as.character(sm$terms)[-1:-2], " [+] "))
  }
  if(attr(sm$terms, "intercept") == 1) xnames <- c("", xnames)
  xnames <- xnames[nrrows]
  f <- function(x) {
    z <- paste(x[-1], collapse = " \\\\ ")
    paste("\\underset{\\substack{", z, "}}{", x[1], "}", collapse = "")
  }
  p1 <- paste(apply(coeft, 1, f), xnames, sep = "")
  p2 <- paste(yname, "=", paste(p1, collapse = " + "))
  cat("\\begin{align*}\n", file = file)
  cat(p2, "\n", file = file, append = TRUE)
  cat("\\end{align*}", file = file, append = TRUE)
}


latex.model(m, file = "temp.tex", signif = c("se"),
            yname = "y_i", xnames = c("x_i", "z_i"))


  
