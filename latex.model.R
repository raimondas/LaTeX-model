.help.ESS <- help

x <- rnorm(100, mean = 2, sd = 0.1)
y <- 0.65 * x + rnorm(100, sd = 0.01)
m <- lm(y ~ x)
sm <- summary(m)

latex.model <- function(model, file, yname = NULL, xnames = NULL,
                        digits = 4, signif = c("se", "tval", "pval")) {
  sm <- summary(model)  
  coeft <- round(sm$coefficients, digits)
  coefs <- coeft[, 1]
  se <- coeft[, 2]
  tval <- coeft[, 3]
  pval <- coeft[, 4]
  if(is.null(yname)) yname <- as.character(sm$terms)[2]
  if(is.null(xnames)) xnames <- as.character(sm$terms)[-1:-2]
  if(attr(sm$terms, "intercept") == 1) xnames <- c("", xnames)
  signl <- lapply(signif, function(x)
                  as.character(eval(parse(text = x))))
  names(signl) <- signif
  signl$se <- paste("(", signl$se, ")", sep = "")
  signl$pval <- paste("\\{", signl$pval, "\\}", sep = "")
  signl$tval <- paste("[", signl$tval, "]", sep = "")
  p4 <- paste(signl$se, signl$tval, signl$pval, sep = " \\\\ ")
  p5 <- gsub(" [\\][\\] $", "", p4)
  p6 <- paste("\\underset{\\substack{", p5, "}}{", coefs, "}", sep = "")
  p7 <- paste(paste(p6, xnames, sep = ""), collapse = " + ")
  p8 <- paste(yname, " = ", p7, sep = "")
  cat("\\begin{align*}\n", file = file)
  cat(p8, "\n", file = file, append = TRUE)
  cat("\\end{align*}", file = file, append = TRUE)
}


latex.model(m, file = "temp.tex", signif = c("se", "tval"))
