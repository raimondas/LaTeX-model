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
  p1 <- paste(paste(coefs, xnames, sep = ""), collapse = " + ")
  p2 <- paste(yname, "=", p1, sep = " ")
  signl <- lapply(signif, function(x)
                  as.character(eval(parse(text = x))))
  names(signl) <- signif
  signl$se <- paste(paste("\\mbox{\\footnotesize(", signl$se, ")}",
                          sep = ""), collapse = " ")
  signl$pval <- paste(paste("\\mbox{\\footnotesize\\{", signl$pval,
                            "\\}}", sep = ""), collapse = " ")
  signl$tval <- paste(paste("\\mbox{\\footnotesize[", signl$tval,
                            "]}", sep = ""), collapse = " ")
  signl <- signl[signif]
  p3 <- paste(unlist(signl), collapse = "\\\\\n")
  cat("\\begin{align*}\n", file = file)
  cat(p2, "\\\\\n", p3, "\n", file = file, append = TRUE)
  #cat(p2, "\\\\\n", ses, "\\\\\n", ts, "\\\\\n", ps, "\n",
  #    file = file, append = TRUE)
  cat("\\end{align*}", file = file, append = TRUE)
}


latex.model(m, file = "temp.tex", signif = c("se", "tval"))
