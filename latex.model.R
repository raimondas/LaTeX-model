.help.ESS <- help

x <- rnorm(100, mean = 2, sd = 0.1)
y <- 0.65 * x + rnorm(100, sd = 0.01)
m <- lm(y ~ x)

latex.model <- function(model, file, yname = NULL, xnames = NULL,
                        round = 4) {
  sm <- summary(model)  
  coeft <- sm$coefficients
  coefs <- coeft[, 1]
  ses <- coeft[, 2]
  ts <- coeft[, 3]
  ps <- coeft[, 4]
  cat(coefs, "\n", ses, "\n", "ts", "\n", "ps", file = file)
}

latex.model(m, file = "temp.tex")
