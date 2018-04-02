# Functions for reporting APA results
# Alex Danvers, April 2018

# Chi Square Model Comparison reporting
# x is model comparison test as conducted by lme4
chi_inline = function(x) {
  df.comp <- x$`Chi Df`[2]
  chi.val <- round(x$Chisq[2], digits=2)
  p.val <- round(x$`Pr(>Chisq)`[2], digits=3)
  p.txt <- substr(as.character(p.val), start=2, stop=6)
  if (p.val > .001) {
    out <- paste0("$\\chi^2$"," (",df.comp,") = ",chi.val,", ","*p*"," = ",p.txt)
  } else {
    out <- paste0("$\\chi^2$"," (",df.comp,") = ",chi.val,", ","*p*"," < .001")
  }
  return(out)
}

# lmerTest reporting of significance of coefficients
# x is a lmerTest model, c is the number of the coefficient
lmerT.coef = function(x, coefN = c) {
  mod.summary <- summary(x)
  coef.n <- mod.summary$coefficients[coefN,]
  df.n <- round(coef.n[3], digits=2)
  p.val <- round(coef.n[5], digits=3)
  p.txt <- substr(as.character(p.val), start=2, stop=6)
  if (p.val > .001) {
    out <- paste0("*b*"," = ",round(coef.n[1],digits=2),", ","*t* (",df.n,") = ",
                  round(coef.n[4],digits=2),", *p* = ",p.txt)
  } else {
    out <- paste0("*b*"," = ",round(coef.n[1],digits=2),", ","*t* (",df.n,") = ",
                  round(coef.n[4],digits=2),", *p* = < .001")
  }
  return(out)
}

