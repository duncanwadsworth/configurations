## My attempt at a proper .Rprofile file

## Set to the Hutch's repo
r = getOption("repos")
r["CRAN"] = "https://cran.fhcrc.org/"
options(repos = r)
rm(r)

## General options
#options(graphics.record = TRUE)
#options(save.defaults = "no")

## ------- Convenience functions - all stolen from the internets and hacked
## All-in-one install and require
lopa = function(pkgs){
  ## install packages not already loaded
  pkgs_miss = pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  if (length(pkgs_miss) == 0) {
    message("\n ...Packages were already installed!\n")
  }
  ## load packages
  for (i in 1:length(pkgs)){
    require(pkgs[i], character.only = TRUE)
  }
}
## matlab-like timing functions
tic = function(gcFirst = TRUE, type = c("elapsed", "user.self", "sys.self")){
  type = match.arg(type)
  assign(".type", type, envir = baseenv())
  if(gcFirst) gc(FALSE)
  tic = proc.time()[type]
  assign(".tic", tic, envir = baseenv())
  invisible(tic)
}
toc = function(){
  type = get(".type", envir = baseenv())
  toc = proc.time()[type]
  tic = get(".tic", envir = baseenv())
  print(toc - tic)
  invisible(toc)
}
## Hate default pairs
nicepairs = function(X, histogram = TRUE, ...){
  X = as.matrix(X)
  if(mode(X) != "numeric"){
    stop("Must pass in only numeric values")
  }
  ## for printing pair-wise correlation - ignoring missing values
  panel.cor = function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", cex.cor, ...) {
    usr = par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r = abs(cor(x, y, use = use))
    txt = format(c(r, 0.123456789), digits = digits)[1]
    txt = paste(prefix, txt, sep = "")
    if(missing(cex.cor)){cex = 0.7/strwidth(txt)}
    text(0.5, 0.5, txt, cex = cex * r)
  }
  ## for histogram showing density overlay and rug
  hist.panel = function(x, ...) {
    par(new = TRUE)
    hist(x, col = "gray", probability = TRUE, axes = FALSE, main = "", breaks = "FD")
    lines(density(x, na.rm = TRUE), col = "red", lwd = 2)
    rug(x)
  }
  if(histogram){
    pairs(X, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = hist.panel, lwd = 2, ...)
  } else {
    pairs(X, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, lwd = 2, ...)
  }
}
## Quit that doesn't bother you
exit = function(save = "no", ...){base::quit(save = save, ...)}
## Bioconductor install because I can never remember it
install.bioc = function(pkgs){source("http://bioconductor.org/biocLite.R"); biocLite(pkgs)}
## Aliases that can't be deleted (http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile)
.startup = new.env()
assign("exit", exit, env = .startup)
assign("lopa", lopa, env = .startup)
assign("install.bioc", install.bioc, env = .startup)
assign("nicepairs", nicepairs, env = .startup)
assign("tic", tic, env = .startup)
assign("toc", toc, env = .startup)
attach(.startup)
## Remove so they don't show up in global environment upon startup
rm("exit", "lopa", "install.bioc", "nicepairs", "tic", "toc")

## Upon start up
.First = function(){
  #if(interactive()){
  #  library(stats)
  #  library(ggplot2)
  #  library(plyr)
  #  library(reshape2)
  #  library(xtable)
  #  library(knitr)
  #}
  cat("\nOrale! It's", format(Sys.time(), "%A, %B %d, %Y at %X"), "\n\n")
}

.Last = function(){
  cat("\nLaters! It's", format(Sys.time(), "%A, %B %d, %Y at %X"), "\n")
}
