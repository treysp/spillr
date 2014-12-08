\name{spillr-package}
\alias{spillr-package}
\alias{spillr}
\docType{package}
\title{
R package of miscellaneous functions
}
\description{
Miscellaneous R function created during data cleaning, analysis, and package development.
}
\details{
\tabular{ll}{
Package: \tab spillr\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-08\cr
License: \tab GPL-3\cr
}
In progress...still compiling functions from various projects.
}
\author{
Trey Spiller

Maintainer: Trey Spiller <treyspiller@gmail.com>
}
\examples{
# all.equal.mult() compares >=2 objects with R function all.equal()
foo <- c(1:10)
bar <- c(1:10)
foz <- c(1:10)
baz <- letters[1:10]

all.equal.mult(foo, bar) # TRUE
all.equal.mult(foo, baz) # results of all.equal(foo, baz) as one-item list
all.equal.mult(foo, bar, foz) # TRUE
all.equal.mult(foo, bar, baz) # list of pairwise all.equal() comparisons among objects
}
