\name{plot2d}
\alias{plot2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpretive plane
}
\description{
The function takes as parameters the result of the interca() function, the interpretive axis that will form the x axis, and the interpretive axis that will form the y axis. The function returns the interpretive plane formed by the above.
}
\usage{
plot2d(res, dim1 = 1, dim2 = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
The result of the interca() function.
}
  \item{dim1}{
The number of the interpretive axis that will form the x axis of the interpretive plane.
}
  \item{dim2}{
The number of the interpretive axis that will form the y axis of the interpretive plane.
}
}
\details{
The interca() function must be executed first in order to use the function.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#For example, we will use the wg93 data set that is available in the #package ca των Nenadic, O., & Greenacre, M. (2007). Correspondence #analysis in R, with two-and three-dimensional graphics: the ca package. #Journal of statistical software, 20, 1-13.

library(ca)
library(interCa)
data<-ca::wg93
res<-interca(data,5)
plot2d(res,1,2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
