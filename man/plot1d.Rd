\name{plot1d}
\alias{plot1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpretive axis
}
\description{
The function takes the result of the interca() function and the number of a factorial axis and returns the corresponding interpretive axis with the most significant points.
}
\usage{
plot1d(res, dim = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
The result of the execution of the interca() function.
}
  \item{dim}{
The number of the interpretive axis to be printed.
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
interCa
}
\examples{

#For example, we will use the wg93 data set that is available in the #package ca των Nenadic, O., & Greenacre, M. (2007). Correspondence #analysis in R, with two-and three-dimensional graphics: the ca package. #Journal of statistical software, 20, 1-13.

library(ca)
library(interCa)
data<-ca::wg93
res<-interca(data,5)
plot1d(res,1)

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
