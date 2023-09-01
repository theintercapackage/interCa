\name{interca}
\alias{interca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interca
}
\description{
The function performs multiple correspondence analysis (MCA) on a given data set and a given number of categorical variables. The function returns for each point for each axis the coordinates, the interpretive coordinates, the contributions, and the quality of display.

}
\usage{
interca(data = data, numaxes = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame containing the data to be analyzed.
}
  \item{numaxes}{
The number of categorical variables that will be retained for the calculation of the elements referred to in the function description.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Moschidis, S., Markos, A., & Thanopoulos, A. C. (2022). “Automatic” interpretation of multiple correspondence analysis (MCA) results for nonexpert users, using R programming. Applied Computing and Informatics, (ahead-of-print).
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
data<-ca::wg93

interca(data=data, numaxes=5)
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
