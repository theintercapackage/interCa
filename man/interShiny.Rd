\name{interShiny}
\alias{interShiny}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
interShiny
}
\description{
The function displays a Shiny app through which the users can perform all functions of the interCa package through a graphical interface. The users can load their data in .xslx or csv format through the app and then specify the number of factorial axes they want to retain for the analysis. The users can then produce an interpretive axis or plane, filter points based on their interpretive coordinates, and finally download the corresponding charts in .pdf files, the corresponding tables in .xlsx files, and a ready report with the automatic interpretation of the results of the multiple correspondence analysis.
}
\usage{
interShiny(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
#How to run the shiny app

library(interCa)
interShiny()
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
