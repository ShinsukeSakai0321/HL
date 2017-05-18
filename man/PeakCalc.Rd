\name{PeakCalc}
\alias{PeakCalc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculation of peak values from sampling data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
From sampling data of wave, peak values are calculated and are returned as vector object. By investing three consequtive sampling data, if the increasing direction is changed the peak value is calculated from these three values.
}
\usage{
PeakCalc(Wave)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Wave}{vector of wave sampling data
%%     ~~Describe \code{Wave} here~~
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
Shinsuke Sakai
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
wave<-c(5e-3,3.2e-2,3.8e-2,-3.3e-2,-1.9e-2,-1e-2,1e-3,-8e-3,-2e-2,1e-2,-1e-3,4e-3,1.1e-2,-1e-3,-7e-3,-2e-3)
PeakCalc(Wave=wave)
#Expected results
[1,]  0.044858766
[2,] -0.037777941
[3,]  0.001025000
[4,] -0.020964286
[5,]  0.011100610
[6,] -0.001281250
[7,]  0.011164474
[8,] -0.007011364
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
