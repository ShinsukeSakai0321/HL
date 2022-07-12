\name{HLrainflow}
\alias{HLrainflow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Evaluation of rainflow range and mean values by HLrainflow algorithm
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
HLrainflow algorithm which is exactly same as rain-flow algorithm is applied to the vector of peak values and the combination of half-range and mean values are returned.
}
\usage{
HLrainflow(Peak)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Peak}{
%%     ~~Describe \code{Peak} here~~
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
#ASTM E1049-85(2017) Rainflow Counting Example
pk<-c(-2,1,-3,5,-1,3,-4,4,-2)
HLrainflow(Peak=pk)
#Expected results
  halfR halfM
1     4   1.0
2     4   1.0
3     3  -0.5
4     4  -1.0
5     8   1.0
6     9   0.5
7     8   0.0
8     6   1.0

#Evaluation of peaks and apllying HLrainflow algorithm to them
wave<-c(5e-3,3.2e-2,3.8e-2,-3.3e-2,-1.9e-2,-1e-2,1e-3,-8e-3,-2e-2,1e-2,-1e-3,4e-3,1.1e-2,-1e-3,-7e-3,-2e-3)
pk<-PeakCalc(Wave=wave)
HLrainflow(Peak=pk)
#Expected results
  halfR halfM
1 0.02198929 -0.009969643
2 0.02198929 -0.009969643
3 0.01238186  0.004909680
4 0.01238186  0.004909680
5 0.08263671  0.003540413
6 0.04894241 -0.013306734
7 0.01817584  0.002076555
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
