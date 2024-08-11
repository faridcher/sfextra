\name{num2words}
\alias{num2words}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Numbers to Equivalent Word(s)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function converts numbers to their textual representation in Persian language.
}
\usage{
num2words(x, unit = c("m^2", "dm^2"), dec_sep = ifelse(length(unit) == 2, " و ", " ممیز "))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
A numeric vector of real numbers
}
  \item{unit}{
%%     ~~Describe \code{unit} here~~
unit of x
}
  \item{dec_sep}{
%%     ~~Describe \code{dec_sep} here~~
decimal separator text in Persian language. If \code{unit} has two elements, i.e. units for the whole and decimal parts, this parameter text is "و", otherwise it would be "ممیز" which is more general and requires only one unit.
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
a character vector of x consisitng of the number in textual format
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Farid Cheraghi
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\url{https://pypi.org/project/num2fawords/}
\url{https://github.com/mojtabaahn/php-persian-number-to-words}
\url{https://coderanch.com/t/771776/java/convert-numbers-Persian-word}
\url{https://stackoverflow.com/questions/31439604/how-to-convert-persian-and-arabic-digits-of-a-string-to-english-using-javascript}
}

\examples{
num2words(22.32, 'm^2')
num2words(.8, '')
num2words(5,c('m','cm'))
num2words(c(.8,.08,.83))
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
