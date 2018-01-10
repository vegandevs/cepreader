\name{readCEP}
\alias{readCEP}

\title{Reads a CEP (Canoco) data file }
\description{
  \code{readCEP} reads a file formatted by relaxed strict CEP format
  used by \code{Canoco} software, among others.
}
\usage{
readCEP(file, maxdata=10000, positive=TRUE, trace=FALSE, force=FALSE)
}

\arguments{
  \item{file}{File name (character variable). }
  \item{maxdata}{Maximum number of non-zero entries. }
  \item{positive}{Read only positive data entries (normal in community data).}
  \item{trace}{Work verbosely.}
  \item{force}{Run function, even if \R refuses first.}
}
\details{
  Cornell Ecology Programs (CEP) introduced several data formats
  designed for punched cards.  One of these was the \dQuote{condensed strict}
  format which was adopted by popular software \code{DECORANA} and
  \code{TWINSPAN}. Later, Cajo ter Braak wrote \code{Canoco}
  based on \code{DECORANA}, where he adopted the format, but relaxed it
  somewhat (that's why I call it \dQuote{relaxed strict} format). Further, he
  introduced a more ordinary \dQuote{free} format, and allowed the use of
  classical Fortran style \dQuote{open} format with fixed field widths.  This
  function should be able to deal with all these \code{Canoco} formats,
  whereas it cannot read many of the traditional CEP alternatives.

  All variants of CEP formats have:
  \itemize{
    \item Two or three title cards, most importantly specifying the format (or word
    \code{FREE}) and the number of items per record (number of species
    and sites for \code{FREE} format).
    \item Data in one of three accepted formats:
    \enumerate{
      \item Condensed format: First number on the line is the site
      identifier, and it is followed by pairs (\dQuote{couplets}) of numbers
      identifying the species and its abundance (an integer and a floating
      point number).
      \item Open Fortran format, where the first number on the line must
      be the site number, followed by abundance values in fields of
      fixed widths. Empty fields are interpreted as zeros.
      \item \dQuote{Free} format, where the numbers are interpreted as
      abundance values.  These numbers must be separated by blank space,
      and zeros must be written as zeros.
    }
    \item Species and site names, given in Fortran format \code{(10A8)}:
    Ten names per line, eight columns for each.
  }

  With option \code{positive = TRUE} the function removes all lines and
  columns with zero or negative marginal sums.  In community data
  with only positive entries, this removes empty sites and species.
  If data entries can be negative, this ruins data, and such data sets
  should be read in with option \code{positive = FALSE}.
}
\value{
  Returns a data frame, where columns are species and rows are
  sites. Column and row names are taken from the CEP file, and changed
  into unique \R names by \code{\link{make.names}} after stripping the blanks.
}
\references{ 
  Ter Braak, C.J.F. (1984--): CANOCO -- a FORTRAN program for \emph{cano}nical
  \emph{c}ommunity \emph{o}rdination by [partial] [detrended] [canonical]
  correspondence analysis, principal components analysis and redundancy
  analysis. \emph{TNO Inst. of Applied Computer Sci., Stat. Dept. Wageningen,
  The Netherlands}. 
}
\author{ Jari Oksanen }

\note{
  The function relies on smooth linking of Fortran file input in \R
  session.  This is not guaranteed to work, and therefore the function
  may not work in \emph{your} system, but it
  can crash the \R session.  Therefore the default is that the function
  does not run.  If you still want to try:
  \enumerate{
    \item
    Save your session
    \item
    Run \code{readCEP()} with switch \code{force=TRUE}
  }

  If you transfer files between operating systems or platforms, you
  should always check that your file is formatted to your current
  platform. For instance, if you transfer files from Windows to Linux,
  you should change the files to \code{unix} format, or your session may
  crash when Fortran program tries to read the invisible characters that
  Windows uses at the end of each line.

  This function was included in \pkg{vegan} up to version 2.4-5. It
  was moved to a package of its own because compiled functions using
  Fortran I/O can interfere with compiled code written in other
  languages. This can disturb \pkg{vegan}, other loaded packages and
  packages that depend on \pkg{vegan}.

}


\examples{
## Provided that you have the file `dune.spe'
\dontrun{
theclassic <- readCEP("dune.spe", force=TRUE)}
}
\keyword{ IO }
\keyword{ file }
