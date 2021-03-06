% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dispatch.R
\name{dispatch}
\alias{dispatch}
\title{Distributes debts among several persons}
\usage{
dispatch(balance, order = c("Who", "Whom"), round = 2)
}
\arguments{
\item{balance}{Numeric vector of length greater or equal to 2. The sum
should equal 0 since \code{balance} contains positive values for people
having spend more money than they should have, and negative values for
people having spend less money than they should have. Idealy, the vector
should be named (see example below)}

\item{order}{A character string specifying the sorting method to use for the
output. Must be either "Who" (the default) or "Whom" (see examples).}

\item{round}{Integer. With how many digits should amounts owed be displayed. Defaults to 2}
}
\value{
A 3-column data frame containing the name of the person owing the
  money, the amount owed and the recipient.
}
\description{
When several persons participate in an event and spendings ares not evenly
  distributed among participants, \code{dispatch} provides a clear view of
  who owes how much money to whom.
}
\examples{
balance <- c(125, 346, -28, -312, -41, -90)
names(balance) <- c("John", "Gail", "Mike", "Lenny", "Christine", "Sally")
dispatch(balance)
dispatch(balance, order="Whom")
}

