#' Splits a vector in 2 shorter ones depending on the amount a person should be reimbursed of.
#' 
#' This is an internal function. It should not be called directly by the user.
#' 
#' @param pos Numeric vector containing the amount of money some people in the
#'   group have spend in excess and should be reimbursed of. Idealy, the vector
#'   should have a name for each value.
#' @param neg Numeric vector containing the amount of money some people in the
#'   group should reimburse to balance the spendings. Idealy, the vector should
#'   have a name for each value.
#' 
#' @return A list of 2 numerical vectors. \code{Remb} contains the values that a set of people need to spend to reimburse the first person in the \code{pos} vector. \code{Reste} contains the value that each debtor has still to spend to reimburse the remaining people in the \code{pos} vector.
#' @examples
#' pos <- c(125, 346)
#' names(pos) <- c("John", "Gail")
#' neg <- c(28, 312, 41, 90)
#' names(neg) <- c("Mike", "Lenny", "Christine", "Sally")
#' whoPays(pos,neg)
whoPays <- function(pos,neg) {

	cost <- cumsum(neg)
	tmp <- (cost-pos[1])-neg
	
	idx <- min(which(round(cost,2) >= round(pos[1],2)))
	neg <- c(neg[1:idx],neg[idx:length(neg)])
	neg[idx] <- abs(tmp[idx])
	neg[idx+1] <- neg[idx+1]-neg[idx]

	return(list(Remb = neg[1:idx], Reste=neg[-(1:idx)]))
	
	}
