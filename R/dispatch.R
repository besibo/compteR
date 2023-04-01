#' Distributes debts among several persons
#'
#' When several persons participate in an event and spendings ares not evenly
#'   distributed among participants, \code{dispatch} provides a clear view of
#'   who owes how much money to whom.
#'
#' @param balance Numeric vector of length greater or equal to 2. The sum
#'   should equal 0 since \code{balance} contains positive values for people
#'   having spent more money than they should have, and negative values for
#'   people having spent less money than they should have. Idealy, the vector
#'   should be named (see example below)
#' @param order A character string specifying the sorting method to use for the
#'   output. Must be either "Who" (the default) or "Whom" (see examples).
#' @param round Integer. With how many digits should amounts owed be displayed. Defaults to 2
#' @return A 3-column data frame containing the name of the person owing the
#'   money, the amount owed and the recipient.
#' @examples
#' balance <- c(125, 346, -28, -312, -41, -90)
#' names(balance) <- c("John", "Gail", "Mike", "Lenny", "Christine", "Sally")
#' dispatch(balance)
#' dispatch(balance, order="Whom")

dispatch <- function(balance, order=c("Who","Whom"), round=2) {

	stopifnot(is.numeric(balance), length(balance) >= 2, sum(balance) <= 0.1)
	order <- match.arg(order)

	pos <- sort(balance[balance>0], decreasing = FALSE)
	neg <- abs(sort(balance[balance<0], decreasing = TRUE))

	nb.pos <- length(pos)

	out <- data.frame(Who = factor(levels = names(neg)),
					  Owes.What = numeric(),
					  ToWhom = factor(levels = names(pos)))

	pos2 <- pos
	neg2 <- neg

	for (j in 1:nb.pos) {
		ll <- whoPays(pos2,neg2)

		in.progress <- data.frame(Who = names(ll$Remb),
								  Owes.What = ll$Remb,
								  To.Whom = names(pos[j]))
		rownames(in.progress) <- NULL
		out <- rbind(out, in.progress)

		pos2 <- pos2[-1]
		neg2 <- ll$Reste
		}

	out$Owes.What <- round(out$Owes.What,2)
	out[,1] <- factor(out[,1])
	out[,3] <- factor(out[,3])

	out[,2] <- round(out[,2],round)

	if (order == "Who") {
		return(out[order(out[,1],out[,3]),])
	} else {
		return(out[order(out[,3],out[,1]),])
	}
}
