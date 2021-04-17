# exportieren
mk_score <- function(t = NULL, Y) {
	N <- length(Y)
	t <- check_sequence(t, N)

	comb_t <- utils::combn(t, 2)
	comb_Y <- utils::combn(Y, 2)

	sgn(comb_t, comb_Y)
}

# exportieren
mk_variance <- function(n, t = NULL, u = NULL) {
	t <- check_sequence(t, n)
	u <- check_sequence(u, n)

	(((n * (n - 1) * (2 * n + 5)) - sum(t * (t - 1) * (2 * t + 5)) - sum(u * (u - 1) * (2 * u + 5))) +
			((1 / (9 * n * (n - 1) * (n - 2))) * sum(t * (t - 1) * (t - 2)) * sum(u * (u - 1) * (u - 2))) +
			((1 / (2 * n * (n - 1))) * sum(t * (t - 1)) * sum(u * (u - 1)))) / 18
}

# eventuell exportieren
mk_statisitc <- function(score, variance) {
	if (score > 0) {
		return((score - 1) / sqrt(variance))
	} else if (score < 0) {
		return((score + 1) / sqrt(variance))
	} else {
		return(0)
	}
}

# exportieren
# Für Dokumentation: returns named vector...
mk_tau <- function(score, n, t = NULL, u = NULL) {
	if (is.null(t) & is.null(u)) {
		result <- score / choose(n, 2)

		names(result) <- "Tau a"

		return(result)
	} else {
		t <- check_sequence(t)
		u <- check_sequence(u)
	}

	result <- score / (sqrt(choose(n, 2) - (0.5 * sum(t * (t - 1)))) * sqrt(choose(n, 2) - (0.5 * sum(u * (u - 1)))))

	names(result) <- "Tau b"

	return(result)
}

# exportieren
mk_test <- function(t = NULL, Y, alpha_val = 0.05, alternative = c("two.sided", "less", "greater")) {

}
