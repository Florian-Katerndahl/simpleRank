# exportieren
ts_slope <- function(t = NULL, Y) {
	# t = x-Achse
	# Y =y-Achse
	N <- length(Y)
	t <- check_sequence(t, N)

	comb_t <- utils::combn(t, 2)
	comb_Y <- utils::combn(Y, 2)

	stats::median(
		# aufgrund der von Sen genannten Bediungungen zu t_j und t_i kann ich nicht einfach so die Summe bilden, auch wenn
		# das vielleicht schneller wäre.
		purrr::pmap_dbl(list(comb_Y[, 2], comb_Y[, 1], comb_t[, 2], comb_t[, 1]),
						function(Yj, Yi, tj, ti) {
							if (tj <= ti | tj - ti == 0) {
								return(NA)
							}
							(Yj - Yi) / (tj - ti)
						}

		)
		, na.rm = TRUE)
}

# exportieren
ts_score <- function(t = NULL, Y, slope) {
	N <- length(Y)
	t <- check_sequence(t, N)

	coeff_T <- t * slope

	z <- Y - coeff_T

	comb_t <- utils::combn(t, 2)
	comb_z <- utils::combn(z, 2)

	result <- sgn(comb_t, comb_z)

	list("coeff" = coeff_T, "score" = result)
}

# exportieren
ts_variance <- function(n, u = rep(1, length.out = n)) {
	# Das muss sich besser lösen lassen. Vielleicht kann ich auch einfach annehmen, dass eine Kontingenztabelle übergeben wird?
	# Dann könnte ich eine Hilfsfunktion implementieren, die guckt was als u übergeben wurde und daraus einen Vektor machen....
	# Kann aber nicht mit is.numeric() gemacht werden, das auf table() ebenfalls TRUE zurückgibt.
	# if (is.null(u)) {
	# 	u <- rep(1, length.out = n)
	# } else if (!is.table(u) | !is.array(u)) {
	# 	u <- table(u)
	# }
	#
	# u <- coerce_to_numeric(u)
	((n * (n - 1) * (2 * n + 5)) - sum(u * (u - 1) * (2 * u + 5))) / 18
}

# exportieren
ts_tau <- function(t = NULL, n, score) {
	t <- check_sequence(t, n)

	comb_t <- utils::combn(t, 2)

	N <- onesided_sgn(comb_t)

	(1 / sqrt(N * choose(n, 2))) * score
}

# exportieren
ts_test <- function(t = NULL, Y, alpha_val = 0.05) {

}
