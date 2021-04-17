# TODO
# Brauche noch Funktion, um Datum explizit zu numeric umzuwandeln!
# Diese braucht nicht exportiert zu werden, interne Funktion reicht, die dann in `check_sequence` verwendet wird.

# exportieren?
sgn <- function(t, z) {
	sum(
		purrr::pmap_dbl(list(t[, 2], t[, 1], z[, 2], z[, 1]),
						~ sign(..1 - ..2) * sign(..3 - ..4)
		)
	)
}

# exportieren?
onesided_sgn <- function(t) {
	sum(
		purrr::map2_dbl(t[, 2], t[, 1]), function(t2, t1) sign(t2 - t1)
	)
}

# nicht exportieren
check_sequence <- function(t = NULL, n) {
	if (!is.null(t)) {
		if (length(t) != n) {
			stop("While t was supplied: objects t and Y are not of same length.")
		}
		return(t)
	}
	seq(from = 1, to = n)
}

# nicht exporieren
coerce_to_num <- function(x) {

}
