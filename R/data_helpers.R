get_dist_from_tazs <- function(tazs, transl) {
	transl %>%
		filter(TAZ %in% tazs) %>%
		pull(DIST) %>%
		unique()
}
