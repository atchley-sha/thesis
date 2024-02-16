# Functions related to SE data that aren't for a specific scenario

get_cube_se_diff <- function(se_list = list()) {
	clean_list <- imap(
		se_list,
		\(x, idx) select(x, -c(CO_TAZID, CO_FIPS, CO_NAME)) %>%
			pivot_longer(-TAZ))

	clean_list[[1]] %>%
		full_join(
			clean_list[[2]],
			join_by(TAZ, name),
			suffix = c("_1", "_2")) %>%
		mutate(across(-TAZ, \(x) replace_na(x, 0))) %>%
		mutate(
			diff = value_1 - value_2
			# pct_diff = diff / trips_1
		) %>%
		rename_with(
			\(x) case_match(
				x,
				"value_1" ~ names(clean_list)[1],
				"value_2" ~ names(clean_list)[2]
			),
			.cols = c(value_1, value_2)
		)
}
