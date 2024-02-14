read_taz_file <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		select(TAZ = TAZID)
}

get_sml_districts_from_taz_file <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		group_by(DISTSML) %>%
		summarise()
}

get_med_districts_from_taz_file <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		group_by(DISTMED) %>%
		summarise()
}

make_taz_distsml_transl <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		select(TAZ = TAZID, DISTSML) %>%
		st_drop_geometry()
}

make_taz_distmed_transl <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		select(TAZ = TAZID, DISTSML) %>%
		st_drop_geometry()
}

read_income_groups <- function(income_groups_file) {
	income_groups_file %>%
		read_csv() %>%
		mutate(
			across(
				c(low, high),
				\(x) paste0("$", prettyNum(x, big.mark = ",")
				),
				.names = "{.col}_chr"
			),
			inc_range = case_when(
				is.na(low) ~ paste("\u2264", high_chr),
				is.na(high) ~ paste("\u2265", low_chr),
				TRUE ~ paste0(low_chr, "\u2013", high_chr)
			),
			low = replace_na(low, 0),
			inc_range = fct_reorder(inc_range, low)
		)
}

read_distances <- function(distance_skims_file) {
	distance_skims_file %>%
		read_all_omx("HBW") %>%
		rename(distance = HBW) %>%
		filter(distance < 1000) #External zones are coded as 10000, we don't want those
}
