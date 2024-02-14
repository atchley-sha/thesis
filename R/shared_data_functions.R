#' @export
read_taz_file <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		select(TAZ = TAZID)
}

#' @export
get_sml_districts_from_taz_file <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		group_by(DISTSML) %>%
		summarise() %>%
		rename(DIST = DISTSML)
}

#' @export
get_med_districts_from_taz_file <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		group_by(DISTMED) %>%
		summarise() %>%
		rename(DIST = DISTMED)
}

#' @export
make_taz_distsml_transl <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		select(TAZ = TAZID, DISTSML) %>%
		st_drop_geometry() %>%
		rename(DIST = DISTSML)
}

#' @export
make_taz_distmed_transl <- function(taz_file) {
	taz_file %>%
		st_read() %>%
		select(TAZ = TAZID, DISTMED) %>%
		st_drop_geometry() %>%
		rename(DIST = DISTMED)
}

#' @export
read_income_groups <- function(income_groups_file) {
	income_groups_file %>%
		read_csv() %>%
		mutate(
			inc_range = case_when(
				low <= 0 ~ paste("\u2264", label_comma(prefix = "$")(high)),
				is.infinite(high) ~ paste("\u2265", label_comma(prefix = "$")(low)),
				TRUE ~ paste0(
					label_comma(prefix = "$")(low),
					"\u2013",
					label_comma(prefix = "$")(high)
				)
			),
			inc_range = fct_reorder(inc_range, low)
		)
}

#' @export
read_distance_skims <- function(distance_skims_file) {
	distance_skims_file %>%
		read_all_omx("HBW") %>%
		rename(distance = HBW) %>%
		filter(distance < 1000) #External zones have distance 10000, we don't want those
}
