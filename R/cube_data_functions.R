#' @export
read_cube_taz_se_file <- function(cube_taz_se_file) {
	cube_taz_se_file %>%
		read_csv() %>%
		rename(TAZ = `;TAZID`, med_income = AVGINCOME)
}

#' @export
read_cube_taz_inc_groups_file <- function(cube_taz_inc_groups_file) {
	cube_taz_inc_groups_file %>%
		read_csv() %>%
		select(TAZ = Z, INC1:INC4) %>%
		rename_with(\(x) str_replace(x, "INC", "inc_group_"))
}

#' @export
read_trip_matrix <- function(omx_file) {
	omx_file %>%
		read_all_omx(names = c("auto", "transit", "nonmotor")) %>%
		pivot_longer(-c(origin, destination), names_to = "mode", values_to = "trips") %>%
		filter(trips > 0) %>%
		mutate(trips = trips/100) #The trip matrices are multiplied by 100
}

combine_cube_se <- function(taz_se, taz_inc_groups) {
	taz_se %>%
		select(TAZ, med_income, pop = HHPOP, hh_size = HHSIZE, num_hh = TOTHH) %>%
		full_join(
			taz_inc_groups,
			join_by(TAZ)
		)
}

read_cube_tc_percentages <- function(tc_pct_file, jobe_code_transl) {
	tc_pct_file %>%
		read_csv() %>%
		left_join(jobe_code_transl, join_by(jobcode)) %>%
		select(name, wfrc_2019, wfrc_2050)
}
