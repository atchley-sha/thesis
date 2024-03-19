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
		read_all_omx(names = c("DA", "auto", "transit", "nonmotor")) %>%
		mutate(
			origin, destination,
			drive_alone = DA, carpool = auto - DA, transit, nonmotor,
			.keep = "none"
		) %>%
		# read_all_omx(names = c("auto", "transit", "nonmotor")) %>%
		pivot_longer(-c(origin, destination), names_to = "mode", values_to = "trips") %>%
		filter(trips > 0) %>%
		mutate(trips = trips/100) #The trip matrices are multiplied by 100
}

mcc_read_trip_matrix <- function(omx_file) {
	omx_file %>%
		read_all_omx() %>%
		select(
			origin, destination,
			DA, SR2, SR3p, walk, bike,
			contains("LCL"), contains("BRT"), contains("COR"),
			contains("CRT"), contains("EXP"), contains("LRT")
		) %>%
		mutate(
			origin, destination,
			drive_alone = DA,
			sr2 = SR2,
			sr3 = SR3p,
			walk,
			bike,
			local_bus = rowSums(across(
				c(contains("LCL"), contains("BRT"), contains("COR")))),
			express_bus = rowSums(across(contains("EXP"))),
			crt = rowSums(across(contains("CRT"))),
			lrt = rowSums(across(contains("LRT"))),
			.keep = "none"
		) %>%
		pivot_longer(-c(origin, destination), names_to = "mode", values_to = "trips") %>%
		filter(trips > 0) %>%
		mutate(trips = trips/100) #The trip matrices are multiplied by 100
}

test_mcc_read_trip_matrix <- function(omx_file) {
	omx_file %>%
		read_all_omx()
}

combine_cube_se <- function(taz_se, taz_inc_groups) {
	taz_se %>%
		select(TAZ, med_income, pop = HHPOP, hh_size = HHSIZE, num_hh = TOTHH) %>%
		full_join(
			taz_inc_groups,
			join_by(TAZ)
		)
}

read_cube_rw_percentages <- function(rw_pct_file, jobe_code_transl) {
	rw_pct_file %>%
		read_csv() %>%
		left_join(jobe_code_transl, join_by(jobcode)) %>%
		select(-jobcode) %>%
		pivot_longer(
			-name,
			names_to = c("year", "type"), names_sep = "_",
			values_to = "pct")
}

get_cube_production_se <- function(trips, cube_se) {
	trips %>%
		group_by(purpose, mode, origin) %>%
		summarise(trips = sum(trips), .groups = "drop") %>%
		left_join(cube_se, join_by(origin == TAZ))
}

summarise_cube_transit_se <- function(se_trips) {
	se_trips %>%
		filter(mode == "transit") %>%
		group_by(purpose) %>%
		summarise(
			transit_trips = sum(trips),
			across(
				c(TOTHH, ALLEMP, med_income),
				\(x) matrixStats::weightedMedian(x, trips, na.rm = TRUE)),
			.groups = "drop"
		)

}

read_cube_remote_work_totals <- function(cube_remote_work_totals_file) {
	cube_remote_work_totals_file %>%
		read_csv() %>%
		filter(CO_NAME == "State of Utah") %>%
		select(YEAR, HBJ, Telecom) %>%
		rename(year = YEAR, wfh = HBJ, tc = Telecom) %>%
		pivot_longer(-year, names_to = "type", values_to = "pct")
}

plot_cube_remote_work_totals <- function(cube_remote_work_totals) {
	cube_remote_work_totals %>%
		mutate(type = case_match(
			type,
			"tc" ~ "Telecommute",
			"wfh" ~ "Work-From-Home"
		)) %>%
		ggplot(aes(x = year, y = pct, color = type)) +
		geom_line(linewidth = 1) +
		geom_vline(xintercept = c(2019, 2050), lty = "dotted") +
		scale_y_continuous(transform = "sqrt", labels = label_percent()) +
		scale_x_continuous(breaks = seq(1990,2060,10)) +
		labs(x = "Year", y = "Rate", color = element_blank())
}
