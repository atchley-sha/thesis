#### Setup #############################################################

library(targets)
library(tarchetypes)

package_list <- c(
	"qs",
	"tidyverse",
	"scales",
	"sf",
	"omxr",
	"ggspatial",
	"ggalluvial",
	"ggrepel",
	"DiagrammeR",
	"od"
)

tar_option_set(
	packages = package_list
	# memory = "transient",
	# garbage_collection = TRUE,
	# format = "qs",
)

tar_source("R")

tar_seed_set(34985723)

#### List targets ######################################################

shared_data_targets <- tar_plan(
	tar_file(taz_file, "data/WFRC_TAZ.geojson"),
	taz = read_taz_file(taz_file),
	distsml = get_sml_districts_from_taz_file(taz_file),
	distmed = get_med_districts_from_taz_file(taz_file),
	taz_distsml_transl = make_taz_distsml_transl(taz_file),
	taz_distmed_transl = make_taz_distmed_transl(taz_file),

	tar_file(income_groups_file, "data/income_groups.csv"),
  income_groups = read_income_groups(income_groups_file),

	tar_file(distance_skims_file, "data/skm_DY_Dist.omx"),
	distances = read_distance_skims(distance_skims_file),


)

tbm_data_targets <- tar_plan(

)

abm_data_targets <- tar_plan(

)

#### Run all targets ###################################################

tar_plan(
	shared_data_targets,
	tbm_data_targets,
	abm_data_targets,
)
