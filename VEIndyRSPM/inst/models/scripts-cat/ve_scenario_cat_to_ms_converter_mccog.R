#Date. 25 March 2026.  This version includes the alt powertrain.
## Nick Fisher updated with correct powertrains and popsim call

library(visioneval)
library(dplyr)

modelName <- "VERSPM-scenarios-cat-mccog"
newModelName <- "VERSPM-scenarios-ms-mccog-run13"

# Use Powertrain Scenarios
# Format
# Category level = list
# sub-list
# NAME - Name of the module
# ELEMLABEL - Name of the elemenet level use to append the directory name
# ALIAS - If module use as alias then name of the original package else ""
# DYNAMIC - If module used as dynamic module then name dynamic module specs
scen_alternatives <- list(
  "EV=2" = list(
    "NAME" = "VEPowertrainsAndFuelsMCCOG2024",
    "ELEMLABEL" = "E1",
    "ALIAS" = "VEPowertrainsAndFuelsIndy",
    "DYNAMIC" = ""
    ),
  "EV=3" = list(
    "NAME" = "VEPowertrainsAndFuelsMCCOGEVs",
    "ELEMLABEL" = "E2",
    "ALIAS" = "VEPowertrainsAndFuelsIndy",
    "DYNAMIC" = ""
    ),
  "AS=2" = list(
    "NAME" = "VEPopulationSimInputsYoung",
    "ELEMLABEL" = "S1",
    "ALIAS" = "",
    "DYNAMIC" = "VEPopulationSimDir"
    ),
  "AS=3" = list(
    "NAME" = "VEPopulationSimInputsOld",
    "ELEMLABEL" = "S2",
    "ALIAS" = "",
    "DYNAMIC" = "VEPopulationSimDir"
    )
)

ve_cat_model <- openModel(modelName)
ve_ms_model <- ve_cat_model$copy(newModelName, copyResults = FALSE, copyArchives = FALSE)

create_scenario_dirs <- function(structure_ls){
  # Get the name of the directory to create
  dir_name <- structure_ls$Name
  dir_cats <- structure_ls$ScenarioElements
  dir_cats_names <- names(structure_ls$ScenarioElements)
  dir_cats <- paste(dir_cats_names, dir_cats, sep = '')
  dir_cats <- paste0('_' ,paste(dir_cats, collapse ='', sep = ''))
  dir_path <- paste0(file.path(structure_ls$ModelDir, "scenarios", dir_name), dir_cats)
  dir_name <- paste0(dir_name, dir_cats)
  # dir_path <- file.path(structure_ls$ModelDir, "scenarios", dir_name)
  # cat_levels <- names(scen_alternatives)
  # dir_paths <- c()
  # for (cat_level in cat_levels) {
  #   if (grepl(cat_level, dir_path)) {
  #     dir_paths <- c(file.path(structure_ls$ModelDir,
  #                            "scenarios",
  #                            paste0(dir_name, 
  #                                   scen_alternatives[[cat_level]][["ELEMLABEL"]]),
  #                            dir_paths)
  #     )
  #   }
  # }
  # if (dir.exists(dir_path) & all(is.null(dir_paths))) {
  #   unlink(dir_path, recursive = TRUE)
  # }
  # if (!dir.exists(dir_path) & all(is.null(dir_paths))) {
  #   dir.create(dir_path)
  # }
  if (dir.exists(dir_path)) {
    unlink(dir_path, recursive = TRUE)
  }
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  # if(all(!is.null(dir_paths))){
  #   for (new_dir_path in dir_paths) {
  #     if (dir.exists(new_dir_path)) {
  #       unlink(new_dir_path, recursive = TRUE)
  #     }
  #     if (!dir.exists(new_dir_path)) {
  #       dir.create(new_dir_path)
  #     }
  #   }
  # }
  valid_dirs <- grepl("\\d$", structure_ls$InputPath)
  all_files <- unname(unlist(sapply(structure_ls$InputPath[valid_dirs], list.files, full.names=TRUE)))
  success <- file.copy(all_files, dir_path, overwrite = TRUE)
  if(sum(!success)>0) cat("Missing files: \n", paste0(all_files[!success], collapse = "\n"))
  invisible(TRUE)
}

create_scenario_configs <- function(structure_ls, scen_alternatives=NULL){
  # Get the name of the directory to create
  dir_name <- structure_ls$Name
  dir_cats <- structure_ls$ScenarioElements
  dir_cats_names <- names(structure_ls$ScenarioElements)
  dir_cats <- paste(dir_cats_names, dir_cats, sep = '')
  dir_cats <- paste0('_' ,paste(dir_cats, collapse ='', sep = ''))
  dir_path <- paste0(file.path(structure_ls$ModelDir, "scenarios", dir_name), dir_cats)
  dir_name <- paste0(dir_name, dir_cats)
  use_alias <- FALSE
  use_dynamic <- FALSE
  # For Alias
  OriginalModuleName <- ""
  AliasModuleName <- ""
  # For Dyanmic
  DynamicParam <- ""
  DynamicModule <- ""
  cat_levels <- names(scen_alternatives)
  for(cat_level in cat_levels){
    if (grepl(cat_level, dir_path)){
      if(scen_alternatives[[cat_level]][["ALIAS"]] != ""){
        use_alias <- TRUE
        OriginalModuleName <- scen_alternatives[[cat_level]][["ALIAS"]] 
        AliasModuleName <- scen_alternatives[[cat_level]][["NAME"]] 
      }
      if(scen_alternatives[[cat_level]][["DYNAMIC"]] != ""){
        use_dynamic <- TRUE
        DynamicParam <- scen_alternatives[[cat_level]][["DYNAMIC"]] 
        DynamicModule <- scen_alternatives[[cat_level]][["NAME"]] 
      }
    }
  }
  config_all <- list()
  config_all[[dir_name]] <- list(
    Scenario = structure_ls$Scenario,
    Description = structure_ls$Description,
    Dir = dir_name
  )
  if(use_alias){
    config_all[[dir_name]][[OriginalModuleName]] <- list(
      "AliasFor"=AliasModuleName
    )
  }
  if(use_dynamic){
    config_all[[dir_name]][[DynamicParam]] <- list(
      DynamicModule
    )
  }
  return(invisible(config_all))
}

check_scenario_combos <- function(scenario_ls){
  scenario_name <- scenario_ls$Name
  if(grepl("LU=1", scenario_name)|grepl("Transit=1", scenario_name)){ # Check reference scenario
    return(scenario_ls)
  } else if (grepl("LU=2.*Transit=2", scenario_name)){
    return(scenario_ls)
  } else if(grepl("LU=3.*Transit=3", scenario_name)){
    return(scenario_ls)
  } else if(grepl("LU=4.*Transit=4", scenario_name)){
    return(scenario_ls)
  } else if(grepl("LU=5.*Transit=5", scenario_name)){
    return(scenario_ls)
  } else {
    return(invisible(NULL))
  }
}

# Create scenario input directories and inputs.
setup_structure <- lapply(ve_ms_model$modelScenarios$modelStages, getSetup)
RestrictScenarioCombos <- FALSE
if(RestrictScenarioCombos){
  valid_setup_structure <- lapply(setup_structure, check_scenario_combos)
  valid_setup_structure <- valid_setup_structure[!sapply(valid_setup_structure, is.null)]
} else {
  valid_setup_structure <- setup_structure
}
lapply(valid_setup_structure, create_scenario_dirs)


# Create scenario config files
Config_ls <- list(
  StartFrom = setup_structure[[1]]$StartFrom,
  BaseScenario = setup_structure[[1]]$BaseScenario
)

Config_ls[["ModelStages"]] <- lapply(valid_setup_structure,create_scenario_configs, scen_alternatives=scen_alternatives)

Config_sub_ls <- Config_ls

## Filter to the desired models - generate unique combinations then filter the list
# Filter out by name
names <- c('AVS', 'ST', 'AT', 'Tel', 'AS', 'EV')
filter <- c()
for(i in 1:length(Config_sub_ls$ModelStages)){
  Config_ls2 <- Config_sub_ls$ModelStages[[i]]
  name <- sub('_.*', '', names(Config_ls2))
  contains_all <- all(sapply(names, grepl, x = name, ignore.case = T))
  if(contains_all == T){
    filter <- c(filter, i)
  }
}

Config_sub_ls$ModelStages <- Config_sub_ls$ModelStages[filter]

### Divide into sets of 50
Config_sub_ls$ModelStages <- Config_sub_ls$ModelStages[601:648]
configFile <- file.path(file.path(setup_structure[[1]]$ModelDir, "scenarios","visioneval.cnf"))
cat(configFile,"\n")
yaml_config <- yaml::as.yaml(Config_sub_ls)
yaml_config <- gsub("- ", "  ", yaml_config)
writeLines(yaml_config,configFile)

# Check for crashed models
files = list.files(paste0(getwd(), '/models/', newModelName, '/results'))
status_df = data.frame()

for(i in files){
  results_files = list.files(paste0(getwd(), '/models/', newModelName, '/results/', i))
  log_file = results_files[grep('Log', results_files)]
  lines = readLines(paste0(getwd(), '/models/', newModelName, '/results/', i, '/', log_file))
  final_line = lines[length(lines)]
  if(grepl('Model Run Complete', final_line)){
    s = 'Complete'
  } else{
    s = 'Incomplete'
  }
  temp_df = data.frame(ModelSubset = newModelName, scenario = i, status = s)
  status_df = rbind(status_df, temp_df)
}
