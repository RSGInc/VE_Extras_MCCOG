requirePackage(VEPopulationSim)


for(Year in getYears()) {
  runModule("ReadPopulationSimOutput",         "VEPopulationSim",       RunFor = "AllYears", RunYear = Year)
  runModule("AssignLifeCycle",                 "VESimHouseholds",       RunFor = "AllYears",    RunYear = Year)
  runModule("PredictHousing",                  "VELandUse",           RunFor = "AllYears",    RunYear = Year)
  runModule("LocateEmployment",                "VEPopulationSim",       RunFor = "AllYears",    RunYear = Year)
}
