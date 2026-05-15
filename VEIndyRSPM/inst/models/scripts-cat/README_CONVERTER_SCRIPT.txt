To use the converter script and convert the cat model to multiscenario format, install all relevant packages and copy the script to the "runtime" folder and run from there.

Packages required include:
- VEPowertrainsAndFuelsIndy
- VEPowertrainsAndFuelsMCCOG2024
- VEPowertrainsAndFuelsMCCOGEVS
- VEPopulationSim

Input folders required:
- VEPopulationSimInputsYoung
- VEPopulationSimInputsOld

Line 205 is used to divide the models into groups, edit numbers within the brackets.
Copy of line 205 below:
Config_sub_ls$ModelStages <- Config_sub_ls$ModelStages[601:648]