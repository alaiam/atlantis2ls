# atlantis2ls
Atlantis tools (2ls) to couple Atlantis with calibrar.

## Functions
There are three main types of functions:

- Functions to read Atlantis outputs and convert them into lists that calibrar can use (targets per species, with values over time).  
  These functions include: read_outputs, read_biomass, read_catch, read_abund, read_waa, read_Nbiomass, ...

- A function to run Atlantis.  
  This function is: run_atlantis

- Functions to edit Atlantis parameters.  
  These functions include: edit_BHalpha, edit_BHbeta, edit_KDENR, edit_mfc, edit_mq, edit_mum, ...

## Integration with calibrar
To integrate with calibrar, these functions are called successively within a single function during model calibration.
The first step modifies the input files, the second runs the model, and the third processes the outputs.
To see an example, please refer to this repository:  
https://github.com/alaiam/Atlantis_Calibration/blob/main/runModel_Atlantis.R
  


