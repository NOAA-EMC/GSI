#!/bin/bash
#====================================================
# Purpose:
#    Used by IAT GUI to run gribExtremes
# Author: Deyong Xu / RTi@JCSDA
# History:
#    2/3/2015, D. Xu / RTi@JCSDA , initial code.
#
#====================================================

source ge_gui.config
# ./run_template.sh
echo "$ENV_MODEL_2" >> abc
echo "$ENV_INPUT_1" >> abc
echo "$ENV_INPUT_2" >> abc
echo "$ENV_MAPAIR" >> abc
echo "$ENV_MAPSFC" >> abc
echo "$ENV_MAPUV" >> abc

