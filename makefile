#Makefiles check if prereqs have changed or exist, then run bash code to generate riles
#All paths relative from makefile directory
#Ususal format consists of target : prereqs, followed by a tabbed recipe. Targets Targets
#file output by a program, or an action to perform (a phony target). Prequsities are inputs that
#change the target. Recipes are actions that make performs - ALL MUST BE tabbed
#They typically create  a target if any of the prereqs changes. 
#Recipes consist of rules to apply to the assoicated target, typically  a command to recreate the file from input
#Phony targets (preceded by .PHONY) are paried with prereqs. Calling make on a phony evaluates every file with one of those prereqs. This allows evaluation
#of many files in one command.
#Very useful to have an all target that depends on everything, so all can be recreated in one command
#Also "clean" that deletes all files
#Make evaluates the first target entered, then every prereusitie in sequence., executing the rule for each
#This means changes propagate downstream from the roots of the prereuisite chain
##Good practice to  create OBJS variable (or similar) containing all makefile objects and expand it where needed
###Rules can contain any bash command - mv, rm, etc.
####Implict rules apply to files based on name patterns
####
#Problem: R scripts run from maekfiles are run in the makefile's WD
#For RMarkdoesn, however, R's WD is the Markdown's WD since RMDs run in a clean environment. Modify directory references accordingly.
#
#
#

