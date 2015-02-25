default:
	-rm -r live                                  # Remove old remains
	rsync --recursive -L  ./       live/	     # Copy all the "finished" files
	make -C live/AgdaTutorial		     # Run type checking of the full tutorial
	rsync --recursive -L  before/  live/         # Overwite some files with incomplete files for live coding
	emacs live/AgdaTutorial/Basics.hs live/AgdaTutorial/Basics.hcr live/AgdaTutorial/Basics_shortnames.hcr live/AgdaTutorial/Basics.agda live/AgdaTutorial/Datatypes.agda live/AgdaTutorial/CurryHoward.agda live/AgdaTutorial/Families.agda live/AgdaTutorial/Filter.agda live/AgdaTutorial/Modules.agda live/AgdaTutorial/Records.agda live/Plan
