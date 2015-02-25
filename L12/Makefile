default:
	-rm -r live                                  # Remove old remains
	rsync --recursive -L  ./       live/	     # Copy all the "finished" files
	make -C live/AgdaTutorial		     # Run type checking of the full tutorial
	rsync --recursive -L  before/  live/         # Overwite some files with incomplete files for live coding
	emacs live/Plan live/AgdaTutorial/{Basics.hs,Basics.hcr,Basics_shortnames.hcr,Basics.agda,Datatypes.agda,CurryHoward.agda,Families.agda,Filter.agda,Modules.agda,Records.agda}
