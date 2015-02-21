default:
	-rm -r live                                  # Remove old remains
	rsync --recursive -L  ./       live/	     # Copy all the "finished" files
	make -C live/src			     # Run tests + coverage checking on full code
	firefox live/src/hpc_index.html &            # Show the hpc results
	rsync --recursive -L  before/  live/         # Overwite some files with incomplete files for live coding
	emacs live/src/Expr.hs live/src/Middle.hs live/src/Typed.hs live/Plan &
