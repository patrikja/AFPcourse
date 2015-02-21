default:
	-rm -r live                                  # Remove old remains
	rsync --recursive -L  ./       live/	     # Copy all the "finished" files
	rsync --recursive -L  before/  live/         # Overwite some of them with incomplete files for live coding
	emacs live/Plan live/src/ParserTests.hs live/src/Compiler/Properties.hs &
	make -C src
	firefox src/Compiler/hpc_index.html &
