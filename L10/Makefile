default:
	-rm -r live                                   # Remove old remains
	rsync --recursive -L  ./       live/	     # Copy all the "finished" files
	rsync --recursive -L  before/  live/           # Overwite some of them with incomplete files
	emacs live/Plan live/src/ParserTests.hs live/src/Compiler/Properties.hs
