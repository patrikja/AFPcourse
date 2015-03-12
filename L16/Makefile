default:
	-rm -r live                                  # Remove old remains
	cabal sandbox init
	cabal install
	rsync --recursive -L  ./       live/	     # Copy all the "finished" files
	rsync --recursive -L  before/  live/         # Overwite some files with incomplete files for live coding
	emacs live/Plan &
