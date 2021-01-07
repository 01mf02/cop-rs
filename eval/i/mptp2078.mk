MPTP2078:
	git clone https://github.com/JUrban/$@ && cd $@ && git checkout bc58295

bushy: MPTP2078
	ln -s $</$@

chainy: MPTP2078
	7za x -so $</chainy.tar.7z | tar xf -
