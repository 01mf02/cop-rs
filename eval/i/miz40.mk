miz40.tar.bz2: miz40.tar.bz2.sha512
	wget http://cl-informatik.uibk.ac.at/~mfaerber/cop/miz40.tar.bz2 -O $@
	sha512sum -c $<

miz40: miz40.tar.bz2
	tar xjf $<

miz40/eval-deps.a15: miz40
	$(MAKE) -C miz40

miz40-deps.a15: miz40/eval-deps.a15
	ln -s $< $@
