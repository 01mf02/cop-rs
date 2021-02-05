flyspeck-top.tar.bz2:
	wget http://cl-informatik.uibk.ac.at/~mfaerber/cop/$@ -O $@

flyspeck-top: flyspeck-top.tar.bz2
	tar xjf $<

flyspeck-meson.tar.bz2:
	wget http://cl-informatik.uibk.ac.at/~mfaerber/cop/$@ -O $@

flyspeck-meson: flyspeck-meson.tar.bz2
	tar xjf $<
