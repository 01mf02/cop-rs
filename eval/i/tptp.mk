# keep TPTP tgz file
.SECONDARY:

TPTP-%.tgz:
	wget http://www.tptp.org/TPTP/Archive/$@

TPTP-%: TPTP-%.tgz
	tar xzf $<
	tar xzf $@/$<
	rm $@/$<
