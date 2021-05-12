all: solved.tar.bz2 o-time.tar.bz2 o-stats.tar.bz2 o-proof.tar.bz2 o-stdout.tar.bz2

solved.tar.bz2:
	tar cjf $@ solved/
o-time.tar.bz2:
	find o/ -name "*.time"  | tar cjf $@ -T -
o-stats.tar.bz2:
	find o/ -name "*.stats" | tar cjf $@ -T -
o-proof.tar.bz2:
	find o/ -name "*.o"     | tar cjf $@ -T -
o-stdout.tar.bz2:
	find o/ -name "*.p"     | tar cjf $@ -T -
