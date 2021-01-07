CHAINY := $(shell find i/chainy/ -type f | sort -R)
TIME = /usr/bin/time --format '{"user": %U, "system": %S, "elapsed": "%E"}'
COP = ../target/release/cop

../target/release/cop: .FORCE
	cd .. && cargo build --release
.FORCE:

o/chainy/1s/cop-cut-conj: $(CHAINY:i/chainy/%=\
o/chainy/1s/cop-cut-conj/%)
o/chainy/1s/cop-cut-conj/%: $(COP) i/chainy/%
	@mkdir -p "`dirname $@`"
	-$(TIME) -o "$@.time" timeout 1 $^ --infs "$@.infs" -o "$@.o" \
	  --cut --conj > "$@" || [ $$? -eq 124 ]

o/chainy/1s/cop-cutalt-conj: $(CHAINY:i/chainy/%=\
o/chainy/1s/cop-cutalt-conj/%)
o/chainy/1s/cop-cutalt-conj/%: $(COP) i/chainy/%
	@mkdir -p "`dirname $@`"
	-$(TIME) -o "$@.time" timeout 1 $^ --infs "$@.infs" -o "$@.o" \
	  --cut --cutalt --conj > "$@" || [ $$? -eq 124 ]
