BUSHY := $(shell find i/bushy/ -type f | sort -R)
TIME = /usr/bin/time --format '{"user": %U, "system": %S, "elapsed": "%E"}'

../target/release/cop: .FORCE
	cd .. && cargo build --release
.FORCE:

o/bushy/1s/cop-cut-conj: $(BUSHY:i/bushy/%=\
o/bushy/1s/cop-cut-conj/%)
o/bushy/1s/cop-cut-conj/%: ../target/release/cop i/bushy/%
	@mkdir -p "`dirname $@`"
	-$(TIME) -o "$@.time" timeout 1 $^ --infs "$@.infs" -o "$@.o" \
	  --cut --conj > "$@"

o/bushy/1s/cop-cutalt-conj: $(BUSHY:i/bushy/%=\
o/bushy/1s/cop-cutalt-conj/%)
o/bushy/1s/cop-cutalt-conj/%: ../target/release/cop i/bushy/%
	@mkdir -p "`dirname $@`"
	-$(TIME) -o "$@.time" timeout 1 $^ --infs "$@.infs" -o "$@.o" \
	  --cut --cutalt --conj > "$@"
