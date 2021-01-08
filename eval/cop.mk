include join.mk

TIME := /usr/bin/time --format '{"user": %U, "system": %S, "elapsed": "%E"}'
CHECK := [ $$? -eq 124 ]
COP := ../target/release/cop

$(COP): .FORCE
	cd .. && cargo build --release
.FORCE:

# Create an evaluation target for the connection prover.
#
# Arguments:
# 1. The name of the dataset to evaluate (to be found in `i/`).
# 2. The list of files contained in the dataset
#    (have to be of the format `i/$(1)/...`).
# 3. The timeout for the prover in seconds.
# 4. The arguments passed to the prover, without leading `--`s.
#
# For example,
#     $(eval $(call cop-target,bushy,$(BUSHY),1,cut conj))
# creates a target `o/bushy/1s/cop-cut-conj` that evaluates
# the dataset in `i/bushy` with a 1 second timeout,
# using the flags `--cut --conj`.
#
# See <https://www.gnu.org/software/make/manual/make.html#Eval-Function>
# for how the combination of `define`, `call`, and `eval` works.
define cop-target =
OUT = o/$(1)/$(3)s/cop$$(call join-with,,$$(patsubst %,-%,$(4)))

.PHONY: $$(OUT)
$$(OUT): $$(patsubst i/$(1)/%,$$(OUT)/%,$(2))
$$(OUT)/%: $$(COP) i/$(1)/%
	@mkdir -p "`dirname $$@`"
	-$$(TIME) -o "$$@.time" timeout $(3) \
	  $$^ --infs "$$@.infs"  -o "$$@.o" \
	  $$(patsubst %,--%,$(4)) > "$$@" || $$(CHECK)
endef
