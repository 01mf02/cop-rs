ifndef COP_MK
COP_MK := 1

include join.mk

TIME := /usr/bin/time --format '{"user": %U, "system": %S, "elapsed": "%E"}'
CHECK := [ $$? -eq 124 ]
MEANCOP := ../target/release/meancop

$(MEANCOP): .FORCE
	cargo build --release
.FORCE:

# Create an evaluation target for the connection prover meanCoP.
#
# Arguments:
# 1. The name of the dataset to evaluate (to be found in `i/`).
# 2. The list of files contained in the dataset
#    (have to be of the format `i/$(1)/...`).
# 3. The timeout for the prover in seconds.
# 4. The arguments passed to the prover.
#
# For example,
#     $(eval $(call meancop,bushy,$(BUSHY),1,--conj --cuts rex))
# creates a target `o/bushy/1s/meancop--conj--cutsrex` that evaluates
# the dataset in `i/bushy` with a 1 second timeout,
# using the flags `--conj --cuts rex`.
#
# See <https://www.gnu.org/software/make/manual/make.html#Eval-Function>
# for how the combination of `define`, `call`, and `eval` works.
define meancop =
OUT = o/$(1)/$(3)s/meancop$$(call join-with,,$(4))

.PHONY: $$(OUT)
$$(OUT): $$(patsubst i/$(1)/%,$$(OUT)/%,$(2))
$$(OUT)/%: $$(MEANCOP) i/$(1)/%
	@mkdir -p "`dirname $$@`"
	-TPTP=i/$(1) $$(TIME) -o "$$@.time" timeout $(3) \
	  $$^ --infs "$$@.infs"  -o "$$@.o" \
	  $(4) > "$$@" || $$(CHECK)
endef

STRATEGIES := \
  --conj \
  --conj.--cuts.r \
  --conj.--cuts.ei \
  --conj.--cuts.ex \
  --conj.--cuts.rei \
  --conj.--cuts.rex

undot = $(subst .,$(space),$(1))

endif
