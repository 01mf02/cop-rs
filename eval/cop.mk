ifndef COP_MK
COP_MK := 1

include join.mk

ifndef USE_SOLVED
# ignore errors, such as stack overflows
MODE = -
TIMEOUT = timeout $(1)
endif

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
# If the variable USE_SOLVED is defined, then timeout is disabled and
# only problems known to be solved (via the solved/ directory) are tried.
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
# Be very careful not to use any variables defined here inside recipes!
# Due to expansion rules, the last value that has been assigned to the variable
# will be used for *any* instance of the recipe!
# However, using these variables outside of recipes is fine.
CFG = $(1)/$(3)s/meancop$$(call join-with,,$(4))
OUT = o/$$(CFG)

ifndef USE_SOLVED
$$(OUT): $$(patsubst i/$(1)/%,$$(OUT)/%,$(2))
else
$$(OUT): $$(patsubst %,$$(OUT)/%,$$(shell cat solved/$$(CFG)))
endif

.PHONY: $$(OUT)
$$(OUT)/%: $$(MEANCOP) i/$(1)/%
	@mkdir -p "`dirname $$@`"
	$$(MODE) TPTP=i/$(1) $$(TIME) -o "$$@.time" $$(call TIMEOUT,$(3)) \
	  $$^ --stats "$$@.stats" -o "$$@.o" \
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
