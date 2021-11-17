include cop.mk

TPTP630 := $(shell find i/TPTP-v6.3.0/ -type f -name "*[+-]?.p" | sort -R)

$(foreach s,$(STRATEGIES),$(eval $(call meancop,TPTP-v6.3.0,$(TPTP630),1, $(call undot,$(s)))))
$(foreach s,$(STRATEGIES),$(eval $(call meancop,TPTP-v6.3.0,$(TPTP630),10,$(call undot,$(s)))))
