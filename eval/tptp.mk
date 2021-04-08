include cop.mk

TPTP630FOF := $(shell find i/TPTP-v6.3.0/ -type f -name "*+?.p" | sort -R)

$(foreach s,$(STRATEGIES),$(eval $(call meancop,TPTP-v6.3.0,$(TPTP630FOF),1, $(call undot,$(s)))))
$(foreach s,$(STRATEGIES),$(eval $(call meancop,TPTP-v6.3.0,$(TPTP630FOF),10,$(call undot,$(s)))))
