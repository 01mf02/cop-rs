include cop.mk

BUSHY := $(shell find i/bushy/ -type f | sort -R)

$(foreach s,$(STRATEGIES),$(eval $(call meancop,bushy,$(BUSHY),1, $(call undot,$(s)))))
$(foreach s,$(STRATEGIES),$(eval $(call meancop,bushy,$(BUSHY),10,$(call undot,$(s)))))
