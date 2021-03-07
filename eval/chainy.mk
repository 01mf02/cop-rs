include cop.mk

CHAINY := $(shell find i/chainy/ -type f | sort -R)

$(foreach s,$(STRATEGIES),$(eval $(call meancop,chainy,$(CHAINY),1, $(call undot,$(s)))))
$(foreach s,$(STRATEGIES),$(eval $(call meancop,chainy,$(CHAINY),10,$(call undot,$(s)))))
