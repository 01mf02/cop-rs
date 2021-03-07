include cop.mk

MIZ40DEPSA15 := $(shell find i/miz40-deps.a15/ -type f -name "*.p" | sort -R)

$(foreach s,$(STRATEGIES),$(eval $(call meancop,miz40-deps.a15,$(MIZ40DEPSA15),1, $(call undot,$(s)))))
$(foreach s,$(STRATEGIES),$(eval $(call meancop,miz40-deps.a15,$(MIZ40DEPSA15),10,$(call undot,$(s)))))
