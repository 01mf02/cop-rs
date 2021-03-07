include cop.mk

#FLYMESON := $(shell find i/flyspeck-meson/ -type f -name "*f*.p" | sort -R)
FLYTOP   := $(shell find i/flyspeck-top/   -type f -name "*f*.p" | sort -R)

$(foreach s,$(STRATEGIES),$(eval $(call meancop,flyspeck-top,$(FLYTOP),1, $(call undot,$(s)))))
$(foreach s,$(STRATEGIES),$(eval $(call meancop,flyspeck-top,$(FLYTOP),10,$(call undot,$(s)))))
