include cop.mk

BUSHY := $(shell find i/bushy/ -type f | sort -R)

$(eval $(call cop-target,bushy,$(BUSHY),1,--conj))
$(eval $(call cop-target,bushy,$(BUSHY),1,--conj --cutred))
$(eval $(call cop-target,bushy,$(BUSHY),1,--conj --cutext deep))
$(eval $(call cop-target,bushy,$(BUSHY),1,--conj --cutext shallow))
$(eval $(call cop-target,bushy,$(BUSHY),1,--conj --cutred --cutext deep))
$(eval $(call cop-target,bushy,$(BUSHY),1,--conj --cutred --cutext shallow))

$(eval $(call cop-target,bushy,$(BUSHY),10,--conj))
$(eval $(call cop-target,bushy,$(BUSHY),10,--conj --cutred))
$(eval $(call cop-target,bushy,$(BUSHY),10,--conj --cutext deep))
$(eval $(call cop-target,bushy,$(BUSHY),10,--conj --cutext shallow))
$(eval $(call cop-target,bushy,$(BUSHY),10,--conj --cutred --cutext deep))
$(eval $(call cop-target,bushy,$(BUSHY),10,--conj --cutred --cutext shallow))
