include cop.mk

BUSHY := $(shell find i/bushy/ -type f | sort -R)

$(eval $(call leancop,bushy,$(BUSHY),1,--conj))
$(eval $(call leancop,bushy,$(BUSHY),1,--conj --cutred))
$(eval $(call leancop,bushy,$(BUSHY),1,--conj --cutext deep))
$(eval $(call leancop,bushy,$(BUSHY),1,--conj --cutext shallow))
$(eval $(call leancop,bushy,$(BUSHY),1,--conj --cutred --cutext deep))
$(eval $(call leancop,bushy,$(BUSHY),1,--conj --cutred --cutext shallow))

$(eval $(call leancop,bushy,$(BUSHY),10,--conj))
$(eval $(call leancop,bushy,$(BUSHY),10,--conj --cutred))
$(eval $(call leancop,bushy,$(BUSHY),10,--conj --cutext deep))
$(eval $(call leancop,bushy,$(BUSHY),10,--conj --cutext shallow))
$(eval $(call leancop,bushy,$(BUSHY),10,--conj --cutred --cutext deep))
$(eval $(call leancop,bushy,$(BUSHY),10,--conj --cutred --cutext shallow))
