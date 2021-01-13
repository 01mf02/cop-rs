include cop.mk

BUSHY := $(shell find i/bushy/ -type f | sort -R)

$(eval $(call cop-target,bushy,$(BUSHY),1,--conj))
$(eval $(call cop-target,bushy,$(BUSHY),1,--conj --cut deep))
$(eval $(call cop-target,bushy,$(BUSHY),1,--conj --cut shallow))

$(eval $(call cop-target,bushy,$(BUSHY),10,--conj))
$(eval $(call cop-target,bushy,$(BUSHY),10,--conj --cut deep))
$(eval $(call cop-target,bushy,$(BUSHY),10,--conj --cut shallow))
