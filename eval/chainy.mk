include cop.mk

CHAINY := $(shell find i/chainy/ -type f | sort -R)

$(eval $(call cop-target,chainy,$(CHAINY),1,conj))
$(eval $(call cop-target,chainy,$(CHAINY),1,conj cut))
$(eval $(call cop-target,chainy,$(CHAINY),1,conj cut cutalt))

$(eval $(call cop-target,chainy,$(CHAINY),10,conj))
$(eval $(call cop-target,chainy,$(CHAINY),10,conj cut))
$(eval $(call cop-target,chainy,$(CHAINY),10,conj cut cutalt))
