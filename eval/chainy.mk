include cop.mk

CHAINY := $(shell find i/chainy/ -type f | sort -R)

$(eval $(call leancop,chainy,$(CHAINY),1,--conj))
$(eval $(call leancop,chainy,$(CHAINY),1,--conj --cutred))
$(eval $(call leancop,chainy,$(CHAINY),1,--conj --cutext deep))
$(eval $(call leancop,chainy,$(CHAINY),1,--conj --cutext shallow))
$(eval $(call leancop,chainy,$(CHAINY),1,--conj --cutred --cutext deep))
$(eval $(call leancop,chainy,$(CHAINY),1,--conj --cutred --cutext shallow))

$(eval $(call leancop,chainy,$(CHAINY),10,--conj))
$(eval $(call leancop,chainy,$(CHAINY),10,--conj --cutred))
$(eval $(call leancop,chainy,$(CHAINY),10,--conj --cutext deep))
$(eval $(call leancop,chainy,$(CHAINY),10,--conj --cutext shallow))
$(eval $(call leancop,chainy,$(CHAINY),10,--conj --cutred --cutext deep))
$(eval $(call leancop,chainy,$(CHAINY),10,--conj --cutred --cutext shallow))
