include cop.mk

TPTP630FOF := $(shell find i/TPTP-v6.3.0/ -type f -name "*+?.p" | sort -R)

$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),1,--conj))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),1,--conj --cutred))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),1,--conj --cutext deep))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),1,--conj --cutext shallow))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),1,--conj --cutred --cutext deep))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),1,--conj --cutred --cutext shallow))

$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),10,--conj))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),10,--conj --cutred))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),10,--conj --cutext deep))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),10,--conj --cutext shallow))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),10,--conj --cutred --cutext deep))
$(eval $(call leancop,TPTP-v6.3.0,$(TPTP630FOF),10,--conj --cutred --cutext shallow))
