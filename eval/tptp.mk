include cop.mk

TPTP630FOF := $(shell find i/TPTP-v6.3.0/ -type f -name "*+?.p" | sort -R)

$(eval $(call cop-target,TPTP-v6.3.0,$(TPTP630FOF),1,--conj))
$(eval $(call cop-target,TPTP-v6.3.0,$(TPTP630FOF),1,--conj --cut deep))
$(eval $(call cop-target,TPTP-v6.3.0,$(TPTP630FOF),1,--conj --cut shallow))

$(eval $(call cop-target,TPTP-v6.3.0,$(TPTP630FOF),10,--conj))
$(eval $(call cop-target,TPTP-v6.3.0,$(TPTP630FOF),10,--conj --cut deep))
$(eval $(call cop-target,TPTP-v6.3.0,$(TPTP630FOF),10,--conj --cut shallow))
