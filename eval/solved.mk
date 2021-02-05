# collect solved files for a given prover
solved/%: o/%
	@mkdir -p "`dirname $@`"
	find $< -name "*.o" | xargs basename -s .o | sort > "$@"
