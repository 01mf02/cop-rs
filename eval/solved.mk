# collect solved files for a given prover
solved/%: o/%
	@mkdir -p "`dirname $@`"
	# "%P" prints the filename without the leading starting-point (here: $<)
	find $< -name "*.o" -printf "%P\n" | sed 's/\.o$$//' | sort > "$@"
