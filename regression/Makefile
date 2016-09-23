TESTS=test001 test002 test003 test004 test005 test006

.PHONY:check $(TESTS)

check: $(TESTS)
$(TESTS): %: %.expr %.input
	../src/rc.byte $@.expr && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
