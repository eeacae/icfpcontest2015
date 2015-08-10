SOLVER=$(shell stack path --dist-dir)/build/solve/solve

play_icfp2015 : $(SOLVER)
	cp $(SOLVER) play_icfp2015

$(SOLVER) : clean
	stack setup
	stack build

clean :
	stack clean
	rm -f play_icfp2015

.PHONY: clean
