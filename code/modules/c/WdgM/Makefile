all: compile cp-hfiles
coverage: coverage-compile cp-hfiles

compile:
	gcc -c -I gcc/ -I generated_cfg/ -I rte/ -I inc/ -I stub/ src/*.c generated_cfg/*.c stub/*.c && mv *.o out/

cp-hfiles:
	cd eqc/h_files/ && \
	./gen_symlinks_to_h-files_for_eqc.sh

coverage-compile:
	cov01.exe -1 && \
	/cygdrive/c/Program\ Files\ \(x86\)/BullseyeCoverage/bin/i686-pc-cygwin-gcc.exe -c -I gcc/ -I generated_cfg/ -I rte/ -I inc/ -I stub/ src/*.c generated_cfg/*.c stub/*.c && mv *.o out/

clean:
	rm out/*
