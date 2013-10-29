all:
	gcc -c -I gcc/ -I generated_cfg/ -I rte/ -I inc/ -I stub/ src/*.c generated_cfg/*.c stub/*.c && mv *.o out/

clean:
	rm out/*
