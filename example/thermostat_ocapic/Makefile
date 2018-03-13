
BASE = thermo_gen
PIC = 18f4620

all: $(BASE).hex

$(BASE).hex: $(BASE).asm
	gpasm -y $(BASE).asm

$(BASE) $(BASE).asm: $(BASE).ml
	ocapic $(PIC) -stack-size 3 -heap-size 11 -gc mark_and_compact -ppx "ocalustre -v" -dsource  $$(ocapic -where)/default-config.asm $(BASE).ml


simul1: $(BASE)
	./$(BASE) ocapic_dip40_simulator 'ocapic_circuit_simulator circuit.txt'

simul2: $(BASE).hex
	ocasim $(BASE).hex 'ocapic_circuit_simulator circuit.txt'

prog: $(BASE).hex
	picprog $(BASE).hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE).asm $(BASE)

.PHONY: all simul1 simul2 prog clean
