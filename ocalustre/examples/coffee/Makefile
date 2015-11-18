###########################################################################
##                                                                       ##
##                                OCaPIC                                 ##
##                                                                       ##
##                             Benoit Vaugon                             ##
##                                                                       ##
##    This file is distributed under the terms of the CeCILL license.    ##
##    See file ../../LICENSE-en.                                         ##
##                                                                       ##
###########################################################################

BASE = coffee
PIC = 18f4620

all: $(BASE)_pure.hex

$(BASE)_pure.hex: $(BASE)_pure.asm
	gpasm -y $(BASE)_pure.asm

$(BASE)_pure.ml : $(BASE).ml 
	$$(ocamlfind ppx_tools/rewriter ../../ppx_lustre.native $(BASE).ml -o $(BASE)_pure.ml)

$(BASE)_pure $(BASE)_pure.asm: $(BASE)_pure.ml
	ocapic $(PIC) -stack-size 3 -heap-size 6 $$(ocapic -where)/default-config.asm $(BASE)_pure.ml


simul1: $(BASE)_pure
	./$(BASE)_pure ocapic_dip40_simulator 'ocapic_circuit_simulator circuit.txt'

simul2: $(BASE)_pure.hex
	ocasim $(BASE)_pure.hex 'ocapic_circuit_simulator circuit.txt'

prog: $(BASE)_pure.hex
	picprog $(BASE)_pure.hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE)_pure.asm $(BASE)_pure $(BASE)_pure.ml

.PHONY: all simul1 simul2 prog clean
