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

BASE = led
PIC = 18f4620

all: $(BASE)_pure.hex

$(BASE)_pure.hex: $(BASE)_pure.asm
	gpasm -y $(BASE)_pure.asm

$(BASE)_pure.ml : $(BASE).ml 
	$$(ocamlfind ppx_tools/rewriter ../../ppx_lustre.native $(BASE).ml -o $(BASE)_pure.ml)

$(BASE)_pure $(BASE)_pure.asm: $(BASE)_pure.ml config.asm
	ocapic $(PIC) config.asm $(BASE)_pure.ml


simul1: $(BASE)_pure
	./$(BASE)_pure ocapic_dip40_simulator \
	  'ocapic_lcd_simulator 16x2 e=RC5 rs=RD3 rw=RC4 bus=PORTB'

simul2: $(BASE)_pure.hex
	ocasim $(BASE)_pure.hex ocapic_dip40_simulator \
	  'ocapic_lcd_simulator 16x2 e=RC5 rs=RD3 rw=RC4 bus=PORTB'

prog: $(BASE)_pure.hex
	picprog $(BASE)_pure.hex

clean:
	@rm -f *~ *.o *.cmo *.cmi *.hex *.cod *.lst $(BASE)_pure.asm $(BASE)_pure $(BASE)_pure.ml

.PHONY: all simul1 simul2 prog clean
