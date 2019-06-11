all : snake.byte

snake.byte : IO.ml snake.ml
	ocamlc -ppx "ocalustre -i -m game_loop" graphics.cma unix.cma $? -dsource -o $@

run : snake.byte
	./$<

clean :
	rm -r *.cmi *.cmo snake.byte
