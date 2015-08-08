NAME = lustre
all:
	camlp5o pa_extend.cmo q_MLast.cmo pr_o.cmo pa_$(NAME).ml \
		-o pa_$(NAME).ppo -loc loc
	camlp5o pa_extend.cmo q_MLast.cmo pa_$(NAME).ml \
		-o pa_$(NAME).ast -loc loc
	ocamlc -c -I /home/steven/.opam/4.02.3/lib/camlp5 -pp 'camlp5o pa_extend.cmo q_MLast.cmo -loc loc' \
		-dtypes pa_$(NAME).ml
	camlp5o -I . pr_o.cmo pa_$(NAME).cmo prog.ml -o prog.ppo
	camlp5o -I . pr_r.cmo pa_$(NAME).cmo prog.ml -o prog.ppr
	ocamlopt -dtypes -o prog -pp 'camlp5o -I . pa_$(NAME).cmo' prog.ml
	caml2html -t -ln pa_$(NAME).ml
	caml2html -t -ln prog.ml

clean:
	rm -f prog *.ppo *.ppr *.cmo *.cmi *.o *.cmx *.ast *~ *.ml.html
