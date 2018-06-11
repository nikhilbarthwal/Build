

ex1.optdate ex1.trans_opt_date ex1.err ex1.c_date ex1.s_date ex1.pic_s_date ex1.il_date ex1.java_date : ex1.m \
	builtin.int \
	ex4.int \
	io.int \
	private_builtin.int \
	assoc_list.int2 \
	bitmap.int2 \
	bool.int2 \
	char.int2 \
	construct.int2 \
	deconstruct.int2 \
	enum.int2 \
	list.int2 \
	map.int2 \
	maybe.int2 \
	ops.int2 \
	pair.int2 \
	pretty_printer.int2 \
	rtti_implementation.int2 \
	set.int2 \
	set_ordlist.int2 \
	stream.int2 \
	string.int2 \
	term.int2 \
	time.int2 \
	tree234.int2 \
	type_desc.int2 \
	univ.int2

ex1.mh ex1.mih : ex1.c


ifeq ($(findstring il,$(GRADE)),il)
ex1.module_dep : ex1.il
else
 ifeq ($(findstring java,$(GRADE)),java)
ex1.module_dep : jmercury/ex1.java
 else
ex1.module_dep : ex1.c
 endif
endif


ex1.date ex1.date0 : ex1.m \
	builtin.int3 \
	ex4.int3 \
	io.int3 \
	private_builtin.int3 \
	assoc_list.int3 \
	bitmap.int3 \
	bool.int3 \
	char.int3 \
	construct.int3 \
	deconstruct.int3 \
	enum.int3 \
	list.int3 \
	map.int3 \
	maybe.int3 \
	ops.int3 \
	pair.int3 \
	pretty_printer.int3 \
	rtti_implementation.int3 \
	set.int3 \
	set_ordlist.int3 \
	stream.int3 \
	string.int3 \
	term.int3 \
	time.int3 \
	tree234.int3 \
	type_desc.int3 \
	univ.int3

ex1.date0 : ex1.m \
	builtin.int3 \
	ex4.int3 \
	io.int3 \
	private_builtin.int3 \
	assoc_list.int3 \
	bitmap.int3 \
	bool.int3 \
	char.int3 \
	construct.int3 \
	deconstruct.int3 \
	enum.int3 \
	list.int3 \
	map.int3 \
	maybe.int3 \
	ops.int3 \
	pair.int3 \
	pretty_printer.int3 \
	rtti_implementation.int3 \
	set.int3 \
	set_ordlist.int3 \
	stream.int3 \
	string.int3 \
	term.int3 \
	time.int3 \
	tree234.int3 \
	type_desc.int3 \
	univ.int3



ex1.$O :  \
	time.mh \
	time.mh \
	bitmap.mh \
	bitmap.mh \
	string.mh \
	time.mh \
	io.mh \
	io.mh



ex1.pic_o :  \
	time.mh \
	time.mh \
	bitmap.mh \
	bitmap.mh \
	string.mh \
	time.mh \
	io.mh \
	io.mh


ex1.int0 : ex1.date0
	@:
ex1.int : ex1.date
	@:
ex1.int2 : ex1.date
	@:
ex1.int3 : ex1.date3
	@:
ex1.opt : ex1.optdate
	@:
ex1.trans_opt : ex1.trans_opt_date
	@:
