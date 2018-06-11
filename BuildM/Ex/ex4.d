

ex4.optdate ex4.trans_opt_date ex4.err ex4.c_date ex4.s_date ex4.pic_s_date ex4.il_date ex4.java_date : ex4.m \
	builtin.int \
	private_builtin.int \
	string.int \
	assoc_list.int2 \
	bitmap.int2 \
	bool.int2 \
	char.int2 \
	construct.int2 \
	deconstruct.int2 \
	enum.int2 \
	io.int2 \
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
	term.int2 \
	time.int2 \
	tree234.int2 \
	type_desc.int2 \
	univ.int2

ex4.mh ex4.mih : ex4.c


ifeq ($(findstring il,$(GRADE)),il)
ex4.module_dep : ex4.il
else
 ifeq ($(findstring java,$(GRADE)),java)
ex4.module_dep : jmercury/ex4.java
 else
ex4.module_dep : ex4.c
 endif
endif


ex4.date ex4.date0 : ex4.m \
	builtin.int3 \
	private_builtin.int3 \
	string.int3 \
	assoc_list.int3 \
	bitmap.int3 \
	bool.int3 \
	char.int3 \
	construct.int3 \
	deconstruct.int3 \
	enum.int3 \
	io.int3 \
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
	term.int3 \
	time.int3 \
	tree234.int3 \
	type_desc.int3 \
	univ.int3

ex4.date0 : ex4.m \
	builtin.int3 \
	private_builtin.int3 \
	string.int3 \
	assoc_list.int3 \
	bitmap.int3 \
	bool.int3 \
	char.int3 \
	construct.int3 \
	deconstruct.int3 \
	enum.int3 \
	io.int3 \
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
	term.int3 \
	time.int3 \
	tree234.int3 \
	type_desc.int3 \
	univ.int3



ex4.$O :  \
	time.mh \
	time.mh \
	bitmap.mh \
	bitmap.mh \
	string.mh \
	time.mh \
	io.mh \
	io.mh



ex4.pic_o :  \
	time.mh \
	time.mh \
	bitmap.mh \
	bitmap.mh \
	string.mh \
	time.mh \
	io.mh \
	io.mh


ex4.int0 : ex4.date0
	@:
ex4.int : ex4.date
	@:
ex4.int2 : ex4.date
	@:
ex4.int3 : ex4.date3
	@:
ex4.opt : ex4.optdate
	@:
ex4.trans_opt : ex4.trans_opt_date
	@:
