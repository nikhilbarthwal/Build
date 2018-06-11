% DECLARATIONS FOR PARSER

GLOBAL DOMAINS
	identifier = identifier(string, token)
	identifierlist = identifier*
	number = number(integer, token)
	variable = variable(string, token) ; str(string, token)
	str = str(string, token)
	%list = list(exprlist)
	data = str(string, token) ; list(datalist) ; number(integer, token)
	datalist = data*
	member = member(identifier, expr)
	minorlist = member*
	majorlist = minorlist*
	condition =
		member(expr, expr) ;
		cond_if1(condition, condition) ;
		cond_if2(condition, condition, condition) ;
		cond_and(condition, condition) ;
		cond_or(condition, condition) ;
		lt(expr, expr) ;
		le(expr, expr) ;
		gt(expr, expr) ;
		ge(expr, expr) ;
		eq(expr, expr) ;
		ne(expr, expr) ;
		call(identifier, exprlist) ;
		cond_not(condition)

	expr = 
		expr_if(condition, expr, expr) ;
		add(expr, expr) ;
		sub(expr, expr) ;
		multiply(expr, expr) ;
		modulus(expr, expr) ;
		divide(expr, expr) ;
		concat(expr, expr) ;
		join(expr, expr) ;
		add_list(expr, expr) ;
		generate(expr, expr) ;
		map1(majorlist, expr) ;
		map2(majorlist, expr, condition) ;
		enum(expr) ;
		call(identifier, exprlist) ;
		mcall(identifier, identifier, exprlist) ;
		number(integer, token) ;
		variable(string, token) ;
		str(string, token) ;
		list(exprlist)
	
	exprlist = expr*
	port = expr(expr) ; port(identifier)
	portlist = port*
	mstatement =
		begin ;
		alt ;
		end ;
		function(identifier, identifierlist) ;
		condition(identifier, identifierlist) ;
		export(identifier, expr) ;
		import(identifier, identifier) ;
		define(identifier) ;
		component(identifier, identifierlist, identifierlist) ;
		compose(identifier, identifierlist, identifierlist) ;
		slot(identifier, identifierlist, identifierlist) ;
		multislot(identifier, identifierlist, identifierlist) ;
		statement_or(mstatement, mstatement) ;
		statement_and(mstatement, mstatement) ;
		component1(identifier, identifier) ;
		component2(identifier, identifier, identifier) ;
		multibind(identifier, identifier, identifier, portlist, portlist) ;
		bind(identifier, identifier, identifier, portlist, portlist) ;
		member(identifier, expr) ;
		statement_if(condition);
		statement_else ;
		statement_if_then(condition, mstatement) ;
		statement_if_then_else(condition, mstatement, mstatement) ;
		set(identifier, expr) ;
		mset1(identifierlist, identifier, exprlist) ;
		mset2(identifierlist, identifier, identifier, exprlist) ;
		statement_while1(condition) ;
		statement_while2(condition, mstatement) ;
		statement_do ;
		statement_for1(majorlist) ;
		statement_for2(majorlist, mstatement) ;
		cond(condition) ;
		module(identifier) ;
		build(identifier) ;
		bfile(variable, identifier, exprlist, exprlist) ;
		filecopy(variable, variable, exprlist) ;
		bfolder(variable, exprlist) ;
		binclude(identifier, exprlist)
	
	pstatement =
		begin ;
		alt ;
		end  ;
		pinclude(str) ;
		target(identifier) ;
		project(identifier) ;
		set(identifier, data) ;
		report(identifier, identifier, identifier)
		
	
	statement = m(string, integer, mstatement) ; p(string, integer, pstatement)
	statementslist = statement*
	
	% ***** Project Structure ******
	mstructures = string*
	target = target(identifier, statementslist, statementslist)
	targetlist = target*
	% PStructure : Name, FileName, Module Structures, taget lists
	pstructure = project(identifier, string,  text, targetlist)
	