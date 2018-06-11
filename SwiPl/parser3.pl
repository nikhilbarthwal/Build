%:-module(parser, [class/4, define/2, parse/4]).

:- use_module(generator).

cl :- main_class.
implement :- main_implement.

class('parse_project', 'project', ['core', 'lexer', 'parse_utils', 'errors'], 'Convert Token Stream into Project Item').

%type (Name, parameter), dummy is automatically added for error control
% parameter = be term(name, [parameters]) or list('type-name'), primitive(token type), type(name) or type (name, Description), anytoken(Description)

%define('project', term('p', [type('token', 'ProjectName'), list('p_pair', 'ProjectArgs'), list('token', 'BuildName'), list('token', 'BuildArgs'), list('p_statement','MainBlock'), list('p_group', 'GroupList')])).
define('project', term('p', [type('token', 'ProjectName'), list('p_pair', 'ProjectArgs'), list('token', 'BuildName'), list('token', 'BuildArgs')])).

define('p_pair', term('var',[type('token','VarName'), type('token','VarValue')])).

define('p_group', term('p_grp', [type('token', 'GroupName'), list('token', 'ArgsList'), list('p_statement', 'GroupBlock')])).

% feature Name1.Name2.Name3.Name4(Arg1, Arg2, Arg3, Arg4) in "featurefile"
define('p_statement', term('feature', [ type('token', 'FileName'), list('token', 'NameSpace'), list('token', 'ArgsList')])).

% group Name(Arg1, Arg2, Arg3, Arg4)
define('p_statement', term('group', [ type('token', 'GroupName'), list('token', 'ArgsList')])).

% project Name(Arg1, Arg2, ...) in "project file"
define('p_statement', term('project', [ type('token', 'FileName'), type('token', 'ProjectName'), list('token', 'ArgsList')])).

% ------------------------------------------------------------------

%parse('project', 'project', [ readtokens('identifier1', 'PN'), match('open1'), readtokens('project_pairs', 'PA'), optmatch('white'), match('close1'), optmatch('white'),	match('symbols','='), optmatch('white'), readtokens('namespace', 'BN'), match('open1'), readtokens('build_vars', 'BA'), optmatch('white'), match('close1'), readtokens('endline'), match('identifier1', 'End'), match('endline')], 'p(PN, PA, BN, BA)').
parse('project', 'project', [ readtokens('identifier1', 'PN'), match('open1'), readtokens('project_pairs', 'PA'), optmatch('white'), match('close1'), optmatch('white'),	match('symbols','='), optmatch('white'), readtokens('namespace', 'BN'), match('open1'), readtokens('build_vars', 'BA'), optmatch('white'), match('close1'), readtokens('endline'), match('identifier1', 'End'), match('endline'), readtokens_end], 'p(PN, PA, BN, BA)').
parse('project', 'project', error1, 'Unable to understand the project').

parse('project_pairs', 'p_pair*', [readtokens('p_pair', 'H'), optmatch('white'), match('symbols',','), readtokens('project_pairs', 'T')], '[H|T]').
parse('project_pairs', 'p_pair*', [readtokens('p_pair', 'H')], '[H]').

parse('p_pair', 'p_pair', [optmatch('white'), readtokens('identifier1', 'N'), match('symbols','='), readtokens('p_value', 'V')], 'var(N, V)').

parse('p_value', 'token', [optmatch('white'), readtokens('identifier0', 'X')], 'X').

parse('statementlist', 'p_statement*', [readtokens('statement', 'H'), match('endline'), readtokens('statementlist', 'T')], '[H|T]').
parse('statementlist', 'p_statement*', [readtokens('statement', 'H')], '[H]').

parse('build_vars', 'token*', [readtokens('b_value', 'H'), optmatch('white'), match('symbols',','), readtokens('build_vars', 'T')], '[H|T]').
parse('build_vars', 'token*', [readtokens('b_value', 'H')], '[H]').

parse('b_value', 'token', [optmatch('white'), readtokens('identifier0', 'X')], 'X').
parse('b_value', 'token', [optmatch('white'), readtokens('identifier1', 'X')], 'X').

parse('namespace', 'token*', [readtokens('identifier1', 'H'), match('symbols','.'), readtokens('namespace', 'T')], '[H|T]').
parse('namespace', 'token*', [readtokens('identifier1', 'H')], '[H]').

parse('grouplist', 'p_group*', [readtokens('group', 'H'), readtokens('grouplist', 'T')], '[H|T]').
parse('grouplist', 'p_group*', readtokens_end, '[]').

parse('group', 'p_group', [readtokens('identifier1', 'N'), readtokens('open1'), readtokens('argslist', 'A'), optmatch('white'), match('close1'), match('endline'), readtokens('statementlist', 'S'), match('identifier1', 'End'), match('endline')], 'p_grp(N,A,S)').

parse('argslist', 'token*', [optmatch('white'), readtokens('identifier1', 'H'), optmatch('white'), match('symbols',','), readtokens('argslist', 'T')], '[H|T]').
parse('argslist', 'token*', [optmatch('white'), readtokens('identifier1', 'H')], '[H]').

parse('statement', 'p_statement', [match('identifier0','feature'), match('white'), readtokens('namespace', 'S'), match('open1'), readtokens('build_vars', 'A'), optmatch('white'),readtokens('close1'), match('white'), match('identifier0','in'), match('white'), readtokens('str', 'F')], 'feature(F, S, A)').
parse('statement', 'p_statement', [match('identifier0','group'), match('white'), readtokens('identifier1', 'N'), readtokens('open1'), readtokens('build_vars', 'A'), optmatch('white'), readtokens('close1')], 'group(N, A)').
parse('statement', 'p_statement', [match('identifier0','project'), match('white'), readtokens('identifier1', 'N'), readtokens('open1'), readtokens('build_vars', 'A'), optmatch('white'), readtokens('close1'), match('white'), match('identifier0','in'), match('white'), readtokens('str', 'F')], 'project(F,N, A)').
parse('statement', 'p_statement', error2, 'Unable to understand the statement').


