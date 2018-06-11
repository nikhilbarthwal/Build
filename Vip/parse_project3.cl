% ***** class parse_project : Convert Token Stream into Project Item *****

class parse_project
    open core, lexer, parse_utils, errors
domains
    project =  dummy ;
        p(token ProjectName, p_pair* ProjectArgs).

    p_pair =  dummy ;
        var(token VarName, token VarValue).

    p_group =  dummy ;
        p_grp(token GroupName, token* ArgsList, p_statement* GroupBlock).

    p_statement =  dummy ;
        feature(token FileName, token* NameSpace, token* ArgsList) ;
        group(token GroupName, token* ArgsList) ;
        project(token FileName, token ProjectName, token* ArgsList).


predicates
    parse: (string, token*) -> project.

end class parse_project
