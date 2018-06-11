% --------------- Class parse_project: Convert Structure to Project ---------------

class parse_project
    open core, lexer, process

domains
    project = dummy ;
        % (project FolderName {Parameters} {Target} {MainBody} {Groups} )
        proj(atom FolderName, project_pairs* Parameters, project_namespace* Target, project_statement* MainBody, project_group* Groups).

    project_pairs = pair(atom Name, atom Value)     % (Name:identifier1 Value:str)

    project_namespace = atom. % identifier1

    param = atom. % identifier1

    project_statement = 

        % ( feature FileName:str {Params} )
        statement_feature(atom FileName, param* Params) ;

        % ( group GroupName:identifier1 {Params} )
        statement_group(token GroupName, param* Params) ;

        % ( import ProjectName:str {Params} )
        statement_project(token GroupName, param* Params) .

    project_group = g(token GroupName, param* Params, statement* Body) % (GroupName:identifier1 {Params} {Body} )

predicates
    main: (atom) -> project.

end class parse_project


