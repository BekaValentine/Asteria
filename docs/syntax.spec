syntax Program P ::=  program(D*)  (Program) ;;

syntax Declaration D
  ::=  dataDeclaration(IdentName; VarName*; DataClause*)
    |  typeSynonymDeclaration(IdentName; Type)
    |  typeClassDeclaration(IdentName; VarName*; ClassConstraints*; MethodDeclaration*)
    |  type
