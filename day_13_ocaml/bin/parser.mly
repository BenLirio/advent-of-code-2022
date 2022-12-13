%{
  open Ast
%}
%token <int> INT
%token COMMA
%token LBRAC RBRAC
%token EOF

%start problem
%type <Ast.expr> expr
%type <Ast.expr_pair> expr_pair
%type <Ast.problem> problem

%%
expr:
  | LBRAC cs=separated_list(COMMA, expr) RBRAC { List(cs) }
  | v=INT { Int(v) }
expr_pair:
  | e1=expr e2=expr { (e1, e2) }
problem:
  | es=list(expr_pair) EOF { es }