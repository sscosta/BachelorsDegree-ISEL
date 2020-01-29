

prolog_calculate(Expr, Value) :-
   on_exception(Exc, Value is Expr, handler(Exc,Value)).
   
handler(domain_error(_,_,_,_),'Incorrect expression').   
handler(Exc,Exc).      
   
  
   
