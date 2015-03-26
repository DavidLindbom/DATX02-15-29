
-module(curry).
-compile(export_all).
-import(lists,[reverse/1]).

curry() -> fun curry/1.
curry(F) when is_function(F) -> 
	 {arity,N} = erlang:fun_info(F,arity),
	 curry(F,N,[]);
curry(X) -> X.
curry(F,0,Args)->apply(F,lists:reverse(Args));
curry(F,N,Args)->fun(X)->curry(F,N-1,[X|Args]) end.