
-module(curry).
-compile(export_all).
-import(lists,[reverse/1]).

'__curry'() -> fun curry/1.
curry(F) when is_function(F) -> 
	 {arity,N} = erlang:fun_info(F,arity),
	 curry(F,N,[]);
curry(X) -> X.
curry(F,0,Args)->curry(apply(F,lists:reverse(Args)));
curry(F,N,Args)->fun(X)->curry(F,N-1,[X|Args]) end.

%%A primcall convenience function. To be added to every module
%%in codegen step.
%%apply :: String -> String -> Integer -> a -> b
apply(ModString,FunString,Arity,Tuple)->
		apply(erlang:make_fun(list_to_atom(ModString),
				      list_to_atom(FunString),
				      Arity),
		     tuple_to_list(Tuple)).