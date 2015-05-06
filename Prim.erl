
-module('Prim').
-compile(export_all).
-import(lists,[reverse/1]).


curry(F) when is_function(F) -> 
	 {arity,N} = erlang:fun_info(F,arity),
	 curry(F,N,[]);
curry(X) -> X.
curry(F,0,Args)->curry(apply(F,lists:reverse(Args)));
curry(F,N,Args)->fun(X)->curry(F,N-1,[X|Args]) end.

%%A primcall convenience function. To be added to every module
%%in codegen step.
%%apply :: String -> String -> Integer -> a -> b
apply(ModString,FunString,Arity)-> curry(
		erlang:make_fun(list_to_atom(ModString),
				list_to_atom(FunString),
				Arity)).
'__apply'()->curry(fun apply/3).

arity0toArity1(Mod,Fun)-> 
		fun(_) ->
		'Prim':apply(Mod,Fun,0) end.
coerce(X)->X.

%%receive primitive for use by hopper.
%%to be removed once receive works
%%rcv_key :: key -> Timeout -> IO (Maybe a)
rcv_key(Key,Timeout)->
	receive {t2,Key,X} -> {just,X}
		after Timeout -> nothing 
	end.