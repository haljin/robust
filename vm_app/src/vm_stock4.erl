-module(vm_stock4).
%-behavior(gen_event).
-export([init/1, insert_prod/2, get_prod/2, check_prod/2]).

init(List) ->
	List = [],
	ok.
	
insert_prod(Prod,List)->
	{Name, Price, Am} = Prod,
	RetVal = case check_prod(Name,List) of
	true -> % change the existing value
		{PriceL, AmL} = hd(get_prod(Name,List)),
		ListT = lists:delete({Name, PriceL, AmL},List),
		NewList = ListT ++ [{Name, Price, Am + AmL}];
		%io:format("true"),
		%NewList = List ++ [Prod];
	false -> % append at the end
		%io:format("false"),
		NewList = List ++ [Prod]
	end,
	NewList.
	
get_prod(Prod,List)->
    RetProd = [{X,Y} || {Name, X, Y} <- List, Name =:= Prod],
	RetProd.
	
check_prod(Prod,List)->
    RetProd = [{X,Y} || {Name, X, Y} <- List, Name =:= Prod],
	retVal(RetProd).
	
retVal([]) -> false;
retVal(Val) -> true.
	
	