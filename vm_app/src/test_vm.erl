-module(test_vm).
-include("../include/vmdata.hrl").
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prod_generator() ->
    Products = [cola,chips,candy,faxe_kondi,liquorice,some_random_crap,jelly_beans],
    ?SUCHTHAT(List, 
	      non_empty(list(#product{name = oneof(Products), price = choose(2, 40), ammount = choose(1,10)})),
	      lists:all(fun(E) ->
				E=<1
			end, 
			lists:map(fun(El) ->
					  lists:foldl(fun(El2, Sum) ->
							      case El2#product.name of
								  El ->
								      Sum +1;
								  _ ->
								      Sum
							      end
						      end, 0, List)
				  end, Products))).
prod_nostock_generator() ->
    Products = [cola,chips,candy,faxe_kondi,liquorice,some_random_crap,jelly_beans],
    ?SUCHTHAT(List, 
	      non_empty(list(#product{name = oneof(Products), price = choose(2, 10), ammount = 0})),
	      lists:all(fun(E) ->
				E=<1
			end, 
			lists:map(fun(El) ->
					  lists:foldl(fun(El2, Sum) ->
							      case El2#product.name of
								  El ->
								      Sum +1;
								  _ ->
								      Sum
							      end
						      end, 0, List)
				  end, Products))).

coin_generator()->
    [#coin{type = ore50, value = 0.5, ammount = choose(0,30)},
     #coin{type = kr1, value = 1, ammount = choose(0,30)},
     #coin{type = kr2, value = 2, ammount = choose(0,30)},
     #coin{type = kr5, value = 5, ammount = choose(0,30)},
     #coin{type = kr10, value = 10, ammount = choose(0,30)},
     #coin{type = kr20, value = 20, ammount = choose(0,30)}].

choice_generator(Stock) ->
    non_empty(list(
		?LET(Product, oneof(Stock), {call, vm_control, choose_product, [Product#product.name]})
		    )).

operation_generator(Stock)->
    ?LET(Choices, choice_generator(Stock), {Choices, insertion_generator(Choices, Stock), Stock}).

operation_cancel_generator(Stock)->
    ?LET(Choices, choice_generator(Stock), {Choices, insertion_cancel_generator(Choices, Stock), Stock}).

operation_change_generator(Stock)->
    ?LET(Choices, choice_generator(Stock), {Choices, insertion_change_generator(Choices, Stock), Stock}).

insertion_generator(Choices, Stock) ->
    {call, vm_control, choose_product, [ProdName]} = lists:last(Choices),
    Product = lists:keyfind(ProdName, #product.name, Stock), 
    vector(Product#product.price, {call, vm_control, insert_coin, [kr1]}).

insertion_cancel_generator(Choices, Stock) ->
    {call, vm_control, choose_product, [ProdName]} = lists:last(Choices),
    Product = lists:keyfind(ProdName, #product.name, Stock),  
    ?LET(Calls, ?LET(Length, choose(1, Product#product.price-1), 
		     vector(Length, {call, vm_control, insert_coin, [kr1]})),
	 Calls ++ [{call, vm_control, cancel, []}]).

insertion_change_generator(Choices, Stock)->
    {call, vm_control, choose_product, [ProdName]} = lists:last(Choices),
    Product = lists:keyfind(ProdName, #product.name, Stock), 
    {insertion_sub_gen(Product#product.price, []), Product}.

insertion_sub_gen(Price, Insertions)->
    ?LET(Coin, oneof([ore50, kr1, kr2, kr5, kr10, kr20]), 
	 case vm_coin:coin_to_val(Coin) >= Price of
	     true ->
		 Insertions ++ [{call, vm_control, insert_coin, [Coin]}];
	     false  ->
		 insertion_sub_gen(Price - vm_coin:coin_to_val(Coin), 
				   Insertions ++ [{call, vm_control, insert_coin, [Coin]}])
	 end).				   
	     

transaction_generator()->
    ?LET(Stock, 
	 prod_generator(), 
	 operation_generator(Stock)).

transaction_cancel_generator()->
    ?LET(Stock, prod_generator(),
	 operation_cancel_generator(Stock)).

transaction_change_generator()->
    ?LET(Stock, prod_generator(),
	 operation_change_generator(Stock)).

transaction_nostock_generator()->
    ?LET(Stock, prod_nostock_generator(),
	 operation_generator(Stock)).

purchase_property()->
    ?FORALL({{Choices, Insertion, Stock}, Coins}, 
	    {transaction_generator(), coin_generator()},
	    begin
		vm_coin:generate_dets(Coins),
		vm_stock:generate_dets(Stock),
		timer:sleep(100),
		vm_sup:start(),
		{call, vm_control, choose_product, [BoughtItem]} = lists:last(Choices),
		lists:foreach(fun(El) -> 
				      eval(El),
				      timer:sleep(10) 
			      end, Choices ++ Insertion),
		timer:sleep(200),		
		Result = lists:keymember(BoughtItem, 1, vm_case:get_all()),
		vm_sup:stop(),
		timer:sleep(400),
		file:delete("stock.db"),
		file:delete("coin.db"),	
		Result
	    end).

cancel_property() ->
     ?FORALL({{Choices, Insertion, Stock}, Coins}, 
	    {transaction_cancel_generator(), coin_generator()},
	    begin
		vm_coin:generate_dets(Coins),
		vm_stock:generate_dets(Stock),
		timer:sleep(100),
		vm_sup:start(),
		{call, vm_control, choose_product, [BoughtItem]} = lists:last(Choices),
		InsertedSum =  calc_inserted_sum(Insertion),
		lists:foreach(fun(El) -> 
				      eval(El),
				      timer:sleep(50) 
			      end, Choices ++ Insertion),
		timer:sleep(200),
		CaseSum = calc_case_sum(),
		Result = (not lists:keymember(BoughtItem, 1, vm_case:get_all())) and
										   (CaseSum == InsertedSum),
		vm_sup:stop(),
		timer:sleep(400),
		file:delete("stock.db"),
		file:delete("coin.db"),	
		Result
	    end).

change_property()->
    ?FORALL({{Choices, {Insertion, Product}, Stock}, Coins}, 
	    {transaction_change_generator(), coin_generator()},
	    begin
		vm_coin:generate_dets(Coins),
		vm_stock:generate_dets(Stock),
		
		Price = Product#product.price,	
		InsertedSum =  calc_inserted_sum(Insertion),
		MachineSum =  calc_machine_sum(Coins, InsertedSum - Price, Insertion),
		io:format("Machine ~p~n Change ~p~n", [MachineSum, InsertedSum - Price]),
		io:format("Inserted ~p~n Price ~p~n", [InsertedSum, Price]),
		timer:sleep(100),
		vm_sup:start(),
		lists:foreach(fun(El) -> 
				      eval(El),
				      timer:sleep(10) 
			      end, Choices ++ Insertion),
		timer:sleep(200),
		CaseSum = calc_case_sum(),
		Result = case MachineSum < (InsertedSum - Price) of
			     true ->
				 CaseSum == MachineSum;
			     false ->
				 CaseSum == InsertedSum - Price
			 end,
		
		vm_sup:stop(),
		timer:sleep(400),
		file:delete("stock.db"),
		file:delete("coin.db"),	
		Result
	    end).

nostock_property()->
    ?FORALL({{Choices, Insertion, Stock}, Coins}, 
	    {transaction_nostock_generator(), coin_generator()},
	    fails(
	      begin
		  vm_coin:generate_dets(Coins),
		  vm_stock:generate_dets(Stock),
		  timer:sleep(100),
		  vm_sup:start(),
		  {call, vm_control, choose_product, [BoughtItem]} = lists:last(Choices),
		  lists:foreach(fun(El) -> 
					eval(El),
					timer:sleep(10) 
				end, Choices ++ Insertion),
		  timer:sleep(200),		
		  Result = lists:keymember(BoughtItem, 1, vm_case:get_all()),
		  vm_sup:stop(),
		timer:sleep(400),
		  file:delete("stock.db"),
		  file:delete("coin.db"),	
		  Result
	      end)).


calc_case_sum()->
    lists:foldl(fun({C, A}, Acc) ->
			Acc + (vm_coin:coin_to_val(C) * A)
		end, 0, vm_coincase:get_all()).
    

calc_inserted_sum(Insertion) ->
    lists:foldl(fun({call, vm_control, Op, Data}, Acc) ->
			case Op of
			    insert_coin ->
				[Coin] = Data,
				Acc + vm_coin:coin_to_val(Coin);
			    cancel ->
				Acc
			end
		end, 0, Insertion).
    
calc_machine_sum(Coins, Change, Insertions) ->
    NewCoins = lists:foldl(fun({call, vm_control, insert_coin, [Coin]}, Acc) ->
				   
				   C = lists:keyfind(Coin, #coin.type, Coins),
				   
				   lists:keyreplace(Coin, #coin.type, Acc, 
						C#coin{ ammount = C#coin.ammount +1})
		end, Coins, Insertions),
    
    lists:foldl(fun(El, Acc) ->
			case (Change-Acc) >= El#coin.value of
			    true -> 
				Ammount = min(El#coin.ammount, Change / El#coin.value),
				Acc + (El#coin.value * Ammount);
			    false ->
				Acc
			end
		end, 0, lists:reverse(NewCoins)).
