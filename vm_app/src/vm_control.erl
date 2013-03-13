-module(vm_control).
%-behavior(gen_server).
-exports([start/0, stop/0, choose_product/1, insert_coin/1, 
	  request/0, cancel/0]).

start()->
    ok.

stop()->
    ok.

choose_product(Product)->
    ok.

insert_coin(Coin)->
    ok.

request()->
    ok.

cancel()->
    ok.
