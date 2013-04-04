-module(vm_stock).
%-behavior(gen_server).
-export([start/1, stop/0, check_product/1, get_product/2, insert_products/1]).

start({list, ProductList})->
    ok;
start({dets, DetsTable}) ->
    ok.

stop()->
    ok.

%% Returns: ok - the product is in stock
%%          {warning, noproduct} - the product is out of stock
check_product(Product, Price) ->
    
%% Input: Product - the product requested
%%        Coins - the money customer put into the machine
get_product(Product, Coins)->
    ok.

insert_products(ProductList)->
    ok.
