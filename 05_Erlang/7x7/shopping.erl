-module(shopping).
-export([calculate_total_prices/1]).

% calculate_total_prices/1: Calculates the total price for each item in the shopping list.
calculate_total_prices(ShoppingList) ->
    % Uses a list comprehension to iterate over each element in the ShoppingList.
    % For each tuple {Item, Quantity, Price}, it multiplies Quantity by Price to get the total price.
    % It then creates a new list of tuples with the Item and the calculated total price.
    [{Item, Quantity * Price} || {Item, Quantity, Price} <- ShoppingList].
