-module(bufferMemory).
-compile(export_all).

init(Master, Max,RestaurantName) -> 
	Msg = "Table is open!",
	Master ! {RestaurantName, message, Msg},
	Table = [],
	loop(Master, Table, Max).

loop(Master, Table, Max) ->

	receive
		{Producer, Sushi, Name, sushiReady} ->
			QtdSushi = length(Table),
			if
				Max > QtdSushi ->
					addSushi(Master, Table, Max, Sushi, Producer, Name);
				QtdSushi >= Max ->
					%io:format("List is full: ~w~n", [length(Table)]),
					Producer ! full,
					loop(Master, Table, Max)

			end;

		{Client, starving, Name} ->
			QtdSushi = length(Table),
			if
				QtdSushi > 0 ->
					removeSushi(Master, Table, Max, Client, Name);

				0 >= QtdSushi ->
					io:format("List is empty: ~w~n", [length(Table)]),
					Client ! noSushi,
					loop(Master, Table, Max)
			end;
			

		stop ->
			true
	end.

addSushi(Master, Table, Max, Sushi, Producer, Name) ->
	QtdSushi = length(Table),
	NewTable = lists:append(Table, [Sushi]),
	NewQtdSushi = QtdSushi+1,
	Msg = lists:flatten(io_lib:format("A Sushi was added to the table! There are ~w now.",[NewQtdSushi])),
	Master ! {Name, message, Msg},
	checkTable(Master, NewTable, Max, QtdSushi+1, Producer).

removeSushi(Master, Table, Max, Client, Name) ->
	QtdSushi = length(Table),
	{Sushi, All} = getSushi(Table),
	Client ! {Sushi, ready},
	NewQtdSushi = QtdSushi-1,
	Msg = lists:flatten(io_lib:format("A Sushi was removed from the table. There are ~w now.", [NewQtdSushi])),
	Master ! {Name, message, Msg},
	loop(Master, All, Max).

getSushi([Head | Tail]) ->
	{Head, Tail}.

checkTable(Master, Table, Max, QtdSushi, Producer) ->
	if
		Max > QtdSushi ->
			Producer ! done;
			
		QtdSushi >= Max ->
			Producer ! full
	end,

	loop(Master, Table, Max).



