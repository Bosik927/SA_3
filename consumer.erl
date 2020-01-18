-module(consumer).
-compile(export_all).

start(Master, Table, Name) ->
    register(?MODULE, Pid=spawn(?MODULE, init, [Master, Table, Name])),
    Pid.

init(Master, Table, Name) ->
	Msg = "I joined to table!",
	Master ! {Name, message, Msg},

	loop(Master, Table, 2000, Name).

loop(Master, Table, Hush, Name) ->
	timer:sleep(Hush),
	getSushi(Master, Table, Name),
	receive
		shutdown ->
            exit(shutdown);
		stop ->
			true
	end.
			
getSushi(Master, Table, Name) ->
	Pid = self(),
	Table ! {Pid, starving, Name},

	receive
		{Sushi, ready} ->
			eatSushi(Master, Sushi, Name),
			loop(Master, Table, 2000, Name);
		noSushi ->
			Msg = "I am starting to hate this place. There is no sushi!!",
			Master ! {Pid, message, Msg},
			loop(Master, Table, 5000, Name);
		dead ->
			true
	end.

eatSushi(Master, Sushi, Name) ->
	Pid = self(),
	Sushi ! {Pid, getID},
	receive
		{SushiId, sushi} ->
			Msg = lists:flatten(io_lib:format("I ate sushi ~p and it was great! Thank you.", [SushiId])),
			Sushi ! eaten,
			Master ! {Name, message, Msg}
	end.