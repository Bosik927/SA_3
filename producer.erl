-module(producer).
-compile(export_all).

start(Master, Table, Name) ->
    register(?MODULE, Pid=spawn(?MODULE, init, [Master, Table, Name])),
    Pid.

init(Master, Table, Name) ->
	Msg = "I came to work!",
	Master ! {Name, message, Msg},

	loop(Master, Table, 1, 1000, Name).

loop(Master, Table, SushiId, Hush, Name) ->
	timer:sleep(Hush),
	makeSushi(Table, SushiId, Name),
	receive
		full ->
			Msg = "There's no space on the table, so I am going to take a rest now!",
			Master ! {Name, message, Msg},
			loop(Master, Table, SushiId+1, 10000,Name);
		shutdown ->
            exit(shutdown);
		done ->
			loop(Master, Table, SushiId+1, 1000, Name)
	end.

makeSushi(Table, SushiId, Name) ->
	Self = self(),
	Sushi = spawn(product, init, [Self, SushiId]),
	Table ! {Self, Sushi, Name,  sushiReady}.