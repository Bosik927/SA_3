-module(table).
-compile(export_all).

-record(table, {name,
				table,
				clients, 
				waiters}).


start(MaxSushi, TableName) ->
    register(?MODULE, Pid=spawn(?MODULE, init, [MaxSushi, TableName])),
    Pid.
terminate() ->
    ?MODULE ! shutdown.

% WAITERS
get_waiters() ->
	?MODULE ! {self(), get_waiters},
	receive
        {Waiters} -> Waiters
    after 5000 ->
        {error}
    end.
	
add_waiter(Name) ->
	?MODULE ! {self(), create_waiter, Name},
	receive
        done -> done
    after 5000 ->
        {error}
    end.
	
delete_waiter(Name) ->
	?MODULE ! {self(), delete_waiter, Name},
	receive
        done -> done
    after 5000 ->
        {error}
    end.

% CLIENTS
add_client(Name) ->
	?MODULE ! {self(), create_client, Name},
	receive
        done -> done
    after 5000 ->
        {error}
    end.
	
delete_client(Name) ->
	?MODULE ! {self(), delete_client, Name},
	receive
        done -> done
    after 5000 ->
        {error}
    end.

get_clients() ->
	?MODULE ! {self(), get_clients},
	receive
        {Clients} -> Clients
    after 5000 ->
        {error}
    end.

% TABLE
get_table() ->
	?MODULE ! {self(), get_table},
	receive
        {Table} -> Table
    after 5000 ->
        {error}
    end.
	

% PRIVATE METHODS
createSushiTable(MasterPid, MaxSushi,TableName) ->
	spawn(bufferMemory, init, [MasterPid, MaxSushi,TableName]).

createSushiMen(MasterPid, Table, Name) ->
	spawn(producer, init, [MasterPid, Table, Name]).

createJapanese(MasterPid, Table, Name) ->
	spawn(consumer, init, [MasterPid, Table, Name]).

init(MaxSushi, TableName) ->
    loop(#table{name = TableName,
				table = createSushiTable(self(), MaxSushi,TableName),
				clients=orddict:new(),
                waiters=orddict:new()}).	

loop(T=#table{}) ->
	receive
		% MESSAGE
		{From, message, Msg} ->
			showMessage(From, [Msg]),
			loop(T);
		
		% WAITER
		{Pid, create_waiter, Name} ->
			Waiter = createSushiMen(self(),T#table.table,Name),
			Waiters = orddict:store(Name,Waiter, T#table.waiters),
			Pid ! done,
			loop(T#table{waiters=Waiters});
		{Pid, delete_waiter, Name} ->
			Waiters = case orddict:find(Name, T#table.waiters) of
                         {ok, W} ->
							W ! shutdown,
                            orddict:erase(Name, T#table.waiters);
                         error ->
                             T#table.waiters
                     end,
            Pid ! done,
			loop(T#table{waiters=Waiters});
		{Pid, get_waiters} ->
			Pid ! {T#table.waiters},
			loop(T);
		
		% CLIENT
		{Pid, create_client, Name} ->
			Client = createJapanese(self(),T#table.table,Name),
			Clients = orddict:store(Name,Client, T#table.clients),
			Pid ! done,
			loop(T#table{clients=Clients});
		{Pid, delete_client, Name} ->
			Clients = case orddict:find(Name, T#table.clients) of
                         {ok, C} ->
							C ! shutdown,
                            orddict:erase(Name, T#table.clients);
                         error ->
                             T#table.clients
                     end,
            Pid ! done,
			loop(T#table{clients=Clients});
		{Pid, get_clients} ->
			Pid ! {T#table.clients},
			loop(T);

		% TABLE
		{Pid, get_table} ->
			Pid ! {T#table.table},
			loop(T);

		% REST
		shutdown ->
            exit(shutdown);
		stop ->
			true
	end.

showMessage(From, Msg) ->
	io:format("~p says: ~p~n", [From, Msg]).
showInfo(Msg) ->
	io:format("Info: ~p~n", [Msg]).