-module(item_transformer).

-behaviour(gen_statem).

-export([callback_mode/0, init/1]). 
-export([start_item/2, stop_item/1, upsert/3, internal_read/1, insert/4, set/4, update/4, read_item/2, state/1, stable/3]).
-export([idle/3, working/3]).

%%% -------------------------------------------------------
%%% Item Transformer API - Only known to Frappe
%%% -------------------------------------------------------

start_item(FS, Key) ->
    gen_statem:start_link(?MODULE, {FS, Key}, []).

%%% -------------------- Sync Calls -------------------------
stop_item(ITrans) ->
    gen_statem:stop(ITrans).

internal_read(ITrans) ->
    gen_statem:call(ITrans, internal_read).

state(ITrans) ->
    gen_statem:call(ITrans, state).

%%% -------------------- ASync Calls -------------------------
insert(ITrans, Value, Cost, From) ->
    gen_statem:cast(ITrans, {insert, Value, Cost, From}).

set(ITrans, Value, Cost, From) ->
    gen_statem:cast(ITrans, {set, Value, Cost, From}).

update(ITrans, Value, Cost, From) ->
    gen_statem:cast(ITrans, {update, Value, Cost, From}).

read_item(ITrans, From) ->
    gen_statem:cast(ITrans, {read, From}).

stable(ITrans, From, Ref) ->
    gen_statem:cast(ITrans, {stable, From, Ref}).

upsert(ITrans, Fun, From) ->
    gen_statem:cast(ITrans, {upsert, Fun, From}).




%%% -------------------------------------------------------
%%% Mandatory callback functions
%%% -------------------------------------------------------

init({FS, Key}) ->
    % A cost of 0 is invalid for an Item. 
    % Using it here is a guarantee for freshness (TM) when checking if this item has a value or not
    Data = {FS, Key, default, 0}, 
    {ok, idle, Data}.

callback_mode() -> state_functions.

%%% -------------------------------------------------------
%%% State callbacks
%%% -------------------------------------------------------

%%% -------------------- Idle -------------------------
idle(cast, {upsert, Fun, From}, Data) ->
    {_FS, _Key, Value, Cost} = Data,
    case Cost of
        0 -> 
            work(Fun, new, From);
        _ ->            
            work(Fun, {existing, Value}, From)
    end,
    {next_state, working, Data};

idle(cast, {insert, Value, Cost, From}, Data) ->
    {FS, Key, _OldValue, OldCost} = Data,
    case OldCost of
        0 -> 
            Res = gen_server:call(FS, {write, Key, Cost, Value, write}),
            case Res of
                ok -> 
                    NewData = {FS, Key, Value, Cost}, 
                    {keep_state, NewData, [{reply, From, ok}]};
                {error, Reason} ->
                    {keep_state, Data, [{reply, From, {error, Reason}}]}
            end;
        _ -> 
            {keep_state_and_data, [{reply, From, {error, "Key already set"}}]}
    end;

idle(cast, {set, Value, Cost, From}, Data) ->
    {FS, Key, _V, _C} = Data,
    Res = gen_server:call(FS, {write, Key, Cost, Value, write}),
    case Res of
        ok -> 
            NewData = {FS, Key, Value, Cost}, 
            {keep_state, NewData, [{reply, From, ok}]};
        {error, Reason} ->
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;

idle(cast, {update, Value, Cost, From}, Data) ->
    {FS, Key, _, OldCost} = Data,
    case OldCost of 
        0 -> {keep_state, Data, [{reply, From, {error, "Item not found"}}]};
        _ ->
            Res = gen_server:call(FS, {write, Key, Cost, Value, write}),
            case Res of
                ok -> 
                    NewData = {FS, Key, Value, Cost}, 
                    {keep_state, NewData, [{reply, From, ok}]};
                {error, Reason} ->
                    {keep_state, Data, [{reply, From, {error, Reason}}]}
            end
    end;

idle(cast, {read, From}, Data) ->
    {FS, Key, Value, Cost} = Data,
    case Cost of
        0 -> {keep_state_and_data, [{reply, From, nothing}]};
        _ ->
            gen_server:call(FS, {write, Key, Cost, Value, read}), 
            {keep_state_and_data, [{reply, From, {ok, Value}}]}
    end;

idle(cast, {stable, From, Ref}, Data) ->
    {_, _, Value, Cost} = Data,
    case Cost of
        0 -> 
            io:format("No data"),
            {keep_state_and_data, []};
        _ ->
            % Hack to give the process a msg immediately
            {A, _} = From,
            A ! {Ref, Value},
            % Should have been like this: {reply, From, {Ref, Value}}, but doesn't work
            {keep_state_and_data, []}
    end;

% This call shouldn't happen, but it does. Give me 1 more day and I'll fix it :)
idle(cast, {done, _Value, _From}, _) ->
    {keep_state_and_data, []};

idle({call, From}, internal_read, Data) ->
    {_, _, Value, Cost} = Data,
    {keep_state_and_data, [{reply, From, {Value, Cost}}]}.

%%% -------------------- Working -------------------------
working({call, From}, read, Data) -> 
    {FS, Key, Value, Cost} = Data,
    gen_server:call(FS, {write, Key, Cost, Value, read}),
    {keep_state_and_data, [{reply, From, {Value, Cost}}]};

working(cast, {set, Value, Cost, From}, Data) ->
    {FS, Key, _V, _C} = Data,
    Res = gen_server:call(FS, {write, Key, Cost, Value, write}),
    case Res of
        ok -> 
            {next_state, idle, {FS, Key, Value, Cost}, [{reply, From, ok}]};
        {error, _} ->
            {next_state, idle, Data, [{reply, From, ok}]}
    end;

working(cast, {done, NewValue, From}, Data) -> 
    {FS, Key, _, _} = Data,
    case NewValue of
        {new_value, Value, Cost} -> 
            Res = gen_server:call(FS, {write, Key, Cost, Value, write}),
            case Res of 
                ok -> 
                    {next_state, idle, {FS, Key, Value, Cost}, [{reply, From, Res}]};
                _ -> 
                    {next_state, idle, Data, [{reply, From, Res}]}
            end;
        _ -> 
            {next_state, idle, Data, [{reply, From, ok}]}
    end;

working(cast, {done, _}, Data) -> 
    {next_state, idle, Data};

working(cast, {failed, Error, From}, Data) ->
    {FS, Key, _, _} = Data,
    case Error of
        {new_value, Value, Cost} -> 
            Res = gen_server:call(FS, {write, Key, Cost, Value, write}),
            case Res of 
                ok -> 
                    {next_state, idle, {FS, Key, Value, Cost}, [{reply, From, Res}]};
                {error, Reason} -> 
                    {next_state, idle, Data, [{reply, From, {error, Reason}}]}
            end;
        _ -> 
            {next_state, idle, Data, [{reply, From, {error, Error}}]}
    end;

working(cast, {failed, _}, Data) -> 
    {next_state, idle, Data};

working({call, From}, state, _) ->
    {keep_state_and_data, [{reply, From, working}]};

% This will postpone any upsert, insert, or update calls while working, 
% ensuring that they are processed sequentially
working(cast, _, _) ->
    {keep_state_and_data, [{postpone, true}]};

working({call, _}, _, _) ->
    {keep_state_and_data, [{postpone, true}]}.

%%% -------------------------------------------------------
%%% Auxiliary Functions
%%% -------------------------------------------------------

work(Fun, Arg, From) ->
    % Spawn with link to make sure, this worker is killed when Item Transformer is stopped
    Me = self(),
    spawn_link(fun() -> 
        try   
            Value = Fun(Arg),
            gen_statem:cast(Me, {done, Value, From})
        catch
            throw : Throw -> gen_statem:cast(Me, {failed, Throw, From});
            exit : Exit -> gen_statem:cast(Me, {failed, Exit, From});
            error : Error -> gen_statem:cast(Me, {failed, Error, From})
        end
    end).