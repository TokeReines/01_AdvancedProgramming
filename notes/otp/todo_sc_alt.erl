-module(todo_sc_alt).
-export([start/0, add_item/3, all_items/1, finish/2]).

%% API
start()                 -> spawn(fun () -> loop(init()) end).
add_item(TL, Desc, Due) -> nonblock(TL, {add, {Desc, Due}}).
all_items(TL)           -> request_reply(TL, all_items).
finish(TL, Index)       -> request_reply(TL, {finish, Index}).

%% Data manipulation
init() -> [].

handle_call(all_items, Items) ->
  {reply, {ok, Items}, Items};
handle_call({finish, Index}, Items) ->
  try {Before, [_Idx | After]} = lists:split(Index-1, Items),
       {reply, ok, Before ++ After}
  catch
    error : _ -> {reply, {error, index_out_of_bounds}, Items}
  end.

handle_cast({add, {Description, Due}}, Items) ->
  Item = #{ description => Description,
            due => Due},
  [Item | Items].

%% Communication
nonblock(Pid, Request) -> Pid ! {cast, self(), Request}.

request_reply(Pid, Request) ->
  Pid ! {call, self(), Request},
  receive
    {Pid, Response} -> Response
  end.

loop(Data) ->
  receive
    {call, From, Request} ->
      case handle_call(Request, Data) of
        {reply, Res, NewData} ->
          From ! {self(), Res},
          loop(NewData);
        {noreply, NewData} ->
          loop(NewData)
      end;
    {cast, _From, Request} ->
      NewData = handle_cast(Request, Data),
      loop(NewData)
  end.
