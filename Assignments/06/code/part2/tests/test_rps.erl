-module(test_rps).

-export([test_all/0]).

-import(qc_rps, [prop_rps/0]).

%% Maybe you want to use eunit
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").


test_all() -> eunit:test(testsuite(), [verbose]).


testsuite() -> 
      [ {"Basic behavior",  spawn,
          [ test_start_broker()
          , test_queue_up()
          ]
        },
        {"QuickCheck tests", spawn,
          [ prop_rps_test()

          ]
        }
      ].

%%% ------------------------------------------
%%% Basic behavior
%%% ------------------------------------------

test_start_broker() ->
    {"Start a broker, and nothing else",
     fun() ->
             ?assertMatch({ok, _}, rps:start())
     end}.

test_queue_up() -> 
    {"Queue up against a Rock bot",
     fun() ->
      {ok, A} = rps:start(),
      spawn(fun() -> rock_bot:queue_up_and_play(A) end),
      {Res, Player, C} =rps:queue_up(A, "Julian", 3),
      io:format("~w~n", [C]),
      % ?assertMatch({ok, _, _}, rps:queue_up(A, "Julian", 3)),
      ?assertMatch({ok, _, _}, {Res, Player, C}),
      % coordinator:stop(C),
      rps:stop(A)
    end
    }.

%%% ------------------------------------------
%%% QuickCheck tests
%%% ------------------------------------------

prop_rps_test() ->
    {"prop_sample", {timeout, 10000, ?_assert(eqc:quickcheck(qc_rps:prop_rps()))}}.
