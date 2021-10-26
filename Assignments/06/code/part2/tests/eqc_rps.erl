-module(eqc_rps).
-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).

-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

%% Maybe you want to use eunit
-include_lib("eunit/include/eunit.hrl").
