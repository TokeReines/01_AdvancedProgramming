-module(async).

-export([new/2, wait/1, poll/1]).

% Aid stands for an actions ID, which is an opaque datatype (that is you decide what it should it should be)
% Actionid -> 0.

% request_reply(Pid, Request) ->
%     Pid ! {self(), Request},
%     receive
%         {Pid, Response} -> Response
%     end.

% Færdig/result, 

% starts a concurrent computatation that computes 'Fun(Arg)'. It returns an action ID (Aid)
% A1 = async:new(fun ({X, Y}) -> X+Y end, {1,2}).
% A2 = async:new(fun ({X}) -> timer:sleep(15000) end, 15000).
-spec new(fun(), any()) -> pid().
new(Fun, Arg) -> 
      spawn(fun() -> 
        Res = Fun(Arg),
        receive
          From -> From ! Res
        end
      end).
    

% Waits for an asyncronous action to complete, and return the values of the computation. If the asynchronous
% action threw an exception, then the exceptions is rethrown by wait.
wait(Aid) -> 
  Aid ! self(),
  receive
    Res -> Res
  end.


% that check wether an asynchronoys action has completed yet. If it has not completed yet
% then the result is 'nothing'; otherwise the result is '{exceptions, Ex}' if the 
% the asynchronous action raised the exceptio 'Ex', or '{ok, Res}' if it returned the value 'Res'.
poll(Aid) -> not_implemented.
  % receive
  %            {'nothing'} -> {'nothing'};
  %        end.

