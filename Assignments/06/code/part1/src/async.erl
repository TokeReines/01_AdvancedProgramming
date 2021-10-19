-module(async).

-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).

% New: Supervisor()  -> Worker
% Wait: Supervisor() -> await NewSupervisor
% Poll: poll NewSupervisor

% Starts a supvisor with a worker that invokes the function
new(Fun, Arg) -> nope. 
wait(Aid) -> nope. % Spawns a supervisor that waits for ^ supervisor to complete
poll(Aid) -> nope. % Polls the supervisor from new()
wait_catch(Aid) -> nope.
wait_any(Aids) -> nope.
