-module(server).
-export([start/1,stop/1,loop/0]). % Brukade vara 0

% cd("C:/Users/danie/Documents/GitHub/Dan-Maeaec/Lab 2").


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    Pid = spawn(fun() -> loop() end),
    catch(unregister(ServerAtom)),
    register(ServerAtom, Pid),
    Pid.
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    %not_implemented.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    ServerAtom ! stop,
    catch(unregister(ServerAtom)),
    ok.

loop() ->
  io:format('Channels are: '),
  receive
    {join, Channel, From, Ref} ->
        io:format('joined'),
        From ! {ok_joined, self(), Ref},
        loop();
    {msg, From, Ref, Text} -> 
        io:format("Message from  ~w: ~w~n", [From, Text]),
        From ! {reply, Ref, 'Message arrived!'},
        loop();
    stop ->
        io:format("Stopped ~w~n", [self()]),
        ok
  end.



% Kanske använd genserver istället?