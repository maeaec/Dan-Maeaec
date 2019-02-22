-module(server).
-export([start/1,stop/1,loop/1]). % Brukade vara 0

% cd("C:/Users/danie/Documents/GitHub/Dan-Maeaec/Lab 2").


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    Pid = spawn(fun() -> loop([]) end),
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

broadcast([], _Channel, _Nick, _Msg) ->
    ok;

broadcast([Head|Members], Channel, Nick, Msg) ->
    io:format('Message is: ~w~n', [{message_receive, Channel, Nick, Msg}]),
    genserver:request(Nick, {message_receive, Channel, Nick, Msg}),
    broadcast(Members, Channel, Nick, Msg).

% [{Channel_name, [frodo, bilbo, gandalf]}]

loop(Members) ->
  receive
    {join, Channel, From, Ref} ->
        io:format('Join received by server~n'),
        %Members2 = [From|Members],
        %io:format('Members are: ~w~n', Members2),
        From ! {ok_join, self(), Ref},
        loop([From|Members]);
    {leave, Channel, From, Ref} ->
        io:format('Leave received by server~n'),
        io:format('Members are: ~w~n', [Members]),
        From ! {ok_leave, self(), Ref},
        loop(Members);
    {message_send, Channel, Msg, From, Ref} ->
        io:format('Message_send received by server~n'),
        io:format('Members are: ~w~n', [Members]),
        broadcast(Members, Channel, From, Msg),
        From ! {ok_message_send, self(), Ref},
        loop(Members);
    stop ->
        io:format("Stopped ~w~n", [self()]),
        ok
  end.



% Kanske använd genserver istället?