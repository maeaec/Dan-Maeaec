-module(server).
-export([start/1,stop/1,loop/1]).

%cd("C:/Users/danie/Documents/GitHub/Dan-Maeaec/Lab 2").

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    Pid = spawn(fun() -> loop([]) end),
    catch(unregister(ServerAtom)),
    register(ServerAtom, Pid),
    Pid.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    ServerAtom ! stop,
    catch(unregister(ServerAtom)),
    ok.

loop(Channels) ->
  receive
    {join, Channel, From, Ref} ->
        %io:format('Join received by server~n'),

        Cond = lists:member(Channel, Channels),
        if
            Cond ->
                list_to_atom(Channel) ! {join, From, Ref},
                loop(Channels);
            true ->
                ok
        end,

        NewChannelAtom = list_to_atom(Channel),
        channel:start(Channel, NewChannelAtom),
        list_to_atom(Channel) ! {join, From, Ref},
        %io:format('Create channel'),
        loop([Channel|Channels]);
        
    {leave, Channel, From, Ref} ->
        %io:format('Leave received by server~n'),
        %io:format('Channels are: ~w~n', [Channels]),
        Cond = not lists:member(Channel, Channels),
        if
            Cond ->
                io:format('No such channel'),
                loop(Channels);
            true ->
                ok
        end,

        list_to_atom(Channel) ! {leave, From, Ref},
        loop(Channels);

    {message_send, Channel, Msg, From, Nick, Ref} ->
        %io:format('Message_send received by server~n'),
        %io:format('Channels are: ~w~n', [Channels]),
        Cond = not lists:member(Channel, Channels),
        if
            Cond ->
                io:format('No such channel'),
                loop(Channels);
            true ->
                ok
        end,

        list_to_atom(Channel) ! {message_send, Msg, From, Nick, Ref},
        loop(Channels);

    stop ->
        io:format("Stopped ~w~n", [self()]),
        ok
  end.



% Kanske använd genserver istället?