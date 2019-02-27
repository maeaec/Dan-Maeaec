-module(server).
-export([start/1,stop/1,loop/2]).

%cd("C:/Users/danie/Documents/GitHub/Dan-Maeaec/Lab 2").

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    Pid = spawn(fun() -> loop([], []) end),
    catch(unregister(ServerAtom)),
    register(ServerAtom, Pid),
    Pid.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    ServerAtom ! stop,
    catch(unregister(ServerAtom)),
    ok.

loop(Channels, Nicknames) ->
  receive
    {join, Channel, From, Ref} ->
        %io:format('Join received by server~n'),

        Cond = lists:member(Channel, Channels),
        if
            Cond ->
                list_to_atom(Channel) ! {join, From, Ref},
                loop(Channels, Nicknames);
            true ->
                ok
        end,

        NewChannelAtom = list_to_atom(Channel),
        channel:start(Channel, NewChannelAtom),
        list_to_atom(Channel) ! {join, From, Ref},
        %io:format('Create channel'),
        loop([Channel|Channels], Nicknames);
        
    {leave, Channel, From, Ref} ->
        %io:format('Leave received by server~n'),
        %io:format('Channels are: ~w~n', [Channels]),
        Cond = not lists:member(Channel, Channels),
        if
            Cond ->
                From ! {error, user_not_joined, "Trying to leave a nonexistent channel", Ref};
            true ->
                list_to_atom(Channel) ! {leave, From, Ref}
        end,

        loop(Channels, Nicknames);

    {message_send, Channel, Msg, From, Nick, Ref} ->
        io:format('Message_send received by server~w~n', [self()]),
        %io:format('Channels are: ~w~n', [Channels]),
        Cond = not lists:member(Channel, Channels),
        if
            Cond ->
                From ! {error, user_not_joined, "Trying to send a message to a nonexistent channel", Ref};
            true ->
                list_to_atom(Channel) ! {message_send, Msg, From, Nick, Ref}
        end,

        loop(Channels, Nicknames);

    {nick_change, OldNick, NewNick, From, Ref} ->
        Cond = lists:member(NewNick, Nicknames),
        if
            Cond ->
                From ! {error, nick_taken, "The nickname is already taken", Ref},
                loop(Channels, Nicknames);
            true ->
                From ! {ok_nick_change, Ref},
                Nicknames_updated = lists:delete(OldNick, [NewNick|Nicknames]),
                loop(Channels, Nicknames_updated)
        end;

    {nick_init, Nick, From, Ref} ->

        From ! {ok_nick_init, Ref},

        Nicknames_updated = [Nick|Nicknames],
        loop(Channels, Nicknames_updated);

    stop ->
        io:format("Stopped ~w~n", [self()]),
        ok
  end.



% Kanske använd genserver istället?