-module(channel).
-export([start/2,stop/1,loop/2]).

% cd("C:/Users/danie/Documents/GitHub/Dan-Maeaec/Lab 2").

% Start a new server process with the given name
% Do not change the signature of this function.
start(Channel, ServerAtom) ->
    Pid = spawn(fun() -> loop(Channel, []) end),
    catch(unregister(ServerAtom)),
    register(ServerAtom, Pid),
    Pid.
    
% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    ServerAtom ! stop,
    catch(unregister(ServerAtom)),
    ok.

broadcast(_Channel, [], _Msg, _From, _Nick) ->
    ok;

broadcast(Channel, [Head|Members], Msg, From, Nick) ->
    %io:format('Message is: ~w~n', [{message_receive, Channel, Nick, Msg}]),
    if
        From =/= Head ->
            Head!{request, self(), make_ref(), {message_receive, Channel, Nick, Msg}};
            %genserver:request(Nick, {message_receive, Channel, Nick, Msg});
        true ->
            ok
    end,
    broadcast(Channel, Members, Msg, From, Nick).

loop(Channel, Members) ->
  receive
    {join, From, Ref} ->
        %io:format('Join received by channel~n'),
        Cond = not lists:member(From, Members),
        if
            Cond ->
                From ! {ok_join, Ref},
                loop(Channel, [From|Members]);
            true ->
                From ! {error, user_already_joined, "Trying to join a channel the client is already in", Ref},% Returnera user_already_joined på nåt sätt???
                loop(Channel, Members)
        end;

    {leave, From, Ref} ->
        %io:format('Leave received by channel~n'),
        %io:format('Members are: ~w~n', [Members]),
        Cond = lists:member(From, Members),
        if
            Cond ->
                From ! {ok_leave, Ref},
                loop(Channel, lists:delete(From, Members));
            true ->
                From ! {error, user_not_joined, "Trying to leave a channel the client is not in", Ref},
                loop(Channel, Members)
        end;

    {message_send, Msg, From, Nick, Ref} ->
        %io:format('Message_send received by channel~n'),
        %io:format('Members are: ~w~n', [Members]),

        Cond = lists:member(From, Members),
        if
            Cond ->
                broadcast(Channel, Members, Msg, From, Nick),
                From ! {ok_message_send, Ref},
                loop(Channel, Members);
            true ->
                From ! {error, user_not_joined, "Trying to send a message to a channel the client is not in", Ref}
        end,

        loop(Channel, Members);

    stop ->
        io:format("Stopped ~w~n", [self()]),
        ok
  end.

% Kanske använd genserver istället?