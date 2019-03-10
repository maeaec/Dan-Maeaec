-module(server).
-export([initial_state/0,start/1,stop/1,handle/2]).

-record(server_st, {
    nicknames, % List of taken nicknames
    channels
}).

initial_state() ->
    #server_st{
                nicknames = [],
                channels = []
            }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % starts a genserver for the server process
    genserver:start(ServerAtom, server:initial_state(), fun server:handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes

stop_channels([]) ->
    ok;

stop_channels([Head|Channels]) ->
    Head ! stop,
    catch(unregister(Head)),
    stop_channels(Channels).

stop(ServerAtom) ->
    Channels = genserver:request(ServerAtom, {get_channels}),
    stop_channels(Channels),
    ServerAtom ! stop,
    catch(unregister(ServerAtom)),
    ok.

handle(St, {create_channel, Channel}) -> % starts a genserver for a new channel process
    ChannelAtom = list_to_atom(Channel),
    genserver:start(ChannelAtom, channel:initial_state(Channel), fun channel:handle/2),
    Channels_updated = [ChannelAtom|St#server_st.channels],
    {reply, {channel_created, ChannelAtom}, St#server_st{channels = Channels_updated}};

handle(St, {nick_change, OldNick, NewNick}) ->
    Cond = lists:member(NewNick, St#server_st.nicknames), % Checks if someone has the desired nick
    if
        Cond -> % If so, return error
            {reply, {error, nick_taken, "The nickname is already taken"}, St};
        true -> % Otherwise, update the nickname record, and return ok
            Nicknames_updated = lists:delete(OldNick, [NewNick|St#server_st.nicknames]),
            {reply, {ok_nick_change}, St#server_st{nicknames = Nicknames_updated}}
    end;

handle(St, {nick_init, Nick}) -> % Adding nicknames from init to nicknames list
    Nicknames_updated = [Nick|St#server_st.nicknames],
    {reply, {ok_nick_init}, St#server_st{nicknames = Nicknames_updated}};

handle(St, {get_channels}) ->
    {reply, St#server_st.channels, St}.
