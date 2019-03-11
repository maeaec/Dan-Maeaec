-module(server).
-export([initial_state/0,start/1,stop/1,handle/2]).

-record(server_st, {
    nicknames, % List of taken nicknames
    channels % List of created channels
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
stop(ServerAtom) ->
    Channels = genserver:request(ServerAtom, {get_channels}), % Gets the list of created channels of the server
    [Channel ! stop||Channel <- Channels], % Stops all associated channel processes of the server
    ServerAtom ! stop,
    catch(unregister(ServerAtom)),
    ok.

handle(St, {join, Channel, From}) ->
    ChannelAtom = list_to_atom(Channel),
    ChannelPid = whereis(ChannelAtom),

    if
        ChannelPid == undefined -> % channel doesn't exist yet, starting the channel and joining it
            genserver:start(ChannelAtom, channel:initial_state(Channel), fun channel:handle/2),
            case catch (genserver:request(ChannelAtom, {join, From})) of
                {ok_join} ->
                    Channels_updated = [ChannelAtom|St#server_st.channels],
                    {reply, {ok_join}, St#server_st{channels = Channels_updated}}
            end;
        true -> % Channel does exist, send join to channel
            case catch (genserver:request(ChannelAtom, {join, From})) of
                {ok_join} ->
                    {reply, {ok_join}, St};
                {error, user_already_joined, Text} ->
                    {reply, {error, user_already_joined, Text}, St}
            end
    end;

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

handle(St, {get_channels}) -> % Returns the list of created channels, to be used by the function stop
    {reply, St#server_st.channels, St}.
