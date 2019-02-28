-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    Server_pid = whereis(ServerAtom),
    if
        Server_pid == undefined ->
            ok;
        true -> % Sending generated nickname to server
            genserver:request(ServerAtom, {nick_init, Nick})
    end,

    #client_st{
                gui = GUIAtom,
                nick = Nick,
                server = ServerAtom
            }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    Server_name = St#client_st.server,
    Server_pid = whereis(Server_name),
    From = self(),
    
    ChannelAtom = list_to_atom(Channel),
    ChannelPid = whereis(ChannelAtom),

    if
        ChannelPid == undefined -> % channel doesn't exist yet
            if
                Server_pid == undefined-> % server doesn't exist, return error
                    {reply, {error, server_not_reached, "Server is not registered"}, St};
                true -> % server exists, tell server to create a channel with a given name
                    case catch(genserver:request(Server_name, {create_channel, Channel})) of
                        {channel_created, NewChannelAtom} ->
                            % Sent join to the new channel
                            case catch(genserver:request(NewChannelAtom, {join, From})) of
                                % return accordingly
                                {ok_join} ->
                                    {reply, ok, St};
                                {error, user_already_joined, Text} ->
                                    {reply, {error, user_already_joined, Text}, St}
                            end
                    end
            end;
        true -> % Channel does exist, send join to channel
            case catch(genserver:request(ChannelAtom, {join, From})) of
                {ok_join} ->
                    {reply, ok, St};
                {error, user_already_joined, Text} ->
                    {reply, {error, user_already_joined, Text}, St}
            end
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    From = self(),
    
    ChannelAtom = list_to_atom(Channel),
    ChannelPid = whereis(ChannelAtom),
    
    if
        ChannelPid == undefined -> % Channel doesn't exist, return error
            {reply, {error, user_not_joined, "Trying to leave a nonexistent channel"}, St};
        true -> % Channel does exist, send leave to channel
            case catch(genserver:request(ChannelPid, {leave, From})) of
                % Return accordingly
                {ok_leave} ->
                    {reply, ok, St};
                {error, user_not_joined, Text} ->
                    {reply, {error, user_not_joined, Text}, St}
            end
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    From = self(),
    Nick = St#client_st.nick,
    ChannelAtom = list_to_atom(Channel),
    ChannelPid = whereis(ChannelAtom),
    if
        ChannelPid == undefined -> % Channel doesn't exist, return error
            {reply, {error, user_not_joined, "Trying to send a message to a nonexistent channel"}, St};
        true -> % Channel does exist, send message to channel
            Data = {message_send, Msg, From, Nick},
            Ans = genserver:request(ChannelPid, Data),
            case catch(Ans) of
                % Return accordingly
                {ok_message_send} ->
                    {reply, ok, St};
                {error, user_not_joined, Text} ->
                    {reply, {error, user_not_joined, Text}, St}
            end
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    Server_name = St#client_st.server,
    Server_pid = whereis(Server_name),
    OldNick = St#client_st.nick,
    
    if
        Server_pid == undefined -> % Channel doesn't exist, return error
            {reply, {error, server_not_reached, "Server is not registered"}, St};
        true -> % Channel does exist, send nick_change to server
            case catch (genserver:request(Server_name, {nick_change, OldNick, NewNick})) of
                % Return accordingly
                {ok_nick_change} ->
                    {reply, ok, St#client_st{nick = NewNick}};
                {error, nick_taken, Text} ->
                    {reply, {error, nick_taken, Text}, St}
            end
    end;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
