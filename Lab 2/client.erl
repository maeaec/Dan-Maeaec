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
    %From = self(),
    %Ref = make_ref(),
    Server_pid = whereis(ServerAtom),
    if
        Server_pid == undefined ->
            ok; %self() ! stop;
        true ->
            ServerAtom ! {nick_init, Nick}
    end,

    #client_st{
                gui = GUIAtom,
                nick = Nick,
                server = ServerAtom
            }.
    %????????
    

    %receive
    %    {ok_nick_init, Ref} ->
    %        St
    %after
    %    3000 ->
    %        {reply, {error, server_not_reached, "Received no response from the server for nick init request"}, St}
    %end.


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
    Ref = make_ref(),

    if
        Server_pid == undefined->
            {reply, {error, server_not_reached, "Server is not registered"}, St};
        true ->
            Server_name ! {join, Channel, From, Ref},
            %io:format('Join sent by client~n'),

            receive
                {ok_join, Ref} ->
                    %io:format('Join ack received by client~n'),
                    {reply, ok, St};
                {error, user_already_joined, Text, Ref} ->
                    {reply, {error, user_already_joined, Text}, St}
            after
                3000 ->
                    {reply, {error, server_not_reached, "Received no response from the server for join request"}, St}
            end
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    Server_name = St#client_st.server,
    Server_pid = whereis(Server_name),
    From = self(),
    Ref = make_ref(),
    
    if
        Server_pid == undefined->
            {reply, {error, server_not_reached, "Server is not registered"}, St};
        true ->
            Server_name ! {leave, Channel, From, Ref},
            %io:format('Leave sent by client~n'),

            receive
                {ok_leave, Ref} ->
                    %io:format('Leave ack received by client~n'),
                    {reply, ok, St};
                {error, user_not_joined, Text, Ref} ->
                    {reply, {error, user_not_joined, Text}, St}
            after
                3000 ->
                    {reply, {error, server_not_reached, "Received no response from the server for leave request"}, St}
            end
            %{reply, {error, not_implemented, "leave not implemented"}, St} ;
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    Server_name = St#client_st.server,
    Server_pid = whereis(Server_name),
    From = self(),
    Nick = St#client_st.nick,
    Ref = make_ref(),
    
    if
        Server_pid == undefined->
            {reply, {error, server_not_reached, "Server is not registered"}, St};
        true ->
            % Catching when the server_name isn't registered??? fatal or recoverable?
            Server_name ! {message_send, Channel, Msg, From, Nick, Ref},
            %io:format('Message_send sent by client~n'),

            receive
                {ok_message_send, Ref} ->
                    %io:format('Message_send ack received by client~n'),
                    {reply, ok, St};
                {error, user_not_joined, Text, Ref} ->
                    {reply, {error, user_not_joined, Text}, St}
            after
                3000 ->
                    {reply, {error, server_not_reached, "Received no response from the server for message send request"}, St}
                    % Fatal or recoverable? what if the server crashed?
            end
            %{reply, {error, not_implemented, "message sending not implemented"}, St} ;
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
    From = self(),
    Ref = make_ref(),
    
    if
        Server_pid == undefined->
            {reply, {error, server_not_reached, "Server is not registered"}, St};
        true ->
            Server_name ! {nick_change, OldNick, NewNick, From, Ref},

            receive
                {ok_nick_change, Ref} ->
                    {reply, ok, St#client_st{nick = NewNick}};
                {error, nick_taken, Text, Ref} ->
                    {reply, {error, nick_taken, Text}, St}
            after
                3000 ->
                    {reply, {error, server_not_reached, "Received no response from the server for nick request"}, St}
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
