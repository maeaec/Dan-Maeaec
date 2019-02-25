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
    Ref = make_ref(),

    Server_name = St#client_st.server,

    Server_name ! {join, Channel, self(), Ref},
    %io:format('Join sent by client~n'),

    receive
        {ok_join, Ref} ->
            %io:format('Join ack received by client~n'),
            {reply, ok, St};
        {user_already_joined, Ref} ->
            io:format('User already joined~n'),
            {reply, {error, user_already_joined, "User already joined"}, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    Ref = make_ref(),

    Server_name = St#client_st.server,

    Server_name ! {leave, Channel, self(), Ref},
    %io:format('Leave sent by client~n'),

    receive
        {ok_leave, Ref} ->
            %io:format('Leave ack received by client~n'),
            {reply, ok, St}
    end;
   %{reply, {error, not_implemented, "leave not implemented"}, St} ;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    Ref = make_ref(),

    Server_name = St#client_st.server,
    From = self(),
    Nick = St#client_st.nick,

    Server_name ! {message_send, Channel, Msg, From, Nick, Ref},
    %io:format('Message_send sent by client~n'),

    receive
        {ok_message_send, Ref} ->
            %io:format('Message_send ack received by client~n'),
            {reply, ok, St}
    end;
    %{reply, {error, not_implemented, "message sending not implemented"}, St} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

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
