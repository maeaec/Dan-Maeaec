-module(channel).
-export([initial_state/1,handle/2]).

-record(channel_st, {
    members, % List of all members of the channel
    channel % the name of the channel
}).

initial_state(Channel) -> % Initialising with the name of the channel
    #channel_st{
                members = [],
                channel = Channel
            }.

% broadcast sends a message with a nick to everyone else in the channel
broadcast(_Channel, [], _Msg, _From, _Nick) ->
    ok; % Base case

broadcast(Channel, [Head|Members], Msg, From, Nick) -> % Recursive, checking head of list
    if
        From =/= Head -> % Not sending to the sender
            %Ref = make_ref(),
            genserver:request(Head, {message_receive, Channel, Nick, Msg});
            %case catch (genserver:request(Head, {message_receive, Channel, Nick, Msg})) of
            %    {reply, ok, _St} ->
            %        ok;
            %    _ ->
            %        genserver:request(From, {message_not_received})
            %end;
            %Ans = genserver:request(Head, {message_receive, Channel, Nick, Msg}),
            %Cond = {reply, ok, _} == Ans,
            %if
            %    Cond ->
            %        ok;
            %    true ->
            %        genserver:request(From, {message_not_received})
            %end;
            %Head!{request, self(), Ref, {message_receive, Channel, Nick, Msg}};
        true ->
            ok
    end,
    broadcast(Channel, Members, Msg, From, Nick). % Recursive call

% Handles messages sent to channel
handle(St, {join, From}) ->
    Cond = not lists:member(From, St#channel_st.members), % Checking membership in this channel
    if
        Cond -> % If not member, add to member list, return ok
            NewMembers = [From|St#channel_st.members],
            {reply, {ok_join}, St#channel_st{members = NewMembers}};
        true -> % If already member, return error
            {reply, {error, user_already_joined, "Trying to join a channel the client is already in"}, St}
    end;

handle(St, {leave, From}) ->
    Cond = lists:member(From, St#channel_st.members),
    if
        Cond -> % If member, remove from member list, return ok
            NewMembers = lists:delete(From, St#channel_st.members),
            {reply, {ok_leave}, St#channel_st{members = NewMembers}};
        true -> % If not member, return error
            {reply, {error, user_not_joined, "Trying to leave a channel the client is not in"}, St}
    end;

handle(St, {message_send, Msg, From, Nick}) ->
    Cond = lists:member(From, St#channel_st.members),
    if
        Cond -> % Can only send on the channel if member
            spawn(fun() -> broadcast(St#channel_st.channel, St#channel_st.members, Msg, From, Nick) end),
            {reply, {ok_message_send}, St};
        true ->
            {reply, {error, user_not_joined, "Trying to send a message to a channel the client is not in"}, St}
    end.
