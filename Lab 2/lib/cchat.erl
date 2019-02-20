-module(cchat).
-export([server/0,client/0]).
-define(SERVERNAME,shire).

% Start a server
server() ->
    server:start(?SERVERNAME).

% Start a client GUI
client() ->
    %gui:dumb().
    gui:start(?SERVERNAME).

%cd("C:/Users/danie/Documents/Y3-P3 Parallelprogrammering/Lab 2/lib").