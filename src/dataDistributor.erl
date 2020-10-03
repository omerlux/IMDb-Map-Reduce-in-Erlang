%%%-------------------------------------------------------------------
%%% @author Omer
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Oct 2020 10:16
%%%-------------------------------------------------------------------
-module(dataDistributor).
-author("Ilay-Omer").

%% API
%TODO: uncomment this
-compile(export_all).
-export([distribute/0]).


%% distribute - distributing the CSV file rows to each server.
%% NOTE: for now, there aren't any duplicated data..
distribute() ->
  CSV = parse_csv:main(["../csvexample.csv"]),
  NumRows = lists:flatlength(CSV) - 1,
  Servers = readfile(["serverslist.txt"]),
  NumServers = countList(Servers),
  sendData(CSV, Servers, {2, NumRows, ceil(NumRows / NumServers)}). % {start, stop, jump}


%% readfile - read file as strings separated by lines
readfile(FileName) ->
  {ok, Binary} = file:read_file(FileName),
  string:tokens(erlang:binary_to_list(Binary), "\r\n").


%% countList - returning the number of string elements in the list
countList([_ | T]) ->
  count(1, T).
count(X, [_ | T]) ->
  count(X + 1, T);
count(X, []) ->
  X.


%% sendData - sending data to all servers
sendData(_CSV, [], {_, _, _}) ->
  io:format("Sent all data...");

sendData(CSV, [Sx | Servers], {Start, NumRows, Jump}) ->
  if
    Start + Jump < NumRows -> NextStart = Start + Jump;
    true -> NextStart = NumRows + 2
  end,

  % spawning a sender process
  spawn(dataDistributor, sendServerJob, [lists:sublist(CSV, Start, Jump), Sx]),
  io:format("send to ~p, lines ~p to ~p.~n", [Sx, Start, NextStart - 1]),
  sendData(CSV, Servers, {NextStart, NumRows, Jump}).


%% sendServerJob - sending server x the data it needs
sendServerJob(CSV_Partition, Server) ->
  ServerNode = list_to_atom(Server),
  % sending from gen_server the values of the data
  gen_server:cast({serverpid, ServerNode}, {store, CSV_Partition}).

%% serverPidGetter - returning the pid of the server to connect to.
%%serverPidGetter(Server) ->
%%  list_to_atom(
%%    string:sub_string(Server, 1, string:cspan(Server, "@")) ++ "pid"
%%  ).