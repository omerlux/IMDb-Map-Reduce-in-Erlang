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
-export([distribute/0, readfile/1]).


%% distribute - distributing the CSV file rows to each server.
%% note: there aren't any duplicated data. the data is being shuffled
distribute() ->
  Start = os:timestamp(),
  CSV_unshuffled = parse_csv:main(["../IMDb movies.csv"]),   % getting csv file
  CSV = [Y || {_,Y} <- lists:sort([{rand:uniform(), N} || N <- CSV_unshuffled])], % shuffling it randomly
  NumRows = lists:flatlength(CSV) - 1,
  Servers = checkAlive(readfile(["serverslist.txt"]), []),
  NumServers = lists:flatlength(Servers),
  case NumServers>0 of
    true -> sendData(CSV, Servers, {2, NumRows, ceil(NumRows / NumServers)}), % {start, stop, jump}
      io:format("Sent ~p movie records to ~p servers in ~p ms.~n", [NumRows, NumServers, round(timer:now_diff(os:timestamp(), Start) / 1000)]);
    false -> io:format("No servers available...~n")
  end,
  {NumServers,Servers}.


%% readfile - read file as strings separated by lines
readfile(FileName) ->
  try
    {ok, Binary} = file:read_file(FileName),
    string:tokens(erlang:binary_to_list(Binary), "\r\n")
  catch
    error: _Error -> {os:system_time(), error, "Cannot read the csv file"}
  end.


%% sendData - sending data to all servers
sendData(_CSV, [], {_, _, _}) ->
  ok;

sendData(CSV, [Sx | Servers], {Start, NumRows, Jump}) ->
  if
    Start + Jump < NumRows -> NextStart = Start + Jump;
    true -> NextStart = NumRows + 2
  end,
  % spawning a sender process
  spawn(fun() -> sendServerJob(lists:sublist(CSV, Start, Jump), Sx) end),
  io:format("send to ~p, lines ~p to ~p.~n", [Sx, Start, NextStart - 1]),
  sendData(CSV, Servers, {NextStart, NumRows, Jump}).


%% sendServerJob - sending server x the data it needs
sendServerJob(CSV_Partition, ServerNode) ->
  % sending from gen_server the values of the data
  gen_server:cast({serverpid, ServerNode}, {store, CSV_Partition}).


%% checkAlive - check each server if alive. Returns a list of server nodes which are alive
checkAlive([S0 | Servers], Nodes) ->
  Node = list_to_atom(S0),
  case net_kernel:connect_node(Node) of
    true -> % erlang:monitor_node(Node, true),    % monitoring the node!
      checkAlive(Servers, [Node | Nodes]);
    false -> checkAlive(Servers, Nodes)
  end;
checkAlive([], Servers) -> Servers.