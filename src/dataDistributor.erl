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
  NumRows = lists:flatlength(CSV),
  Servers = readfile(["serverslist.txt"]),
  NumServers = countList(Servers),
  sendData(CSV, Servers, {2, NumRows, ceil(NumRows/NumServers)}). % {start, stop, jump}


%% readfile - read file as strings separated by lines
readfile(FileName) ->
  {ok, Binary} = file:read_file(FileName),
  string:tokens(erlang:binary_to_list(Binary), "\r\n").


%% countList - returning the number of string elements in the list
countList([_|T]) ->
  count(1, T).
count(X,[_|T]) ->
  count(X+1, T);
count(X,[]) ->
  X.


%% sendData - sending data to all servers
sendData(CSV, [S0|Servers], {2, NumRows, Jump}) ->
  % spawning a sender process
  spawn(dataDistributor, sendServerJob, [CSV, S0, 2, Jump]),
  io:format("send to ~p, lines ~p to ~p.~n",[S0, 2, Jump]),
  sendData(CSV, Servers, {Jump + 1, NumRows, Jump});

sendData(_, _, {Overshoot, NumRows, _}) when Overshoot >= NumRows ->
  io:format("Sent all data...");

sendData(CSV, [Sx|Servers], {Start, NumRows, Jump}) ->
  Stop = min(Start + Jump, NumRows),
  % spawning a sender process
  spawn(dataDistributor, sendServerJob, [CSV, Sx, Start, Stop]),
  io:format("send to ~p, lines ~p to ~p.~n",[Sx, Start, Stop]),
  sendData(CSV, Servers, {Stop + 1, NumRows, Jump}).


%% sendServerJob - sending server x the data it needs
sendServerJob(CSV, Server, StartLine, StopLine) ->
  ServerNode = list_to_atom(Server),
  SubList = lists:sublist(CSV, StartLine, StopLine),
  % sending from gen_server the values of the data
  gen_server:cast({serverpid, ServerNode}, {store, SubList}).

%% serverPidGetter - returning the pid of the server to connect to.
%%serverPidGetter(Server) ->
%%  list_to_atom(
%%    string:sub_string(Server, 1, string:cspan(Server, "@")) ++ "pid"
%%  ).


%%
%%etssaveit(RowNum) ->
%%  % see https://github.com/isaksamsten/erlang-csv
%%  {csv, Parser} = csv:binary_reader("../csvexample.csv", 3),
%%  parse(Parser, RowNum).
%%
%%parse(_, 0) ->
%%  %TODO: return parser to continue from where i stopped...
%%  io:format("EOS!!!!");
%%parse(Parser, RowNum) ->
%%  case csv:next_line(Parser) of
%%    {row, [Imdb_title_id, Title, Original_title, Year, Date_published, Genre, Duration, Country, Language,
%%      Director, Writer, Production_company, Actors, Description, Avg_vote, Votes, Budget, Usa_groosincome,
%%      Worldwide_groos_income, Metascore, Reviews_from_users, Reviews_from_critics], Id} ->
%%      io:format("~p: id- ~p title- ~p orig_title- ~p ~n
%%      year- ~p date- ~p genre- ~p duration- ~p country- ~p language~p ~n
%%      director- ~p writer- ~p prod_comp- ~p actors- ~p ~n
%%      description- ~p~n
%%       avg_vote- ~p votes- ~p budget- ~p usa_gross- ~p ww_gross- ~p metascore- ~p reviews_usrs- ~p reviews_critics- ~p ~n
%%       ===================~n",
%%        [Id, Imdb_title_id, Title, Original_title, Year, Date_published, Genre, Duration, Country, Language,
%%        Director, Writer, Production_company, Actors, Description, Avg_vote, Votes, Budget, Usa_groosincome,
%%        Worldwide_groos_income, Metascore, Reviews_from_users, Reviews_from_critics]);
%%    {annotation, {Key, Value}} ->
%%      io:format("~p: ~p ~n", [Key, Value]);
%%    eof ->
%%      io:format("End of csv-file ~n")
%%  end,
%%  parse(Parser, RowNum - 1).