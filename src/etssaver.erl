%%%-------------------------------------------------------------------
%%% @author Omer
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Oct 2020 10:16
%%%-------------------------------------------------------------------
-module(etssaver).
-author("Ilay-Omer").

%% API
-export([etssaveit/1]).


etssaveit(RowNum) ->
  % see https://github.com/isaksamsten/erlang-csv
  {csv, Parser} = csv:binary_reader("../csvexample.csv", [{annotation, true}]),
  parse(Parser, RowNum).

parse(_, 0) ->
  %TODO: return parser to continue from where i stopped...
  io:format("EOS!!!!");
parse(Parser, RowNum) ->
  case csv:next_line(Parser) of
    {row, [Imdb_title_id, Title, Original_title, Year, Date_published, Genre, Duration, Country, Language,
      Director, Writer, Production_company, Actors, Description, Avg_vote, Votes, Budget, Usa_groosincome,
      Worldwide_groos_income, Metascore, Reviews_from_users, Reviews_from_critics], Id} ->
      io:format("~p: id- ~p title- ~p orig_title- ~p ~n
      year- ~p date- ~p genre- ~p duration- ~p country- ~p language~p ~n
      director- ~p writer- ~p prod_comp- ~p actors- ~p ~n
      description- ~p~n
       avg_vote- ~p votes- ~p budget- ~p usa_gross- ~p ww_gross- ~p metascore- ~p reviews_usrs- ~p reviews_critics- ~p ~n
       ===================~n",
        [Id, Imdb_title_id, Title, Original_title, Year, Date_published, Genre, Duration, Country, Language,
        Director, Writer, Production_company, Actors, Description, Avg_vote, Votes, Budget, Usa_groosincome,
        Worldwide_groos_income, Metascore, Reviews_from_users, Reviews_from_critics]);
    {annotation, {Key, Value}} ->
      io:format("~p: ~p ~n", [Key, Value]);
    eof ->
      io:format("End of csv-file ~n")
  end,
  parse(Parser, RowNum - 1).