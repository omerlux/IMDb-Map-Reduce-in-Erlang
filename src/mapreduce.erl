%%%-------------------------------------------------------------------
%%% @author Omer
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2020 14:20
%%%-------------------------------------------------------------------
-module(mapreduce).
-author("Ilay-Omer").

-record(query,
{
  type,
  searchVal,
  searchCategory,
  resultCategory
}).

-record(movie_data, {id, title, original_title, year,
  date_published, genre, duration, country, language, director,
  writer, production_company, actors, description, avg_vote,
  votes, budget, usa_gross_income, worlwide_gross_income,
  metascore, reviews_from_users, reviews_from_critics}).

%% API
-export([get/2]).


%% get - return the values according to the request
get(TableName, Query = #query{}) ->
  % mapping the values by the searchCategory, searchVal
  case ets:file2tab(TableName) of
    {ok, Table} ->
      Ans = map(Table, Query),
      ets:delete(Table),
      Ans;
    _ -> error
  end.

%% map -
% generic map - searching searchVal in searchCategory.
map(Table, Query = #query{type = generic}) ->
  TupleList =
    case Query#query.searchCategory of
      "Title" -> ets:match_object(Table, {'$0', #movie_data{title = Query#query.searchVal, _='_'}});
      "Year" -> ets:match_object(Table, {'$0', #movie_data{year = Query#query.searchVal, _='_'}});
      _ -> none
    end,
  List = [X || {'$0', X} <- TupleList];

% OTHER map - ????
map(Table, Query = #query{}) ->
  % DO SOMETHING...
  ok.
