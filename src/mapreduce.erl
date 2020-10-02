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
-export([map/2]).


%% get - return the values according to the request
map(Table, Query = #query{}) ->
  % mapping the values by the searchCategory, searchVal
  List =
    case Query#query.searchCategory of
      "Title" -> ets:match_object(Table, {_, #movie_data{title = Query#query.searchVal}})
    end.
