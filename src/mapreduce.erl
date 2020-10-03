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
    _ -> table_error
  end.

%% map -
% generic map - searching searchVal in searchCategory.
map(Table, Query = #query{type = generic}) ->
  TupleList =
    case Query#query.searchCategory of

      "Title" -> ets:select(Table,
        [{{'_', #movie_data{title = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{title = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      "Year" -> ets:match_object(Table,
        {'$0', #movie_data{year = Query#query.searchVal, _ = '_'}});

      "Genre" -> ets:select(Table,
        [{{'_', #movie_data{genre = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{genre = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      % note: try that... (range of +-5)
      "Duration" ->
        {Int, _} = string:to_integer(Query#query.searchVal),
        Int1 = integer_to_list(Int - 4), Int2 = integer_to_list(Int - 3), Int3 = integer_to_list(Int - 2),
        Int4 = integer_to_list(Int - 1),
        Int5 = integer_to_list(Int + 1), Int6 = integer_to_list(Int + 2), Int7 = integer_to_list(Int + 3),
        Int8 = integer_to_list(Int + 4),
        ets:match_object(Table, {'$0', #movie_data{duration = Int, _ = '_'}}) ++
          ets:match_object(Table, {'$0', #movie_data{duration = Int1, _ = '_'}}) ++
          ets:match_object(Table, {'$0', #movie_data{duration = Int2, _ = '_'}}) ++
          ets:match_object(Table, {'$0', #movie_data{duration = Int3, _ = '_'}}) ++
          ets:match_object(Table, {'$0', #movie_data{duration = Int4, _ = '_'}}) ++
          ets:match_object(Table, {'$0', #movie_data{duration = Int5, _ = '_'}}) ++
          ets:match_object(Table, {'$0', #movie_data{duration = Int6, _ = '_'}}) ++
          ets:match_object(Table, {'$0', #movie_data{duration = Int7, _ = '_'}}) ++
          ets:match_object(Table, {'$0', #movie_data{duration = Int8, _ = '_'}});

      "Country" -> ets:select(Table,
        [{{'_', #movie_data{country = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{country = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      "Language" -> ets:select(Table,
        [{{'_', #movie_data{language = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{language = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      "Director" -> ets:select(Table,
        [{{'_', #movie_data{director = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{director = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      "Writer" -> ets:select(Table,
        [{{'_', #movie_data{writer = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{writer = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      "Production Company" -> ets:select(Table,
        [{{'_', #movie_data{production_company = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{production_company = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      "Actor" -> ets:select(Table,
        [{{'_', #movie_data{actors = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{actors = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      "Description" -> ets:select(Table,
        [{{'_', #movie_data{description = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{description = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      "Score" -> ets:select(Table,
        [{{'_', #movie_data{metascore = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{metascore = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      "Budget" -> ets:select(Table,
        [{{'_', #movie_data{budget = ['_' | Query#query.searchVal] ++ '_', _ = '_'}},
          [],
          ['$_']}]) ++
      ets:select(Table,
        [{{'_', #movie_data{budget = Query#query.searchVal ++ '_', _ = '_'}},
          [],
          ['$_']}]);

      _ -> []
    end,
  reduce(lists:usort([X || {_, X} <- TupleList]), Query);

% OTHER map - ????
map(_Table, _Query = #query{}) ->
  % DO SOMETHING...
  anotherquery.


%% reduce -
% generic reduce - taking only the resultCategory.
reduce(MappedList, Query = #query{type = generic}) ->
  case Query#query.resultCategory of
    "All" -> MappedList;
    "Title" -> [X || #movie_data{title = X} <- MappedList];
    "Year" -> [X || #movie_data{year = X} <- MappedList];
    "Genre" -> [X || #movie_data{genre = X} <- MappedList];
    "Duration" -> [X || #movie_data{duration = X} <- MappedList];
    "Country" -> [X || #movie_data{country = X} <- MappedList];
    "Language" -> [X || #movie_data{language = X} <- MappedList];
    "Director" -> [X || #movie_data{duration = X} <- MappedList];
    "Writer" -> [X || #movie_data{writer = X} <- MappedList];
    "Production Company" -> [X || #movie_data{production_company = X} <- MappedList];
    "Actor" -> [X || #movie_data{actors = X} <- MappedList];
    "Description" -> [X || #movie_data{description = X} <- MappedList];
    "Score" -> [X || #movie_data{metascore = X} <- MappedList];
    "Budget" -> [X || #movie_data{budget = X} <- MappedList];
    "Number of results" -> countList(MappedList)
  end.

%% countList - returning the number of string elements in the list
countList([_ | T]) ->
  count(1, T).
count(X, [_ | T]) ->
  count(X + 1, T);
count(X, []) ->
  X.