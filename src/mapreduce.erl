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



-record(movie_data, {id, title, original_title, year,
  date_published, genre, duration, country, language, director,
  writer, production_company, actors, description, avg_vote,
  votes, budget, usa_gross_income, worlwide_gross_income,
  metascore, reviews_from_users, reviews_from_critics}).
-record(query,
{
  type,
  searchVal,
  searchCategory,
  resultCategory = #movie_data{}
}).
-record(numOfResults, {number}).

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
map(Table, Query = #query{type = generic, searchVal = SearchVal}) ->
  reduce(
    case Query#query.searchCategory of
      "Title" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.title, SearchVal) > 0];
      "Year" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), Movie#movie_data.year == SearchVal];
      "Genre" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.genre, SearchVal) > 0];
      % note: try that... (range of +-5)
      "Duration" -> [Movie || {_id, Movie} <- ets:tab2list(Table),
        list_to_integer(Movie#movie_data.duration) > list_to_integer(SearchVal) - 5,
        list_to_integer(Movie#movie_data.duration) < list_to_integer(SearchVal) + 5];
      "Country" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.country, SearchVal) > 0];
      "Language" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.language, SearchVal) > 0];
      "Director" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.director, SearchVal) > 0];
      "Writer" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.writer, SearchVal) > 0];
      "Production Company" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.production_company, SearchVal) > 0];
      "Actor" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.actors, SearchVal) > 0];
      "Description" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.description, SearchVal) > 0];
      "Score" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.title, SearchVal) > 0];
      "Budget" ->
        [Movie || {_id, Movie} <- ets:tab2list(Table), string:str(Movie#movie_data.budget, SearchVal) > 0];
      _ -> []
    end, Query);


% OTHER map - ????
map(_Table, _Query = #query{}) ->
  % DO SOMETHING...
  anotherquery.


%% reduce -
% generic reduce - taking only the resultCategory.
reduce(MappedList, Query = #query{type = generic}) ->
  %% each element in list will be construct fromt he query.resultCategory section

  % note: after success try, delete it
%%  Fun_value = fun(Category, Condition, Value) ->
%%    case Condition of
%%      true -> Value;
%%      false -> ""
%%    end
%%    end,
%%  Fun_moviedata_reduce = fun(Hey = #hey{}) ->
%%    Hey_ans = #hey{a = Fun_value(a, false, Hey#hey.a),
%%      b = Fun_value(b, true, Hey#hey.b),
%%      c = Fun_value(c, false, Hey#hey.c)}
%%                         end,
  %% mapping the wanted values -
  % Condition is the resultCategory#movie_data.CATEGORY value
  % Value is the value we want to insert the table
  Fun_value = fun(Condition, Value) ->
    case Condition of
      true -> Value;
      false -> ""
    end
              end,
  Fun_moviedata_reduce = fun(Movie = #movie_data{}, Query) ->
    #movie_data{id = Movie#movie_data.id, title = Movie#movie_data.title,
      original_title = Fun_value(Query#query.resultCategory#movie_data.original_title, Movie#movie_data.original_title),
      year = Fun_value(Query#query.resultCategory#movie_data.year, Movie#movie_data.year),
      date_published = Fun_value(Query#query.resultCategory#movie_data.date_published, Movie#movie_data.date_published),
      genre = Fun_value(Query#query.resultCategory#movie_data.genre, Movie#movie_data.genre),
      duration = Fun_value(Query#query.resultCategory#movie_data.duration, Movie#movie_data.duration),
      country = Fun_value(Query#query.resultCategory#movie_data.country, Movie#movie_data.country),
      language = Fun_value(Query#query.resultCategory#movie_data.language, Movie#movie_data.language),
      director = Fun_value(Query#query.resultCategory#movie_data.director, Movie#movie_data.director),
      writer = Fun_value(Query#query.resultCategory#movie_data.writer, Movie#movie_data.writer),
      production_company = Fun_value(Query#query.resultCategory#movie_data.production_company, Movie#movie_data.production_company),
      actors = Fun_value(Query#query.resultCategory#movie_data.actors, Movie#movie_data.actors),
      avg_vote = Fun_value(Query#query.resultCategory#movie_data.avg_vote, Movie#movie_data.avg_vote),
      votes = Fun_value(Query#query.resultCategory#movie_data.votes, Movie#movie_data.votes),
      budget = Fun_value(Query#query.resultCategory#movie_data.budget, Movie#movie_data.budget),
      usa_gross_income = Fun_value(Query#query.resultCategory#movie_data.usa_gross_income, Movie#movie_data.usa_gross_income),
      worlwide_gross_income = Fun_value(Query#query.resultCategory#movie_data.worlwide_gross_income, Movie#movie_data.worlwide_gross_income),
      metascore = Fun_value(Query#query.resultCategory#movie_data.metascore, Movie#movie_data.metascore),
      reviews_from_users = Fun_value(Query#query.resultCategory#movie_data.reviews_from_users, Movie#movie_data.reviews_from_users),
      reviews_from_critics = Fun_value(Query#query.resultCategory#movie_data.reviews_from_critics, Movie#movie_data.reviews_from_critics)}
                         end,

  [Fun_moviedata_reduce(Movie) || Movie = #movie_data{} <- MappedList].

%%  [#movie_data{id = Movie#movie_data.id, title = Movie#movie_data.title,
%%    original_title = (fun() -> case Query#query.resultCategory#movie_data.original_title == true of
%%                                 true -> Movie#movie_data.original_title;
%%                                 false -> ""
%%                               end end),
%%    year = (fun() -> case Query#query.resultCategory#movie_data.year == true of
%%                       true -> Movie#movie_data.year;
%%                       false -> ""
%%                     end end),
%%    date_published = (fun() -> case Query#query.resultCategory#movie_data.date_published == true of
%%                                 true -> Movie#movie_data.date_published;
%%                                 false -> ""
%%                               end end),
%%    genre = (fun() -> case Query#query.resultCategory#movie_data.genre == true of
%%                        true -> Movie#movie_data.genre;
%%                        false -> ""
%%                      end end),
%%    duration = (fun() -> case Query#query.resultCategory#movie_data.duration == true of
%%                           true -> Movie#movie_data.duration;
%%                           false -> ""
%%                         end end),
%%    country = (fun() -> case Query#query.resultCategory#movie_data.country == true of
%%                          true -> Movie#movie_data.country;
%%                          false -> ""
%%                        end end),
%%    language = (fun() -> case Query#query.resultCategory#movie_data.language == true of
%%                           true -> Movie#movie_data.language;
%%                           false -> ""
%%                         end end),
%%    director = (fun() -> case Query#query.resultCategory#movie_data.director == true of
%%                           true -> Movie#movie_data.director;
%%                           false -> ""
%%                         end end),
%%    writer = (fun() -> case Query#query.resultCategory#movie_data.writer == true of
%%                         true -> Movie#movie_data.writer;
%%                         false -> ""
%%                       end end),
%%    production_company = (fun() -> case Query#query.resultCategory#movie_data.production_company == true of
%%                                     true -> Movie#movie_data.production_company;
%%                                     false -> ""
%%                                   end end),
%%    actors = (fun() -> case Query#query.resultCategory#movie_data.actors == true of
%%                         true -> Movie#movie_data.actors;
%%                         false -> ""
%%                       end end),
%%    avg_vote = (fun() -> case Query#query.resultCategory#movie_data.avg_vote == true of
%%                           true -> Movie#movie_data.avg_vote;
%%                           false -> ""
%%                         end end),
%%    votes = (fun() -> case Query#query.resultCategory#movie_data.votes == true of
%%                        true -> Movie#movie_data.votes;
%%                        false -> ""
%%                      end end),
%%    budget = (fun() -> case Query#query.resultCategory#movie_data.budget == true of
%%                         true -> Movie#movie_data.budget;
%%                         false -> ""
%%                       end end),
%%    usa_gross_income = (fun() -> case Query#query.resultCategory#movie_data.budget == true of
%%                                   true -> Movie#movie_data.budget;
%%                                   false -> ""
%%                                 end end),
%%    worlwide_gross_income = (fun() -> case Query#query.resultCategory#movie_data.budget == true of
%%                                        true -> Movie#movie_data.budget;
%%                                        false -> ""
%%                                      end end),
%%    metascore = (fun() -> case Query#query.resultCategory#movie_data.budget == true of
%%                            true -> Movie#movie_data.budget;
%%                            false -> ""
%%                          end end),
%%    reviews_from_users = (fun() -> case Query#query.resultCategory#movie_data.budget == true of
%%                                     true -> Movie#movie_data.budget;
%%                                     false -> ""
%%                                   end end),
%%    reviews_from_critics = (fun() -> case Query#query.resultCategory#movie_data.budget == true of
%%                                       true -> Movie#movie_data.budget;
%%                                       false -> ""
%%                                     end end)}
%%    || Movie = #movie_data{} <- MappedList].

% TODO: delete it if the gui works well...
%%  case Query#query.resultCategory of
%%    "All" -> MappedList;
%%    "Title" -> [#reduced_data{id = X, title = Y, categoryInfo = Y} || #movie_data{id = X, title = Y} <- MappedList];
%%    "Year" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, year = Z} <- MappedList];
%%    "Genre" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, genre = Z} <- MappedList];
%%    "Duration" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, duration = Z} <- MappedList];
%%    "Country" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, country = Z} <- MappedList];
%%    "Language" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, language = Z} <- MappedList];
%%    "Director" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, director = Z} <- MappedList];
%%    "Writer" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, writer = Z} <- MappedList];
%%    "Production Company" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, production_company = Z} <- MappedList];
%%    "Actor" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, actors = Z} <- MappedList];
%%    "Description" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, description = Z} <- MappedList];
%%    "Score" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, metascore = Z} <- MappedList];
%%    "Budget" ->
%%      [#reduced_data{id = X, title = Y, categoryInfo = Z} || #movie_data{id = X, title = Y, budget = Z} <- MappedList];
%%    "Number of results" -> #numOfResults{number = countList(MappedList)}
%%  end.

%% countList - returning the number of string elements in the list
countList([_ | T]) ->
  count(1, T).
count(X, [_ | T]) ->
  count(X + 1, T);
count(X, []) ->
  X.