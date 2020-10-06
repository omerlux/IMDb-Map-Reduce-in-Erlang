%%%-------------------------------------------------------------------
%%% @author Ilay
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. ספט׳ 2020 20:39
%%%-------------------------------------------------------------------
-module(wxclient).
-author("Ilay-Omer").

%% API
-export([start/0, readfile/1]).
-include_lib("wx/include/wx.hrl").

%%-record(state, {parent, config}).
-record(movie_data, {id, title, original_title, year,
  date_published, genre, duration, country, language, director,
  writer, production_company, actors, description, avg_vote,
  votes, budget, usa_gross_income, worlwide_gross_income,
  metascore, reviews_from_users, reviews_from_critics}).
-record(query, {type, searchVal, searchCategory, resultCategory}).

%% Will get the pid of server
%% will send the query on button pressing
start() ->

  %%--------------------- Frame and components build -------------------------------------------------------------------
  WX = wx:new(),
  Frame = wxFrame:new(WX, 1, "IMDb Map-Reduce Project"),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
%%  Panel = wxPanel:new(Frame, []),
%%  wxPanel:setSizer(Panel, MainSizer),
%%  wxPanel:setBackgroundColour(Panel,?wxBLACK),
  SubSizer1 = wxBoxSizer:new(?wxVERTICAL),
%%  Logo = wxBitmap:new("../images/imdb logo.png"),
%%  wxSizer:add(MainSizer, Logo),
  TopTxt = wxStaticText:new(Frame, ?wxID_ANY, "Query Window"),
  Headline = wxStaticText:new(Frame, ?wxID_ANY, "Insert Value (case-sensitive):"),
  TextCtrl = wxTextCtrl:new(Frame, 1, [{value, ""}, {style, ?wxDEFAULT}]),
  wxTextCtrl:setToolTip(TextCtrl, "Enter your search value here (case-sensitive)"),
  TextCtrlValidation = wxTextCtrl:new(Frame, 1, [{value, ""}, {style, ?wxDEFAULT}]),
  ButtonSend = wxButton:new(Frame, ?wxID_ANY, [{label, "Send Query"}, {style, ?wxBU_EXACTFIT}]),
  wxButton:setToolTip(ButtonSend, "Send your query to the disco"),
  Choices = ["Title", "Year", "Genre", "Duration", "Country", "Language", "Director", "Writer", "Production Company", "Actor", "Description", "Score", "Budget"],
  SortingChoices = ["ID", "Title", "Year", "Genre", "Duration", "Country", "Language", "Director", "Writer", "Production Company", "Actor", "Description", "Score", "Budget"],
  %% Create a wxListBox that uses multiple selection
  ListBox = wxListBox:new(Frame, 1, [{size, {-1, 100}},
    {choices, Choices}, {style, ?wxLB_SINGLE}]),
  wxListBox:setToolTip(ListBox, "Choose your search value category"),
  CheckBoxes =
    [wxCheckBox:new(Frame, 1, "Duration", []),
      wxCheckBox:new(Frame, 2, "Genre", []),
      wxCheckBox:new(Frame, 3, "Country", []),
      wxCheckBox:new(Frame, 4, "Language", []),
      wxCheckBox:new(Frame, 5, "Director", []),
      wxCheckBox:new(Frame, 6, "Writer", []),
      wxCheckBox:new(Frame, 7, "Actors", []),
      wxCheckBox:new(Frame, 8, "Production", []),
      wxCheckBox:new(Frame, 9, "Score", []),
      wxCheckBox:new(Frame, 10, "Budget", []),
      wxCheckBox:new(Frame, 11, "Description", []),
      wxCheckBox:new(Frame, 12, "Year", [])],

  %%--------------------- Sizers and Events setup ----------------------------------------------------------------------
  wxSizer:add(SubSizer1, TopTxt, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, Headline, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, TextCtrl, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ListBox, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  CheckSizer = wxStaticBoxSizer:new(?wxVERTICAL, Frame,
    [{label, "Select Categories (ID, Title are always selected):"}]),

  Fun = %%Adding checkboxes to the sizer
  fun(Item) ->
    wxSizer:add(CheckSizer, Item)
  end,
  wx:foreach(Fun, CheckBoxes),
  wxSizer:add(SubSizer1, CheckSizer),
  ListBoxSort = wxListBox:new(Frame, 1, [{size, {-1, 100}},
    {choices, SortingChoices}, {style, ?wxLB_SINGLE}]),
  wxSizer:add(SubSizer1, ListBoxSort, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxListBox:setToolTip(ListBoxSort, "Sort by"),
  %%Event configuration for button click:
  wxEvtHandler:connect(ButtonSend, command_button_clicked, [{callback, fun handle_click_event/2},
    {userData, {wx:get_env(), TextCtrl, ListBox, CheckBoxes, TextCtrlValidation, ListBoxSort}}]),

  wxSizer:add(SubSizer1, ButtonSend, [{flag, ?wxALL}, {border, 8}]),
  wxSizer:add(SubSizer1, TextCtrlValidation, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL, [{underlined, true}]),
  wxTextCtrl:setFont(TopTxt, Font),
  wxSizer:add(MainSizer, SubSizer1, [{flag, ?wxALIGN_LEFT}, {border, 5}]),
  wxWindow:setSizer(Frame, MainSizer),
  wxFrame:show(Frame).


handle_click_event(A = #wx{}, _B) ->
  {Env, TextBox, ListBox, CheckBoxes, TextCtrlValidation, ListBoxSort} = A#wx.userData,
  wx:set_env(Env),
  %%Create the resultCategory true/false tuple:
  Categories2Show = #movie_data{
    id = true,
    title = true,
    original_title = false,
    year = wxCheckBox:getValue(lists:nth(12, CheckBoxes)),
    date_published = false,
    genre = wxCheckBox:getValue(lists:nth(2, CheckBoxes)),
    duration = wxCheckBox:getValue(lists:nth(1, CheckBoxes)),
    country = wxCheckBox:getValue(lists:nth(3, CheckBoxes)),
    language = wxCheckBox:getValue(lists:nth(4, CheckBoxes)),
    director = wxCheckBox:getValue(lists:nth(5, CheckBoxes)),
    writer = wxCheckBox:getValue(lists:nth(6, CheckBoxes)),
    production_company = wxCheckBox:getValue(lists:nth(8, CheckBoxes)),
    actors = wxCheckBox:getValue(lists:nth(7, CheckBoxes)),
    description = wxCheckBox:getValue(lists:nth(11, CheckBoxes)),
    avg_vote = wxCheckBox:getValue(lists:nth(9, CheckBoxes)),
    votes = false,
    budget = wxCheckBox:getValue(lists:nth(10, CheckBoxes)),
    usa_gross_income = false,
    worlwide_gross_income = false,
    metascore = false,
    reviews_from_users = false,
    reviews_from_critics = false
  },
  %% --------------------- Input validation ----------------------------------------------------------------------------
  Func_IsTextInt =
    fun(List) ->
      case lists:flatlength(List) > 0 of
        true ->
          List2 = lists:flatten(List),
          Max = lists:max(List2),
          Min = lists:min(List2),
          (48 =< Min) and (Max =< 57);  %%ASCII of 0,9
        false -> false
      end
    end,

  Func_IsTextDouble =
    fun(List) ->
      case lists:flatlength(List) == 3 of
        true ->
          List2 = lists:flatten(List),
          (((lists:nth(1, List2) >= 48) and (lists:nth(3, List2) >= 48)) and
            (((lists:nth(1, List2) =< 57) and (lists:nth(3, List2) =< 57)) and
              (lists:nth(2, List2) =:= 46)));
        false -> false
      end
    end,

  IsInputValid =
    (
        (wxListBox:getSelection(ListBox) /= -1) and
          (
              ((wxListBox:getSelection(ListBox) == 3) and Func_IsTextInt(wxTextCtrl:getValue(TextBox))) or %%DURATION
              (
                  ((wxListBox:getSelection(ListBox) == 1) and Func_IsTextInt(wxTextCtrl:getValue(TextBox))) or %%YEAR
                  (
                      ((wxListBox:getSelection(ListBox) == 11) and (Func_IsTextInt(wxTextCtrl:getValue(TextBox)) or Func_IsTextDouble(wxTextCtrl:getValue(TextBox)))) or %%SCORE
                      ((wxListBox:getSelection(ListBox) /= 11) and ((wxListBox:getSelection(ListBox) /= 3) and (wxListBox:getSelection(ListBox) /= 1)))
                  )
              )
          )
    ),

  case IsInputValid of
    true ->
      wxTextCtrl:setLabel(TextCtrlValidation, "The input is valid"),
      %%---------------------------------- Sending the query to the master: ------------------------------------------------
      Query = #query{type = generic,
        searchVal = wxTextCtrl:getValue(TextBox),
        searchCategory = wxListBox:getString(ListBox, wxListBox:getSelection(ListBox)), resultCategory = Categories2Show},
      [MasterNode | _T] = readfile(["clientslist.txt"]),
      StartTime = os:timestamp(),%% for performance evaluation
      _Ack = gen_server:call({masterpid, list_to_atom(MasterNode)}, Query), %% we receive acknowledge and the result will arrive from another process
      receive
      %% ************* Handling Query Results Here: *******************
        [] ->
          WindowZero = wxWindow:new(),
          FrameZero = wxFrame:new(WindowZero, ?wxID_ANY, "No Results"),
          MainSizerZero = wxBoxSizer:new(?wxVERTICAL),
          TextZero = wxStaticText:new(FrameZero, ?wxID_ANY,"There are 0 results for the requested qeury"),
          wxSizer:add(MainSizerZero, TextZero, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
          wxWindow:setSizer(FrameZero, MainSizerZero),
          wxFrame:show(FrameZero),
          wxWindow:show(WindowZero);
        MoviesRaw when is_list(MoviesRaw) ->
          Movies = lists:sort(fun(M1 = #movie_data{}, M2 = #movie_data{}) ->
            (getValueForSorting(M1, wxListBox:getSelection(ListBoxSort)) < getValueForSorting(M2, wxListBox:getSelection(ListBoxSort))) end, MoviesRaw),
          TotalTime = round(timer:now_diff(os:timestamp(), StartTime) / 1000),%% for performance evaluation
          %% Setup sizers and frames:
          Window2 = wxWindow:new(),
          Frame = wxFrame:new(Window2, ?wxID_ANY, "Results"),
          wxFrame:center(Frame),
          Panel = wxPanel:new(Frame, []),
          MainSizer = wxBoxSizer:new(?wxVERTICAL),
          NumberOfResults = lists:flatlength(Movies),
          Label = "Search value: " ++ Query#query.searchVal ++ " | Value category: " ++ Query#query.searchCategory
            ++ " | " ++ integer_to_list(NumberOfResults) ++ " Results | Evaluation Time: " ++ integer_to_list(TotalTime) ++ "ms",
          Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, Label}]),

          %% Creating the results table:
          Grid = create_grid(Panel, Movies, NumberOfResults, Query),

          %% Add to sizers and show:
          Options = [{flag, ?wxEXPAND}, {proportion, 1}],
          wxSizer:add(Sizer, Grid, Options),
          wxSizer:add(MainSizer, Sizer, Options),
          wxPanel:setSizer(Panel, MainSizer),
          wxFrame:show(Frame),
          wxWindow:show(Window2);
      %% ************************************************************
        _ -> %% We didn't receive a list - error
          Window2 = wxWindow:new(),
          Frame2 = wxFrame:new(Window2, ?wxID_ANY, "Error"),
          wxStaticText:new(Frame2, ?wxID_ANY, "An error occured."),
          wxFrame:show(Frame2),
          wxWindow:show(Window2)
      end;
    false -> wxTextCtrl:setLabel(TextCtrlValidation, "The input is invalid")
  end.

%% Creating the result's table
create_grid(Panel, [Datum2 = #movie_data{} | T], NumberOfResults, Query = #query{}) ->
  Grid = wxGrid:new(Panel, 2, []),
  %% Setting up the table size and labels:
  ResultsCategories = Query#query.resultCategory,
  NumberOfCategories = countTrueCategories(ResultsCategories, 3, 2), %% Counting how many categories the user chose
  wxGrid:createGrid(Grid, NumberOfResults, NumberOfCategories - 1),
  wxGrid:setColLabelValue(Grid, 0, "ID"),
  wxGrid:setColLabelValue(Grid, 1, "Title"),
  %% Setting the columns:
  Counter = counters:new(1, []), %% Counter for placing the chosen categories in the columns
  counters:add(Counter, 1, 2),
  if Query#query.resultCategory#movie_data.description =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Description"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.duration =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Duration (minutes)"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.genre =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Genres"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.country =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Country"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.language =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Language"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.director =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Director"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.writer =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Writer"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.actors =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Actors"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.production_company =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Production"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.avg_vote =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Score"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.budget =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Budget"),
    counters:add(Counter, 1, 1);
    true -> void
  end,

  if Query#query.resultCategory#movie_data.year =:= true ->
    wxGrid:setColLabelValue(Grid, counters:get(Counter, 1), "Year"),
    counters:add(Counter, 1, 1);
    true -> void
  end,
  %% Inserting the data to the table:
  Func =
    fun({RowNumber, Datum = #movie_data{}}) ->
      Counter2 = counters:new(1, []),
      counters:put(Counter2, 1, 2),
      wxGrid:setCellValue(Grid, RowNumber, 0, Datum#movie_data.id),
      wxGrid:setCellValue(Grid, RowNumber, 1, Datum#movie_data.title),
      if Query#query.resultCategory#movie_data.description =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.description),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.duration =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.duration),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.genre =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.genre),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.country =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.country),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.language =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.language),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.director =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.director),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.writer =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.writer),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.actors =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.actors),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.production_company =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.production_company),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.avg_vote =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.avg_vote),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.budget =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.budget),
        counters:add(Counter2, 1, 1);
        true -> void
      end,
      if Query#query.resultCategory#movie_data.year =:= true ->
        wxGrid:setCellValue(Grid, RowNumber, counters:get(Counter2, 1), Datum#movie_data.year),
        counters:add(Counter2, 1, 1);
        true -> void
      end
    end,
  NumberingList = lists:seq(0, NumberOfResults - 1),
  RowsList = lists:zip(NumberingList, [Datum2 | T]),
  wx:foreach(Func, RowsList),
  Grid;

create_grid(_, _, _, _) ->
  invalid_params.

%% readfile - read file as strings separated by lines
readfile(FileName) ->
  try
    {ok, Binary} = file:read_file(FileName),
    string:tokens(erlang:binary_to_list(Binary), "\r\n")
  catch
    error: _Error -> {os:system_time(), error, "Cannot find the given master"}
  end.

countTrueCategories(ResultsCategories = #movie_data{}, 22, Count) ->
  if element(22, ResultsCategories) =:= true -> Count + 1;
    true -> Count
  end;

countTrueCategories(ResultsCategories = #movie_data{}, StartIndex, Count) ->
  if element(StartIndex, ResultsCategories) =:= true ->
    countTrueCategories(ResultsCategories, StartIndex + 1, Count + 1);
    true -> countTrueCategories(ResultsCategories, StartIndex + 1, Count)
  end;

countTrueCategories(_, _, _) -> invalid_params.

getValueForSorting(Movie = #movie_data{}, SortParam) ->
  if
    SortParam == 0 -> Movie#movie_data.id;
    SortParam == 1 -> Movie#movie_data.title;
    SortParam == 2 -> Movie#movie_data.year;
    SortParam == 3 -> Movie#movie_data.genre;
    SortParam == 4 -> Movie#movie_data.duration;
    SortParam == 5 -> Movie#movie_data.country;
    SortParam == 6 -> Movie#movie_data.language;
    SortParam == 7 -> Movie#movie_data.director;
    SortParam == 8 -> Movie#movie_data.writer;
    SortParam == 9 -> Movie#movie_data.production_company;
    SortParam == 10 -> Movie#movie_data.actors;
    SortParam == 11 -> Movie#movie_data.description;
    SortParam == 12 -> Movie#movie_data.avg_vote;
    SortParam == 13 -> Movie#movie_data.budget;
    true -> Movie#movie_data.title
  end.





