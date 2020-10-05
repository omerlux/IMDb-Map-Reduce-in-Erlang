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

%% note===========================================================================================
%% note===========================================================================================


%% Will get the pid of server
%% will send the query on button pressing
start() ->

  %%note: Frame and components build -----------------------------------------------------------------------------------
  WX = wx:new(),
  Frame = wxFrame:new(WX, 1, "IMDb Map-Reduce Project"), %%TODO whats the difference from Panel?
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  SubSizer1 = wxBoxSizer:new(?wxVERTICAL),
  TopTxt = wxStaticText:new(Frame, ?wxID_ANY, "Query Window"),
  Headline = wxStaticText:new(Frame, ?wxID_ANY, "Insert Value (case-sensitive):"),
  TextCtrl = wxTextCtrl:new(Frame, 1, [{value, ""}, {style, ?wxDEFAULT}]),
  wxTextCtrl:setToolTip(TextCtrl, "Enter your search value here (case-sensitive)"),
  ButtonSend = wxButton:new(Frame, ?wxID_ANY, [{label, "Send Query"}, {style, ?wxBU_EXACTFIT}]),
  wxButton:setToolTip(ButtonSend, "Send your query to the disco"),
  Choices = ["Title", "Year", "Genre", "Duration", "Country", "Language", "Director", "Writer", "Production Company", "Actor", "Description", "Score", "Budget"],
  %% Create a wxListBox that uses multiple selection
  ListBox = wxListBox:new(Frame, 1, [{size, {-1, 100}},
    {choices, Choices}, {style, ?wxLB_SINGLE}]),
  wxListBox:setToolTip(ListBox, "Choose your search value category"),

  %%note: Sizers setup -------------------------------------------------------------------------------------------------
  wxSizer:add(SubSizer1, TopTxt, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, Headline, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, TextCtrl, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ListBox, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  CheckSizer = wxStaticBoxSizer:new(?wxVERTICAL, Frame,
    [{label, "Select Categories (ID, Title are always selected):"}]),
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
  Fun =
    fun(Item) ->
      %%wxCheckBox:connect(Item, command_checkbox_clicked),
      wxSizer:add(CheckSizer, Item)
    end,
  wx:foreach(Fun, CheckBoxes),
  wxSizer:add(SubSizer1, CheckSizer),
  wxEvtHandler:connect(ButtonSend, command_button_clicked, [{callback, fun handle_click_event/2},
    {userData, {wx:get_env(), TextCtrl, ListBox, CheckBoxes}}]),
  wxSizer:add(SubSizer1, ButtonSend, [{flag, ?wxALL}, {border, 8}]),

  Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL, [{underlined, true}]),
  wxTextCtrl:setFont(TopTxt, Font),
  wxSizer:add(MainSizer, SubSizer1, [{flag, ?wxALIGN_LEFT}, {border, 5}]),
  wxWindow:setSizer(Frame, MainSizer),
  wxFrame:show(Frame).

handle_click_event(A = #wx{}, _B) ->
  {Env, TextBox, ListBox, CheckBoxes} = A#wx.userData,
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
  Query = #query{type = generic,
    searchVal = wxTextCtrl:getValue(TextBox),
    searchCategory = wxListBox:getString(ListBox, wxListBox:getSelection(ListBox)), resultCategory = Categories2Show},
  [MasterNode | _T] = readfile(["clientslist.txt"]),
  _Ack = gen_server:call({masterpid, list_to_atom(MasterNode)}, Query),
  receive
  %% ************* Handling Query Results: *******************
    Movies when is_list(Movies) ->
      Window2 = wxWindow:new(),
      Frame = wxFrame:new(Window2, ?wxID_ANY, "Results"),
      wxFrame:center(Frame),
      Panel = wxPanel:new(Frame, []),
      %% Setup sizers:
      MainSizer = wxBoxSizer:new(?wxVERTICAL),
      NumberOfResults = lists:flatlength(Movies),
      Label = "Search value: " ++ Query#query.searchVal ++ " | Value category: " ++ Query#query.searchCategory
      ++ " | "++integer_to_list(NumberOfResults)++" Results",
      Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, Label}]),
      Grid = create_grid(Panel, Movies, NumberOfResults, Query),
      %% Add to sizers:
      Options = [{flag, ?wxEXPAND}, {proportion, 1}],
      wxSizer:add(Sizer, Grid, Options),
      wxSizer:add(MainSizer, Sizer, Options),
      wxPanel:setSizer(Panel, MainSizer),
      wxFrame:show(Frame),
      wxWindow:show(Window2);
  %% ************************************************************
    _ ->
      Window2 = wxWindow:new(),
      Frame2 = wxFrame:new(Window2, ?wxID_ANY, "Popup"),
      wxStaticText:new(Frame2, ?wxID_ANY, "Reply"),
      wxFrame:show(Frame2),
      wxWindow:show(Window2)
  end.

create_grid(Panel, [Datum2 = #movie_data{} | T],NumberOfResults, Query = #query{}) ->
  Grid = wxGrid:new(Panel, 2, []),
  ResultsCategories = Query#query.resultCategory,
  NumberOfCategories = countTrueCategories(ResultsCategories, 3, 2),
  wxGrid:createGrid(Grid, NumberOfResults, NumberOfCategories - 1),
  wxGrid:setColLabelValue(Grid, 0, "ID"),
  wxGrid:setColLabelValue(Grid, 1, "Title"),
  Counter = counters:new(1, []),
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
  Grid.

%% readfile - read file as strings separated by lin9wes
readfile(FileName) -> %%TODO need to handle errors - when file:read_file returns {error,Reason}
%%  try
  {ok, Binary} = file:read_file(FileName),
  string:tokens(erlang:binary_to_list(Binary), "\r\n").
%%catch
%%error: Error -> {os:system_time(), error, Error}
%%end.

countTrueCategories(ResultsCategories = #movie_data{}, 22, Count) ->
  if element(22, ResultsCategories) =:= true -> Count + 1;
    true -> Count
  end;

countTrueCategories(ResultsCategories = #movie_data{}, StartIndex, Count) ->
  if element(StartIndex, ResultsCategories) =:= true ->
    countTrueCategories(ResultsCategories, StartIndex + 1, Count + 1);
    true -> countTrueCategories(ResultsCategories, StartIndex + 1, Count)
  end.







