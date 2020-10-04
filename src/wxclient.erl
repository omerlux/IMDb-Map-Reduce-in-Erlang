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
-record(numOfResults, {number}).
-record(reduced_data, {id, title, categoryInfo}).
-record(state,
{
  parent,
  config
}).
-record(movie_data, {id, title, original_title, year,
  date_published, genre, duration, country, language, director,
  writer, production_company, actors, description, avg_vote,
  votes, budget, usa_gross_income, worlwide_gross_income,
  metascore, reviews_from_users, reviews_from_critics}).
-record(query, {type, searchVal, searchCategory, resultCategory}).

%% note======================================= TODO: =============================================
%% 1. Mention that the search is case sensitive
%% 2. Add the number of result for every query, not related to the "Nubmer of results" option
%% 3. Showing the data on the Grid?
%% note===========================================================================================


%% Will get the pid of server
%% will send the query on button pressing
start() ->

  %%note: Frame and components build -----------------------------------------------------------------------------------
  WX = wx:new(),
  Frame = wxFrame:new(WX, 1, "Disco Map-Reduce Project"), %%TODO whats the difference from Panel?
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  SubSizer1 = wxBoxSizer:new(?wxVERTICAL),
  SubSizer2 = wxBoxSizer:new(?wxVERTICAL),
  TopTxt = wxStaticText:new(Frame, ?wxID_ANY, "Query Window"),
  Headline = wxStaticText:new(Frame, ?wxID_ANY, "Insert Value:"),
  TextCtrl = wxTextCtrl:new(Frame, 1, [{value, ""}, {style, ?wxDEFAULT}]),
  wxTextCtrl:setToolTip(TextCtrl, "Enter your search value here"),
  ButtonSend = wxButton:new(Frame, ?wxID_ANY, [{label, "Send Query"}, {style, ?wxBU_EXACTFIT}]),
  wxButton:setToolTip(ButtonSend, "Send your query to the disco"),
  Choices = ["Title", "Year", "Genre", "Duration", "Country", "Language", "Director", "Writer", "Production Company", "Actor", "Description", "Score", "Budget"],
  Choices2 = ["Title", "Year", "Genre", "Duration", "Country", "Language", "Director", "Writer", "Production Company", "Actor", "Description", "Score", "Budget", "Number of results", "All"],
  %% Create a wxListBox that uses multiple selection
  ListBox = wxListBox:new(Frame, 1, [{size, {-1, 100}},
    {choices, Choices}, {style, ?wxLB_SINGLE}]),
  wxListBox:setToolTip(ListBox, "Choose your search value category"),
  ComboBox = wxComboBox:new(Frame, 5, [{choices, Choices2}]),
  wxComboBox:setToolTip(ComboBox, "Choose the category your interested in from the results"),
  wxEvtHandler:connect(ButtonSend, command_button_clicked, [{callback, fun handle_click_event/2},
    {userData, {wx:get_env(), TextCtrl, ListBox, ComboBox}}]),

  %%note: Sizers setup -------------------------------------------------------------------------------------------------
  wxSizer:add(SubSizer1, TopTxt, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, Headline, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, TextCtrl, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ListBox, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ComboBox, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, ButtonSend, [{flag, ?wxALL}, {border, 8}]),
%%  wxSizer:add(GridSizer, Grid, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),%%TODO hide it at startup?
  Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL, [{underlined, true}]),
  wxTextCtrl:setFont(TopTxt, Font),
  wxSizer:add(MainSizer, SubSizer1, [{flag, ?wxALIGN_LEFT}, {border, 5}]),
  wxSizer:add(MainSizer, SubSizer2, [{flag, ?wxALIGN_RIGHT}, {border, 5}]),
  wxWindow:setSizer(Frame, MainSizer),
  wxFrame:show(Frame).

handle_click_event(A = #wx{}, _B) ->
  {Env, TextBox, ListBox, ComboBox} = A#wx.userData,
  wx:set_env(Env),
  Query = #query{type = generic,
    searchVal = wxTextCtrl:getValue(TextBox),
    searchCategory = wxListBox:getString(ListBox, wxListBox:getSelection(ListBox)),
    resultCategory = wxComboBox:getValue(ComboBox)},
  %%[MasterNode | _T] = readfile(["clientslist.txt"]), %%TODO had a problem reading the file...
  MasterNode = "master@DESKTOP-3NPJUSA",
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
      Label = "Search value: " ++ Query#query.searchVal ++ " | Value category: " ++ Query#query.searchCategory
        ++ " | Information required: " ++ Query#query.resultCategory,
      Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, Label}]),
      Grid = create_grid(Panel, Movies),
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

create_grid(Panel, [Datum = #reduced_data{} | T]) ->
  Grid = wxGrid:new(Panel, 2, []),
  NumberOfResults = lists:flatlength(T) + 1,
  wxGrid:createGrid(Grid, NumberOfResults, 3),
  wxGrid:setColLabelValue(Grid, 0, "Id"),
  wxGrid:setColLabelValue(Grid, 1, "Title"),
  wxGrid:setColLabelValue(Grid, 2, "Required Value"),
  Func =
    fun({RowNumber, Datum = #reduced_data{}}) ->
      wxGrid:setCellValue(Grid, RowNumber, 0, Datum#reduced_data.id),
      wxGrid:setCellValue(Grid, RowNumber, 1, Datum#reduced_data.title),
      wxGrid:setCellValue(Grid, RowNumber, 2, Datum#reduced_data.categoryInfo)
    end,
  NumberingList = lists:seq(0, NumberOfResults - 1),
  RowsList = lists:zip(NumberingList, [Datum | T]),
  wx:foreach(Func, RowsList),
  Grid;

create_grid(Panel, Datum = #numOfResults{}) ->
  Grid = wxGrid:new(Panel, 2, []),
  wxGrid:createGrid(Grid, 1, 1),
  wxGrid:setCellValue(Grid, 0, 0, "The number of result is: " ++ Datum#numOfResults.number),
  Grid;

create_grid(Panel, [Datum = #movie_data{} | T]) ->
  Grid = wxGrid:new(Panel, 2, []),
  NumberOfResults = lists:flatlength(T) + 1,
  wxGrid:createGrid(Grid, NumberOfResults, 14),
  wxGrid:setColLabelValue(Grid, 0, "Id"),
  wxGrid:setColLabelValue(Grid, 1, "Title"),
  wxGrid:setColLabelValue(Grid, 2, "Description"),
  wxGrid:setColLabelValue(Grid, 3, "Duration (minutes)"),
  wxGrid:setColLabelValue(Grid, 4, "Genres"),
  wxGrid:setColLabelValue(Grid, 5, "Country"),
  wxGrid:setColLabelValue(Grid, 6, "Language"),
  wxGrid:setColLabelValue(Grid, 7, "Director"),
  wxGrid:setColLabelValue(Grid, 8, "Writer"),
  wxGrid:setColLabelValue(Grid, 9, "Actors"),
  wxGrid:setColLabelValue(Grid, 10, "Production"),
  wxGrid:setColLabelValue(Grid, 11, "Score"),
  wxGrid:setColLabelValue(Grid, 12, "Budget"),
  wxGrid:setColLabelValue(Grid, 13, "Year"),
  Func =
    fun({RowNumber, Datum = #movie_data{}}) ->
      wxGrid:setCellValue(Grid, RowNumber, 0, Datum#movie_data.id),
      wxGrid:setCellValue(Grid, RowNumber, 1, Datum#movie_data.title),
      wxGrid:setCellValue(Grid, RowNumber, 2, Datum#movie_data.description),
      wxGrid:setCellValue(Grid, RowNumber, 3, Datum#movie_data.duration),
      wxGrid:setCellValue(Grid, RowNumber,4, Datum#movie_data.genre),
      wxGrid:setCellValue(Grid, RowNumber, 5, Datum#movie_data.country),
      wxGrid:setCellValue(Grid, RowNumber, 6, Datum#movie_data.language),
      wxGrid:setCellValue(Grid, RowNumber, 7, Datum#movie_data.director),
      wxGrid:setCellValue(Grid, RowNumber, 8, Datum#movie_data.writer),
      wxGrid:setCellValue(Grid, RowNumber, 9, Datum#movie_data.actors),
      wxGrid:setCellValue(Grid, RowNumber, 10, Datum#movie_data.production_company),
      wxGrid:setCellValue(Grid, RowNumber, 11, Datum#movie_data.metascore),
      wxGrid:setCellValue(Grid, RowNumber, 12, Datum#movie_data.budget),
      wxGrid:setCellValue(Grid, RowNumber, 13, Datum#movie_data.year)
    end,
  NumberingList = lists:seq(0, NumberOfResults - 1),
  RowsList = lists:zip(NumberingList, [Datum | T]),
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








