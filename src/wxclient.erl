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

  Choices = ["Title", "Year", "Genre", "Duration", "Country", "Language", "Director", "Writer", "Production Company", "Actor", "Description", "Score", "Budget"],
  SortingChoices = ["ID", "Title", "Year", "Genre", "Duration", "Country", "Language", "Director", "Writer", "Production Company", "Actor", "Description", "Score", "Budget"],

  %%--------------------- Creating Components -------------------------------------------------------------------
  WX = wx:new(),
  Frame = wxFrame:new(WX, ?wxID_ANY, "IMDb Map-Reduce Project"),
  Window = wxWindow:new(Frame, ?wxID_ANY),

  MainSizer = wxStaticBoxSizer:new(?wxVERTICAL, Frame),
%%  LogoSizer = wxBoxSizer:new(?wxVERTICAL,Frame),
%%  QuerySizer = wxBoxSizer:new(?wxVERTICAL,Frame),
  ButtonSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Frame),
  %%SortSizer = wxStaticBoxSizer:new(?wxVERTICAL,Frame,[{label, "Select Sorting Category:"}]),
  %%CheckSizer = wxStaticBoxSizer:new(?wxVERTICAL, Frame, [{label, "Select Categories (ID, Title are always selected):"}]),
  %%CheckSizer = wxBoxSizer:new(?wxVERTICAL),
  Headline = wxStaticText:new(Frame, ?wxID_ANY, "Insert Value (case-sensitive):"),
  Headline2 = wxStaticText:new(Frame, ?wxID_ANY, "Select Categories (ID, Title are always selected):"),
  Headline3 = wxStaticText:new(Frame, ?wxID_ANY, "Select Sorting Category:"),
  TextCtrl = wxTextCtrl:new(Frame, ?wxID_ANY, [{value, ""}, {style, ?wxDEFAULT}]),
  TextCtrlValidation = wxStaticText:new(Frame, ?wxID_ANY, ""),
  ButtonSend = wxButton:new(Frame, ?wxID_ANY, [{label, "Send Query"}, {style, ?wxBU_EXACTFIT}]),
  ListBox = wxListBox:new(Frame, ?wxID_ANY, [{size, {-1, 100}}, {choices, Choices}, {style, ?wxLB_SINGLE}]),
  %%CheckPanel = wxPanel:new(Window,[]),
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
  ListBoxSort = wxListBox:new(Frame, ?wxID_ANY, [{size, {-1, 100}}, {choices, SortingChoices}, {style, ?wxLB_SINGLE}]),
  %%Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL, [{underlined, true}]),

  Image = wxImage:new("logo.png", []),
  Bitmap = wxBitmap:new(wxImage:scale(Image, round(wxImage:getWidth(Image) * 0.345), round(wxImage:getHeight(Image) * 0.345), [{quality, ?wxIMAGE_QUALITY_HIGH}])),
  StaticBitmap = wxStaticBitmap:new(Frame, ?wxID_ANY, Bitmap),

  Image2 = wxImage:new("query.png", []),
  Bitmap2 = wxBitmap:new(wxImage:scale(Image2, round(wxImage:getWidth(Image2) * 0.25), round(wxImage:getHeight(Image2) * 0.25), [{quality, ?wxIMAGE_QUALITY_HIGH}])),
  StaticBitmap2 = wxStaticBitmap:new(Frame, ?wxID_ANY, Bitmap2),


  %%--------------------- Setting Components Properties -------------------------------------------------------------------
  wxTextCtrl:setToolTip(TextCtrl, "Enter your search value here (case-sensitive)"),
  wxButton:setToolTip(ButtonSend, "Send your query to the disco"),
  wxListBox:setToolTip(ListBox, "Choose your search value category"),
  wxListBox:setToolTip(ListBoxSort, "Sort by"),
  wxWindow:setLabel(Window, "IMDb Map-Reduce Project"),
  wxWindow:setBackgroundColour(Frame, {40, 40, 40}),
  wxWindow:setSize(Frame, 0, 0, 350, 765),
  wxStaticText:setForegroundColour(Headline, {255, 200, 0}),
  wxStaticText:setForegroundColour(Headline2, {255, 200, 0}),
  wxStaticText:setForegroundColour(Headline3, {255, 200, 0}),
  wxStaticText:setForegroundColour(TextCtrlValidation, ?wxRED),

  %%--------------------- Sizers hierarchy and Events setup ----------------------------------------------------------------------
  wxSizer:add(MainSizer, StaticBitmap, [{flag, ?wxALL bor ?wxEXPAND}]),
  wxSizer:add(MainSizer, StaticBitmap2, [{flag, ?wxALL bor ?wxEXPAND}]),
  wxSizer:add(MainSizer, Headline, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(MainSizer, TextCtrl, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(MainSizer, ListBox, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(MainSizer, Headline2, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),

  Fun = fun(Item) -> wxSizer:add(MainSizer, Item, [{flag, ?wxLEFT bor ?wxRIGHT bor ?wxEXPAND}, {border, 8}]),
    wxCheckBox:setForegroundColour(Item, {255, 200, 0}) end, %%Adding checkboxes to the sizer
  wx:foreach(Fun, CheckBoxes),

%%  wxSizer:add(MainSizer, LogoSizer, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
%%  wxSizer:add(MainSizer, QuerySizer, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
%%  wxSizer:add(MainSizer, ButtonSizer, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),

  wxSizer:add(MainSizer, Headline3, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(MainSizer, ListBoxSort, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(ButtonSizer, ButtonSend, [{flag, ?wxALL}, {border, 8}]),
  wxSizer:add(ButtonSizer, TextCtrlValidation, [{flag, ?wxALL}, {border, 8}]),
%%  wxSizer:add(MainSizer, CheckSizer, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
%%  wxSizer:add(MainSizer, SortSizer, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(MainSizer, ButtonSizer, [{flag, ?wxTOP bor ?wxLEFT bor ?wxRIGHT bor ?wxEXPAND}, {border, 8}]),

%%  %%Event configuration for button click:
  wxEvtHandler:connect(ButtonSend, command_button_clicked, [{callback, fun handle_click_event/2},
    {userData, {wx:get_env(), TextCtrl, ListBox, CheckBoxes, TextCtrlValidation, ListBoxSort}}]),

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
      wxTextCtrl:setLabel(TextCtrlValidation, "The query is valid"),
      wxStaticText:setForegroundColour(TextCtrlValidation, {255, 200, 0}),
      %%---------------------------------- Sending the query to the master: ------------------------------------------------
      Query = #query{type = generic,
        searchVal = wxTextCtrl:getValue(TextBox),
        searchCategory = wxListBox:getString(ListBox, wxListBox:getSelection(ListBox)), resultCategory = Categories2Show},
      [MasterNode | _T] = readfile(["clientslist.txt"]),
      StartTime = os:timestamp(),%% for performance evaluation
      try
        gen_server:call({masterpid, list_to_atom(MasterNode)}, Query), %% we receive acknowledge and the result will arrive from another process
        receive
        %% ************* Handling Query Results Here: *******************
          [] ->
            WindowZero = wxWindow:new(),
            FrameZero = wxFrame:new(WindowZero, ?wxID_ANY, "No Results"),
            MainSizerZero = wxBoxSizer:new(?wxVERTICAL),
            TextZero = wxStaticText:new(FrameZero, ?wxID_ANY, "There are 0 results for the requested qeury"),
            wxSizer:add(MainSizerZero, TextZero, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
            wxWindow:setSizer(FrameZero, MainSizerZero),
            wxFrame:show(FrameZero),
            wxWindow:show(WindowZero);

          MoviesRaw when is_list(MoviesRaw) ->
            Movies = lists:sort(fun(M1 = #movie_data{}, M2 = #movie_data{}) ->
              (getValueForSorting(M1, wxListBox:getSelection(ListBoxSort)) < getValueForSorting(M2, wxListBox:getSelection(ListBoxSort))) end, MoviesRaw),
            TotalTime = round(timer:now_diff(os:timestamp(), StartTime) / 1000),%% for performance evaluation
            %% Setup sizers and frames:--------------------------------------------------------------
            Window2 = wxWindow:new(),
            Frame2 = wxFrame:new(Window2, ?wxID_ANY, "IMDb Map-Reduce Project"),
            MainSizer = wxBoxSizer:new(?wxVERTICAL),
            NumberOfResults = lists:flatlength(Movies),
            Label = "Search value: " ++ Query#query.searchVal ++ " | Value category: " ++ Query#query.searchCategory
              ++ " | " ++ integer_to_list(NumberOfResults) ++ " Results | Evaluation Time: " ++ integer_to_list(TotalTime) ++ "ms",
            Headline = wxStaticText:new(Frame2, ?wxID_ANY, Label),
            Grid = create_grid(Frame2, Movies, NumberOfResults, Query), %% Creating the results table:
            Image3 = wxImage:new("results.png", []),
            Bitmap3 = wxBitmap:new(wxImage:scale(Image3, round(wxImage:getWidth(Image3) * 0.345), round(wxImage:getHeight(Image3) * 0.345), [{quality, ?wxIMAGE_QUALITY_HIGH}])),
            StaticBitmap3 = wxStaticBitmap:new(Frame2, ?wxID_ANY, Bitmap3),
            %% Sizers properties and hierarchy -----------------------------------------------------
            wxWindow:setSize(Frame2, 0, 0, 500, 500),
            wxWindow:setBackgroundColour(Frame2, {40, 40, 40}),
            wxStaticText:setForegroundColour(Headline, {255, 200, 0}),
            wxFrame:center(Frame2),
            wxSizer:add(MainSizer, StaticBitmap3, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
            wxSizer:add(MainSizer, Headline, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
            Options = [{flag, ?wxEXPAND}, {proportion, 1}],
            wxSizer:add(MainSizer, Grid, Options),
            wxWindow:setSizer(Frame2, MainSizer),
            wxFrame:show(Frame2);
        %% ************************************************************
          _ -> %% We didn't receive a list - error
            Window2 = wxWindow:new(),
            Frame2 = wxFrame:new(Window2, ?wxID_ANY, "Error"),
            wxStaticText:new(Frame2, ?wxID_ANY, "An error occured."),
            wxFrame:show(Frame2),
            wxWindow:show(Window2)
        end
      catch
        _:_-> wxTextCtrl:setForegroundColour(TextCtrlValidation, ?wxRED),
          wxTextCtrl:setLabel(TextCtrlValidation, "The master is down")
      end;
        false -> wxTextCtrl:setForegroundColour(TextCtrlValidation, ?wxRED),
          wxTextCtrl:setLabel(TextCtrlValidation, "The input is invalid")
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





