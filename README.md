# Project-Ilay-Omer - IMDb Map-Reduce Project in Erlang
 This is the final project of "Functional programming in concurrent and distributed systems", by Ilay and Omer. Demonstration on [YouTube](https://youtu.be/u_CJGBQcP_M).
 
- ### General Info:
 
 The main purpose of the project is to implement a map-reduce algorithm for iMDB. 
 
 The data is taken from [Kaggle](https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset) and contains ~85k records of movies.
 In addition, we created a distributed machine which distributes the data from *master* node to *servers* node. Number of servers can be chosen by the user.
 
 *clients* can be created in any computer/terminal and ask for different kinds of queries from the *master* node. The answer will be delivered from each *server* (which contains different records), to the *master* and then the master will group them into one result which will be sent back to the client.

- ### Technical Overview:
We built the system as the Disco-Project system supposed to be:
<p style="height: 10px; overflow: hidden" align="center">
  <img src="https://github.com/ilaynuriel/Project-Ilay-Omer/blob/master/src/overview.png?raw=true">
</p>

##### Our overview of the system (Same as [Disco-Project](https://disco.readthedocs.io/en/latest/overview.html))

- ### How to Run:

First in every computer, run compilation in erlang shell (worked with Erlang 22(OTP22, erts- 10.7))
```
erl
c(master). c(server). c(wxclient). c(parse_csv). c(dataDistributor). 
```

 To run ***server***, write the server's name in *@serverslist.txt* - *server#@ip*
 then at each server write the following line on terminal:
```
erl -name server#@ip -setcookie x -run server start_link
```

 To run ***master***, write the master's name in *@clientslist.txt* (on the first line) - *master@ip*
 then at master terminal write the following line on terminal:
```
erl -name master@ip -setcookie x -run master start_link
```

 To run ***client*** (which can be on the same computer as the servers)
 then at client terminal write the following line on terminal:
```
erl -name client#@ip -setcookie x -run wxclient start
```
If GUI won't open, do as follow:
```
erl -name client#@ip -setcookie x
wxclient:start().
```


 **(-name can be -sname for local connection run)**

- ### Progress:
- [x] Build infrastructure
- [x] Handle exceptions
- [X] Finish client's gui
- [X] Add statistics to queries
